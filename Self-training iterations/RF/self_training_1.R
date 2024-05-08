## Self-training

# Iteration: 1
# Algorithm: Random forest
# Word embeddings dimensions: 100
# Class weights

packages <- c("tidyverse", "tidymodels", "readxl", "ROSE", "recipes", "textrecipes",
              "e1071", "caret", "glmnet", "spacyr", "textdata", "xgboost", "data.table")

for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package,
                     dependencies = TRUE,
                     repos='http://cran.us.r-project.org')
  }
}

for (package in packages){
  library(package, character.only = TRUE)
}

spacy_initialize(model = "nb_core_news_sm")
no_we <- fread("~/SVM_ST/model_1.txt", 
               skip = 1, header = FALSE, sep = " ", quote = "", encoding = "UTF-8")

no_we <- no_we |>
  as_tibble()

################################################################################
covid <- readRDS("E:/Data/Training samples/misinformation_labeled.RDS")

stopwords <- read_xlsx("~/INORK/Processing/stopwords.xlsx")
custom_words <- stopwords |>
  pull(word)

covid <- covid |>
  select(tweet = text, label, id)

covid$label <- case_when(
  covid$label == 0 ~ "non.misinfo",
  covid$label == 1 ~ "misinfo"
)
covid$label <- as.factor(covid$label)

set.seed(1234)
covid_split <- initial_split(covid, prop = 0.8, strata = label)
train <- training(covid_split)
test <- testing(covid_split)

train |>
  count(label) # misinfo = 65, non misinfo = 776

################################################################################
841/(table(train$label)[1] * 2) # 0.5418814 
841/(table(train$label)[2] * 2) # 6.469231 

train <- train |>
  mutate(case_wts = ifelse(label == "misinfo", 6.469231, 0.5418814),
         case_wts = importance_weights(case_wts))

################################################################################
## Random Forest
set.seed(8008)
folds <- vfold_cv(train, strata = label, v = 5, repeats = 2)
cls_metric <- metric_set(yardstick::precision, yardstick::recall, yardstick::f_meas)

model_recipe <- recipe(label~tweet+case_wts, data = train) |>
  step_tokenize(tweet, engine = "spacyr") |>
  step_stopwords(tweet, language = "no", keep = FALSE, 
                 stopword_source = "snowball", 
                 custom_stopword_source = custom_words) |>
  step_lemma(tweet) |>
  step_word_embeddings(tweet, embeddings = no_we) |>
  step_normalize(all_numeric_predictors())

rf_spec <- rand_forest(mtry = tune(), trees = 500, min_n = tune()) |>
  set_mode("classification") |>
  set_engine("ranger")

rf_workf <- workflow() |>
  add_model(rf_spec) |>
  add_recipe(model_recipe) 

set.seed(89)
grid <- grid_latin_hypercube(min_n(), 
                             finalize(mtry(), train), 
                             size = 20)

set.seed(1)
rf_res <- rf_workf |>
  tune_grid(resamples = folds, grid = grid, control = control_grid(save_pred = TRUE), metrics = cls_metric)

rf_params <- select_best(rf_res, metric = "f_meas")

set.seed(456)
rf_final_workf <- rf_workf |>
  finalize_workflow(rf_params)

set.seed(2)
rf_final_fit <- fit(rf_final_workf, train)

rf_preds <- test |>
  bind_cols(predict(rf_final_fit, test))

cm_rf <- confusionMatrix(table(test$label, rf_preds$.pred_class)) 
cm_rf$byClass["F1"] # NAs    
cm_rf$byClass["Precision"] # 
cm_rf$byClass["Recall"] #  

rf_preds |>
  conf_mat(truth = label, estimate = .pred_class) |>
  autoplot(type = "heatmap") 

################################################################################
covid_df <- readRDS("E:/Data/covid_processed.RDS")
match <- subset(covid, (covid$id %in% covid_df$id))
covid_df <- covid_df |>
  #  anti_join(match, by = "id") |>
  rename(tweet = text)

set.seed(89)
rf_preds_all <- covid_df |>
  bind_cols(predict(rf_final_fit, covid_df, type = "prob"))

rf_preds_all_filtered <- rf_preds_all |>
  filter(.pred_misinfo > 0.98 | .pred_non.misinfo > 0.98)

rf_preds_all_filtered_label <- rf_preds_all_filtered |>
  mutate(label = case_when(
    .pred_misinfo > 0.98 ~ "misinfo",
    .pred_non.misinfo > 0.98 ~ "non.misinfo"
  ))

rf_preds_all_filtered_label <- rf_preds_all_filtered_label |>
  select(tweet, label, id)

rf_preds_all_filtered_label |>
  count(label) # misinfo = 3469, nonmisinfo = 57 046

covid_predicted <- full_join(rf_preds_all_filtered_label, covid, by = "id") |>
  mutate(label = coalesce(label.x, label.y),
         tweet = coalesce(tweet.x, tweet.y)) |>
  select(tweet, label, id)

covid_predicted |>
  count(label) # misinfo = 3540, nonmisinfo = 57 903

saveRDS(covid_predicted, "E:/Data/Training samples/misinformation_class_1.RDS")

