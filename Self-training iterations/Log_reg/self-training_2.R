## Self-training

# Iteration: 2
# Algorithm: Logistic regression
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
covid <- readRDS("E:/Data/Training samples/misinformation_class_1.RDS")

stopwords <- read_xlsx("~/INORK/Processing/stopwords.xlsx")
custom_words <- stopwords |>
  pull(word)

covid$label <- as.factor(covid$label)

set.seed(1234)
covid_split <- initial_split(covid, prop = 0.8, strata = label)
train <- training(covid_split)
test <- testing(covid_split)

train |>
  count(label) # misinfo = 2848, non misinfo = 46306

################################################################################
49154/(table(train$label)[1] * 2) # 8.629565  
49154/(table(train$label)[2] * 2) # 0.530752  

train <- train |>
  mutate(case_wts = ifelse(label == "misinfo", 8.629565, 0.530752),
         case_wts = importance_weights(case_wts))

################################################################################
## Logistic regression
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

lr_spec <- logistic_reg(penalty = tune(), mixture = 1) |>
  set_mode("classification") |>
  set_engine("glmnet")

lr_workf <- workflow() |>
  add_model(lr_spec) |>
  add_recipe(model_recipe) |>
  add_case_weights(case_wts)

set.seed(13)
grid <- tibble(penalty = 10^seq(-3, 0, length.out = 20))

set.seed(1)
lr_res <- lr_workf |>
  tune_grid(resamples = folds, grid = grid, metrics = cls_metric)

lr_params <- select_best(lr_res, metric = "f_meas")

set.seed(345)
lr_final_workf <- lr_workf |>
  finalize_workflow(lr_params)

set.seed(2)
lr_final_fit <- fit(lr_final_workf, train)

lr_preds <- test |>
  bind_cols(predict(lr_final_fit, test))

cm_lr <- confusionMatrix(table(test$label, lr_preds$.pred_class), positive = "misinfo") 
cm_lr$byClass["F1"] # 0.9689704     
cm_lr$byClass["Precision"] # 0.9927746   
cm_lr$byClass["Recall"] # 0.946281  

lr_preds |>
  conf_mat(truth = label, estimate = .pred_class) |> 
  autoplot(type = "heatmap") 

# saveRDS(lr_final_fit, "~/INORK_ST/log_reg_round_1.RDS")
################################################################################
covid_df <- readRDS("E:/Data/covid_processed.RDS")
match <- subset(covid, (covid$id %in% covid_df$id))
covid_df <- covid_df |>
  #  anti_join(match, by = "id") |>
  rename(tweet = text)

set.seed(89)
lr_preds_all <- covid_df |>
  bind_cols(predict(lr_final_fit, covid_df, type = "prob"))

lr_preds_all_filtered <- lr_preds_all |>
  filter(.pred_misinfo > 0.98 | .pred_non.misinfo > 0.98)

lr_preds_all_filtered_label <- lr_preds_all_filtered |>
  mutate(label = case_when(
    .pred_misinfo > 0.98 ~ "misinfo",
    .pred_non.misinfo > 0.98 ~ "non.misinfo"
  ))

lr_preds_all_filtered_label <- lr_preds_all_filtered_label |>
  select(tweet, label, id)

lr_preds_all_filtered_label |>
  count(label) # misinfo = 6361, nonmisinfo = 64 408

covid_predicted <- full_join(lr_preds_all_filtered_label, covid, by = "id") |>
  mutate(label = coalesce(label.x, label.y),
         tweet = coalesce(tweet.x, tweet.y)) |>
  select(tweet, label, id)

covid_predicted |>
  count(label) # misinfo = 7035, nonmisinfo = 78 032

saveRDS(covid_predicted, "E:/Data/Training samples/misinformation_class_2.RDS")
