## Self-training

# Iteration: 1
# Algorithm: GBDT
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
covid <- read_xlsx("D:/Data/Training samples/misinformation_labeled.xlsx")

stopwords <- read_xlsx("~/INORK_r/Processing/stopwords.xlsx")
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
  count(label) # misinfo = 65, non misinfo = 798

################################################################################
863/(table(train$label)[1] * 2) # 0.5407268  
863/(table(train$label)[2] * 2) # 6.638462  

train <- train |>
  mutate(case_wts = ifelse(label == "misinfo", 6.638462, 0.5407268),
         case_wts = importance_weights(case_wts))

################################################################################
## GBDT
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

bt_spec <- boost_tree(trees = 500, 
                      min_n = tune(), 
                      tree_depth = tune(), 
                      loss_reduction = tune(), 
                      sample_size = tune(), 
                      mtry = tune()) |>
  set_mode("classification") |>
  set_engine("xgboost") 

bt_workf <- workflow() |>
  add_model(bt_spec) |>
  add_recipe(model_recipe) |>
  add_case_weights(case_wts)

set.seed(89)
grid <- grid_latin_hypercube(tree_depth(), 
                             min_n(), 
                             loss_reduction(), 
                             sample_size = sample_prop(), 
                             finalize(mtry(), train), 
                             size = 20)

set.seed(1)
bt_res <- bt_workf |>
  tune_grid(resamples = folds, grid = grid, control = control_grid(save_pred = TRUE), metrics = cls_metric)

bt_params <- select_best(bt_res, metric = "f_meas")

set.seed(456)
bt_final_workf <- bt_workf |>
  finalize_workflow(bt_params)

set.seed(2)
bt_final_fit <- fit(bt_final_workf, train)

bt_preds <- test |>
  bind_cols(predict(bt_final_fit, test))

cm_bt <- confusionMatrix(table(test$label, bt_preds$.pred_class)) 
cm_bt$byClass["F1"] # 0.08333333     
cm_bt$byClass["Precision"] # 0.07142857 
cm_bt$byClass["Recall"] # 0.1 

bt_preds |>
  conf_mat(truth = label, estimate = .pred_class) |>
  autoplot(type = "heatmap")  

################################################################################
covid_df <- readRDS("E:/Data/covid_processed.RDS")
match <- subset(covid, (covid$id %in% covid_df$id))
covid_df <- covid_df |>
  #  anti_join(match, by = "id") |>
  rename(tweet = text)

set.seed(89)
bt_preds_all <- covid_df |>
  bind_cols(predict(bt_final_fit, covid_df, type = "prob"))

bt_preds_all_filtered <- bt_preds_all |>
  filter(.pred_misinfo > 0.98 | .pred_non.misinfo > 0.98)

bt_preds_all_filtered_label <- bt_preds_all_filtered |>
  mutate(label = case_when(
    .pred_misinfo > 0.98 ~ "misinfo",
    .pred_non.misinfo > 0.98 ~ "non.misinfo"
  ))

bt_preds_all_filtered_label <- bt_preds_all_filtered_label |>
  select(tweet, label, id)

bt_preds_all_filtered_label |>
  count(label) # misinfo = 3469, nonmisinfo = 57 046

covid_predicted <- full_join(bt_preds_all_filtered_label, covid, by = "id") |>
  mutate(label = coalesce(label.x, label.y),
         tweet = coalesce(tweet.x, tweet.y)) |>
  select(tweet, label, id)

covid_predicted |>
  count(label) # misinfo = 3540, nonmisinfo = 57 903

saveRDS(covid_predicted, "E:/Data/Training samples/misinformation_class_1.RDS")
