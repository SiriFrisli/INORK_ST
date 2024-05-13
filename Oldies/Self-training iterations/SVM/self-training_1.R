## Self-training

# Iteration: 1
# Algorithm: SVM
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
## SVM
covid_recipe <- recipe(label~tweet, data = train) |>
  step_tokenize(tweet, engine = "spacyr") |>
  step_stopwords(tweet, language = "no", keep = FALSE, 
                 stopword_source = "snowball", 
                 custom_stopword_source = custom_words) |>
  step_lemma(tweet) |>
  step_word_embeddings(tweet, embeddings = no_we) |>
  step_normalize(all_numeric_predictors()) |>
  prep()

new_train <- bake(covid_recipe, new_data = train)
new_test <- bake(covid_recipe, new_data = test)

set.seed(1234)
svm_model <- svm(formula = label~.,
                 data = new_train,
                 type = "C-classification",
                 kernel = "radial",
                 cross = 5,
                 class.weights = c("misinfo" = 6.469231, "non.misinfo" = 0.541184),
                 probability = TRUE)

model_svm <- new_test |>
  bind_cols(predict(svm_model, new_test))

cm_svm <- confusionMatrix(table(new_test$label, model_svm$...102), positive = "misinfo") 
cm_svm$byClass["F1"] # 0.07692308       
cm_svm$byClass["Precision"] # 0.125    
cm_svm$byClass["Recall"] # 0.05555556 

new_test |>
  bind_cols(predict(svm_model, new_test)) |>
  conf_mat(truth = label, estimate = ...102) |>
  autoplot(type = "heatmap")

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
  count(label) # misinfo = 3469, nonmisinfo = 57 046

covid_predicted <- full_join(lr_preds_all_filtered_label, covid, by = "id") |>
  mutate(label = coalesce(label.x, label.y),
         tweet = coalesce(tweet.x, tweet.y)) |>
  select(tweet, label, id)

covid_predicted |>
  count(label) # misinfo = 3540, nonmisinfo = 57 903

saveRDS(covid_predicted, "E:/Data/Training samples/misinformation_class_1.RDS")
