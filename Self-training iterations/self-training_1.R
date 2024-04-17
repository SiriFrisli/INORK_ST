## Self-training

# Iteration: 1
# Algorithm: SVM
# Word embeddings dimensions: 100

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
covid <- readRDS("D:/Data/Training samples/misinformation_labeled.RDS")
train <- readRDS("D:/Data/manual_links_TRAIN.RDS")
test <- readRDS("D:/Data/manual_links_TEST.RDS")
stopwords <- read_xlsx("~/INORK_R/Processing/stopwords.xlsx")
custom_words <- stopwords |>
  pull(word)

train |>
  count(label) # misinfo = 775, non misinfo = 776

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
                 probability = TRUE)

model_svm <- new_test |>
  bind_cols(predict(svm_model, new_test))

cm_svm <- confusionMatrix(table(new_test$label, model_svm$...102))
cm_svm$byClass["F1"] # 0.1621622   
cm_svm$byClass["Precision"] # 0.375 
cm_svm$byClass["Recall"] # 0.1034483 

################################################################################
covid_df <- readRDS("D:/Data/covid_processed.RDS")

match <- subset(covid, (covid$id %in% covid_df$id))
covid_df <- covid_df |>
  anti_join(match, by = "id") |>
  rename(tweet = text)

set.seed(89)
svm_preds_all <- covid_df |>
  bind_cols(predict(svm_model, covid_df, type = "prob"))

svm_preds_all_all_filtered <- svm_preds_all |>
  filter(.pred_misinfo > 0.95 | .pred_non.misinfo > 0.95)

svm_preds_all_filtered_label <- svm_preds_all_filtered |>
  mutate(label = case_when(
    .pred_misinfo > 0.95 ~ "misinfo",
    .pred_non.misinfo > 0.95 ~ "non.misinfo"
  ))

svm_preds_all_filtered_label <- svm_preds_all_filtered_label |>
  select(tweet, label, id)

svm_preds_all_filtered_label |>
  count(label)

covid_predicted <- rbind(covid, svm_preds_all_filtered_label)

covid_predicted |>
  count(label)
