## Base classifier tests
# Manually labeled sample with links added to training data
# SVM, Logistic Regression, Gradient Boosted Trees
# Word embeddings, 100 dimensions

################################################################################
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

spacy_initialize(model = "nb_core_news_sm") # Spacy environment for lemmas

# Pre-trained word embeddings:
no_we <- fread("~/SVM_ST/model_1.txt", 
               skip = 1, header = FALSE, sep = " ", quote = "", encoding = "UTF-8")
no_we <- no_we |>
  as_tibble()

################################################################################
covid <- readRDS("D:/Data/Training samples/misinformation_labeled.RDS")
# this data has already been preprocessed some

stopwords <- read_xlsx("~/INORK_R/Processing/stopwords.xlsx")
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

# Adding data to the training sample
covid_links <- readRDS("D:/Data/covid_misinfo_links_only.RDS")
covid_links <- covid_links |>
  rename(tweet = text)

covid_links$label <- case_when(
  covid_links$label == 0 ~ "non.misinfo",
  covid_links$label == 1 ~ "misinfo"
)

removeURL <- function(tweet) {
  return(gsub("http\\S+", "", tweet))
}

removeUsernames <- function(tweet) {
  return(gsub("@[a-z,A-Z,_]*[0-9]*[a-z,A-Z,_]*[0-9]*", "", tweet))
}

covid_links$tweet <- apply(covid_links["tweet"], 1, removeURL)
covid_links$tweet <- apply(covid_links["tweet"], 1, removeUsernames)

covid_links$label <- as.factor(covid_links$label) # Outcome variable needs to be factor
covid_links$tweet <- tolower(covid_links$tweet)
covid_links$tweet <- gsub("[[:punct:]]", " ", covid_links$tweet)

covid_links <- covid_links |>
  select(-c(dom_url, conversation_id)) |>
  sample_n(710, seed = 321)

train_full <- rbind(covid_links, train)

train <- train_full

train |>
  count(label) # misinfo = 775, non misinfo = 776

# saveRDS(train, "D:/Data/manual_links_TRAIN.RDS")
# train <- readRDS("D:/Data/manual_links_TRAIN.RDS")

# saveRDS(test, "D:/Data/manual_links_TEST.RDS")
# test <- readRDS("D:/Data/manual_links_TEST.RDS")

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
                 probability = TRUE)
test_svm <- new_test |>
  bind_cols(predict(svm_model, new_test))

cm_svm_test <- confusionMatrix(table(new_test$label, test_svm$...102)) 
cm_svm_test$byClass["F1"] # 0.1621622     
cm_svm_test$byClass["Precision"] # 0.375    
cm_svm_test$byClass["Recall"] # 0.1034483    

set.seed(1234)
svm_tune <- tune.svm(x = label ~., 
                     data = new_train,
                     gamma = 10^(-3:3), 
                     cost = seq(1,10, by = 1),  
                     kernel = "radial")

svm_tune$best.parameters$gamma # 0.001
svm_tune$best.parameters$cost # 10

set.seed(1234)
svm_model <- svm(formula = label~.,
                 data = new_train,
                 type = "C-classification",
                 kernel = "radial",
                 cross = 5,
                 gamma = svm_tune$best.parameters$gamma,
                 cost = svm_tune$best.parameters$cost,
                 probability = TRUE)

model_svm <- new_test |>
  bind_cols(predict(svm_model, new_test))

cm_svm <- confusionMatrix(table(new_test$label, model_svm$...102)) # these are worse than the default model
cm_svm$byClass["F1"] # 0.1395349     
cm_svm$byClass["Precision"] # 0.375    
cm_svm$byClass["Recall"] # 0.08571429 

# USing the default model
new_test |>
  bind_cols(predict(svm_model, new_test)) |>
  conf_mat(truth = label, estimate = ...102) |>
  autoplot(type = "heatmap")

################################################################################
## Logistic regression 

set.seed(8008)
folds <- vfold_cv(train, strata = label, v = 5, repeats = 2)
cls_metric <- metric_set(yardstick::precision, yardstick::recall, yardstick::f_meas)

model_recipe <- recipe(label~tweet, data = train) |>
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
  add_recipe(model_recipe) 

set.seed(564)
grid <- tibble(penalty = 10^seq(-3, 0, length.out = 20))

set.seed(1)
lr_res <- lr_workf |>
  tune_grid(resamples = folds, grid = grid, metrics = cls_metric)

lr_params <- select_best(lr_res, metric = "f_meas")

set.seed(958)
lr_final_workf <- lr_workf |>
  finalize_workflow(lr_params)

set.seed(2)
lr_final_fit <- fit(lr_final_workf, train)

lr_preds <- test |>
  bind_cols(predict(lr_final_fit, test))

cm_lr <- confusionMatrix(table(test$label, lr_preds$.pred_class)) 
cm_lr$byClass["F1"] # 0.1153846      
cm_lr$byClass["Precision"] # 0.375     
cm_lr$byClass["Recall"] # 0.06818182  

test |>
  bind_cols(predict(lr_final_fit, test)) |>
  conf_mat(truth = label, estimate = .pred_class) |> 
  autoplot(type = "heatmap") 

################################################################################
## Gradient Boosted Decision Trees

bt_spec <- boost_tree(trees = 500, min_n = tune(), tree_depth = tune(), loss_reduction = tune(), sample_size = tune(), mtry = tune()) |>
  set_mode("classification") |>
  set_engine("xgboost") 

bt_workf <- workflow() |>
  add_model(bt_spec) |>
  add_recipe(model_recipe)

set.seed(345)
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

set.seed(43)
bt_final_workf <- bt_workf |>
  finalize_workflow(bt_params)

set.seed(2)
bt_final_fit <- fit(bt_final_workf, train)

bt_preds <- test |>
  bind_cols(predict(bt_final_fit, test))

cm_bt <- confusionMatrix(table(test$label, bt_preds$.pred_class))
cm_bt$byClass["F1"] # 0.1304348   
cm_bt$byClass["Precision"] # 0.375    
cm_bt$byClass["Recall"] # 0.07894737    

test |>
  bind_cols(predict(bt_final_fit, test)) |>
  conf_mat(truth = label, estimate = .pred_class) |>
  autoplot(type = "heatmap") 
