packages <- c("tidyverse", "readxl")

for (x in packages){
  if(!require(x, character.only = TRUE)) {
    install.packages(x)
  }
}

for (x in packages) {
  library(x, character.only = TRUE)
}



filenames <- list.files("D:/Data/training samples/st_log_reg/", full.names = TRUE)
list_data <- lapply(filenames, readRDS)
names(list_data) <- paste('it', seq_along(filenames), sep = '_')

list2env(list_data, .GlobalEnv)

covid <- read_xlsx("D:/Data/training samples/misinformation_labeled_finished.xlsx")
covid$label <- case_when(
  covid$label == 0 ~ "non.misinfo",
  covid$label == 1 ~ "misinfo"
)
################################################################################
it_1_same <- it_1 |>
  semi_join(covid, by = join_by(id, label))

it_1_change<- it_1 |>
  anti_join(covid, by = join_by(id, label))

##
it_2_same <- it_2 |>
  inner_join(it_3, by = join_by(id, label))

it_2_change<- it_2 |>
  anti_join(it_3, by = join_by(id, label))

##
it_3_same <- it_3 |>
  inner_join(it_4, by = join_by(id, label))

it_3_change<- it_3 |>
  anti_join(it_4, by = join_by(id, label))

##
it_4_same <- it_4 |>
  inner_join(it_5, by = join_by(id, label))

it_4_change<- it_4 |>
  anti_join(it_5, by = join_by(id, label))

##
it_5_same <- it_5 |>
  inner_join(it_6, by = join_by(id, label))

it_5_change<- it_5 |>
  anti_join(it_6, by = join_by(id, label))

##
it_6_same <- it_6 |>
  inner_join(it_7, by = join_by(id, label))

it_6_change<- it_6 |>
  anti_join(it_7, by = join_by(id, label))

##
it_7_same <- it_7 |>
  inner_join(it_8, by = join_by(id, label))

it_7_change<- it_7 |>
  anti_join(it_8, by = join_by(id, label))

## 
it_8_same <- it_8 |>
  inner_join(it_9, by = join_by(id, label))

it_8_change<- it_8 |>
  anti_join(it_9, by = join_by(id, label))

##
it_9_same <- it_9 |>
  inner_join(it_10, by = join_by(id, label))

it_9_change<- it_9 |>
  anti_join(it_10, by = join_by(id, label))

##
it_10_same <- it_10 |>
  inner_join(it_11, by = join_by(id, label))

it_10_change<- it_10 |>
  anti_join(it_11, by = join_by(id, label))

##
it_11_same <- it_11 |>
  inner_join(it_12, by = join_by(id, label))

it_11_change<- it_11 |>
  anti_join(it_12, by = join_by(id, label))

##
it_12_same <- it_12 |>
  inner_join(it_13, by = join_by(id, label))

it_12_change<- it_12 |>
  anti_join(it_13, by = join_by(id, label))

##
it_13_same <- it_13 |>
  inner_join(it_14, by = join_by(id, label))

it_13_change<- it_13 |>
  anti_join(it_14, by = join_by(id, label))

##
it_14_same <- it_14 |>
  inner_join(it_15, by = join_by(id, label))

it_14_change<- it_14 |>
  anti_join(it_15, by = join_by(id, label))

##
it_15_same <- it_15 |>
  inner_join(it_16, by = join_by(id, label))

it_15_change<- it_15 |>
  anti_join(it_16, by = join_by(id, label))

##
it_16_same <- it_16 |>
  inner_join(it_17, by = join_by(id, label))

it_16_change<- it_16 |>
  anti_join(it_17, by = join_by(id, label))
################################################################################
8 = 17
9 = 18
10 = 19
11 = 20 

##
it_7_same <- it_7 |>
  inner_join(it_8, by = join_by(id, label))

it_7_change<- it_7 |>
  anti_join(it_8, by = join_by(id, label))

## 
it_8_same <- it_8 |>
  inner_join(it_9, by = join_by(id, label))

it_8_change<- it_8 |>
  anti_join(it_9, by = join_by(id, label))

##
it_9_same <- it_9 |>
  inner_join(it_10, by = join_by(id, label))

it_9_change<- it_9 |>
  anti_join(it_10, by = join_by(id, label))

##
it_10_same <- it_10 |>
  inner_join(it_11, by = join_by(id, label))

it_10_change<- it_10 |>
  anti_join(it_11, by = join_by(id, label))
