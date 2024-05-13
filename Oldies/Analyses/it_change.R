packages <- c("tidyverse", "readxl")

for (x in packages){
  if(!require(x, character.only = TRUE)) {
    install.packages(x)
  }
}

for (x in packages) {
  library(x, character.only = TRUE)
}



filenames <- list.files("D:/Data/training samples/st_iterations", full.names = TRUE)
list_data <- lapply(filenames, readRDS)
names(list_data) <- paste('it', seq_along(filenames), sep = '_')

list2env(list_data, .GlobalEnv)

covid <- read_xlsx("D:/Data/training samples/misinformation_labeled.xlsx")
covid |>
  count(label)

################################################################################
it_1_same <- it_1 |>
  inner_join(it_2, by = join_by(id, label))

it_1_change<- it_1 |>
  anti_join(it_2, by = join_by(id, label))

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

################################################################################

