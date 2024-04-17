## Preprocessing

library(tidyverse)

# Loading and preprocessing the data, so that thre's no need to do those steps each time the data is loaded
covid <- readRDS("D:/Data/covid_relevant_nort_domain.RDS")

removeURL <- function(tweet) {
  return(gsub("http\\S+", "", tweet))
}

removeUsernames <- function(tweet) {
  return(gsub("@[a-z,A-Z,_]*[0-9]*[a-z,A-Z,_]*[0-9]*", "", tweet))
}

covid$text <- apply(covid["text"], 1, removeURL)
covid$text <- apply(covid["text"], 1, removeUsernames)

covid$text <- tolower(covid$text)
covid$text <- gsub("[[:punct:]]", " ", covid$text)

covid <- covid |>
  select(id, text, conversation_id, created_at, author_hash, in_reply_to_user_hash)

# saving the processed df
saveRDS(covid, "D:/Data/covid_processed.RDS")


