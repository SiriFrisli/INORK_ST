## Div to clean up the sample for manual labeling

covid_samp <- read_xlsx("D:/Data/Training samples/Old manual label/misinformation_labeled.xlsx")
covid_samp$text <- apply(covid_samp["text"], 1, removeURL)
covid_samp$text <- apply(covid_samp["text"], 1, removeUsernames)
covid_samp$text <- tolower(covid_samp$text)

# Want to only keep the external links within the texts
covid_match <- subset(covid, (covid$id %in% covid_samp$id))

covid_match <- covid_match |>
  select(id, urls)

covid_match <- covid_samp |>
  cbind(covid_match)

covid_match <- covid_match |>
  drop_na(urls)

covid_match <- covid_match |>
  unnest(urls, names_sep = ".") |>
  select(-c(urls.start, urls.end, urls.unwound_url, urls.description))

# Creating a new df, filtering out links to twitter, I only want to keep external links
covid_ext <- covid_match |>
  filter(!str_detect(urls.display_url, "twitter.com")) |>
  filter(!str_detect(urls.display_url, "t.co"))

covid_ext <- covid_ext |>
  select(id, text, conversation_id, expanded_url = urls.expanded_url)
covid_ext$claim <- NA
covid_ext$description <- NA
covid_ext$fact_check <- NA

covid_match$expanded_url <- NA
covid_match <- covid_match |>
  select(id, text, conversation_id, label, claim, description, fact_check, expanded_url)


covid_label <- full_join(covid_ext, covid_match, by = "id") |>
  mutate(expanded_url = coalesce(expanded_url.x, expanded_url.y),
         text = coalesce(text.x, text.y),
         label = coalesce(label.x, label.y),
         claim = coalesce(claim.x, claim.y),
         description = coalesce(description.x, description.y),
         fact_check = coalesce(fact_check.x, fact_check.y),
         conversation_id = coalesce(conversation_id.x, conversation_id.y)) |>
  select(id, text, conversation_id, label, claim, description, fact_check, expanded_url)

covid_label <- covid_label[!duplicated(covid_label$id), ]

write_csv2(covid_label, "D:/Data/Training samples/misinformation_class_clean.csv")


covid_1 <- read_xlsx("D:/Data/Training samples/Old manual label/misinformation_labeled.xlsx")
covid_2 <- read_xlsx("D:/Data/Training samples/misinformation_class_clean.xlsx")

covid_2 <- covid_2 |>
  sample_n(28)

covid_label <- full_join(covid_label, covid_2, by = "id") |>
  mutate(text = coalesce(text.x, text.y),
         label = coalesce(label.x, label.y)) |>
  select(id, text, expanded_url, label)