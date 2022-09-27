library(tidyverse)
library(tidytext)
library(tm)
library(quanteda)
library(stopwords)
library(lsa)
library(SnowballC)
library(quanteda.textstats)
library(RColorBrewer)
library(kableExtra)


# Reading data ------------------------------------------------------------

text <- read.csv("Desktop/UCLA/Amazon/data/hackathon_text.csv")


# Cleaning ----------------------------------------------------------------

text <- text [text$product_category %in% c("SHOES_CLOTHING_PERSONAL"), ]

text$item_name <- gsub("[[:punct:][:blank:]]+", " ", as.character(text$item_name))
text$item_keywords <- gsub("[[:punct:][:blank:]]+", " ", as.character(text$item_keywords))
text$bullet_point <- gsub('[[:punct:] ]+',' ', as.character(text$item_keywords))

text$combined_text <- str_c(text$bullet_point, '', text$item_keywords, '', text$item_name)

text_new <- text %>% 
  mutate(combined_text = iconv(combined_text, from = "latin1", to = "ASCII")) %>%
  filter(!is.na(combined_text))

text_new$row_num <- seq.int(nrow(text_new))

text_new <- text_new %>% 
  mutate(text = "text")

text_new$text_id <- str_c(text_new$text, text_new$row_num)

text_new <- text_new %>% 
  select(-c(row_num, text))

text_new %>% write_csv("Desktop/UCLA/Amazon/data/clothing_text.csv")


# NLP ---------------------------------------------------------------------

toks <- text_new$combined_text %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(stopwords("English")) %>% 
  tokens_select(c("amazon", "brand"), selection = "remove", case_insensitive = TRUE) %>% 
  tokens_wordstem(language = quanteda_options("language_stemmer"))

cos_dtm <- toks %>% 
  dfm() %>% 
  dfm_tfidf() %>% 
  textstat_simil(method = "cosine")

result <- print(sort(cos_dtm[,"text444"],dec=T)) %>% 
  as.data.frame() %>% 
  rownames_to_column("text_id") %>% 
  inner_join(text_new, by = "text_id") %>% 
  head(15)

result %>% 
  select(c(item_id, similarity, item_name, product_type, brand)) %>% 
  kbl(caption = "Similar Items") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = T) %>% 
  row_spec(1:1, color = "black", background = "#f36537")


# Categorical Filter ------------------------------------------------------





