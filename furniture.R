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

text <- text [text$product_category %in% c("FURNITURE"), ]

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

text_new %>% write_csv("Desktop/UCLA/Amazon/data/furniture_text.csv")


# NLP ---------------------------------------------------------------------

toks <- text_new$combined_text %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(stopwords("English")) %>% 
  tokens_select(c("amazon", "furniture"), selection = "remove", case_insensitive = TRUE) %>% 
  tokens_wordstem(language = quanteda_options("language_stemmer"))

cos_dtm <- toks %>% 
  dfm() %>% 
  dfm_tfidf() %>% 
  textstat_simil(method = "cosine")

cos_df <- cos_dtm %>% 
  as.data.frame()

# Result ------------------------------------------------------------------

result<- cos_df %>% 
  filter(document1 == "text238") %>% 
  arrange(desc(cosine)) %>% 
  rename(text_id = document2, similarity = cosine) %>% 
  inner_join(text_new, by = "text_id") %>% 
  select(c(item_id, similarity, item_name, brand, product_category)) %>% 
  head(15)

result %>% 
  kbl(caption = "Similar Items") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = T) %>% 
  row_spec(1:1, color = "black", background = "#f36537")


##--------------

result <- print(sort(cos_dtm[,"text26"],dec=T)) %>% 
  as.data.frame() %>% 
  rownames_to_column("text_id") %>% 
  inner_join(text_new, by = "text_id")

colnames(result)[colnames(result) == "."] <- "similarity"

result %>% 
  head(15) %>% 
  select(c(item_id, similarity, item_name, product_type, brand)) %>% 
  kbl(caption = "Similar Items") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = T) %>% 
  row_spec(1:1, color = "black", background = "#f36537")


# Categorical Filter ------------------------------------------------------

furniture_cat <- read_csv("Desktop/UCLA/Amazon/data/furniture_text_cat.csv")

result_cat <- furniture_cat %>% 
  select(item_id, price, rating, small_business, sponsor) %>% 
  left_join(result, by = "item_id")

sponsored <- result_cat %>% 
  filter(sponsor == "yes") %>% 
  arrange(desc(similarity)) %>% 
  select(c(item_id, similarity, item_name, sponsor)) %>% 
  head(5) %>% 
  kbl(caption = "Similar Items- Sponsored") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = T) %>% 
  row_spec(1:1, color = "black", background = "#f36537")

sponsored


highrating <- result_cat %>% 
  filter(sponsor == "yes") %>% 
  filter(rating == "star 4-5") %>% 
  arrange(desc(similarity)) %>% 
  select(c(item_id, similarity, item_name, rating)) %>% 
  head(5) %>% 
  kbl(caption = "Similar Items- Sponsored & Highly Rated") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = T) %>% 
  row_spec(1:1, color = "black", background = "#f36537")

highrating


samebrand <- result_cat %>% 
  filter(sponsor == "yes") %>% 
  filter(brand == "Rivet") %>% 
  arrange(desc(similarity)) %>% 
  select(c(item_id, similarity, item_name, brand)) %>% 
  head(5) %>% 
  kbl(caption = "Similar Items- Sponsored & Same Brand") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = T) %>% 
  row_spec(1:1, color = "black", background = "#f36537")

samebrand

smallbusiness <- result_cat %>% 
  filter(small_business == "yes") %>% 
  arrange(desc(similarity)) %>% 
  select(c(item_id, similarity, item_name, small_business)) %>% 
  head(5) %>% 
  kbl(caption = "Similar Items- Small Business") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = T) %>% 
  row_spec(1:1, color = "black", background = "#f36537")

smallbusiness
furniture_cat %>% 
  group_by(small_business) %>% 
  ggplot(aes(x = small_business)) +
  geom_bar(aes(fill = small_business, fill = "#f36537"), show.legend = FALSE) +
  labs(x = "Sponsored", y = "Frequency", title = "Simulation- Sponsorship") +
  theme(panel.background = element_rect(fill="#F4F6F7"),
        axis.text.y = element_text(size = 13))
  
furniture_cat %>% 
  group_by(price) %>% 
  ggplot(aes(x = fct_infreq(price))) +
  geom_bar(aes(fill = fct_infreq(price)), show.legend = FALSE) +
  scale_fill_brewer(palette = "Oranges") + 
  labs(x = "Price Range", y = "Frequency", title = "Simulation- Price") +
  theme(panel.background = element_rect(fill="#F4F6F7"),
        axis.text.x.bottom = element_text(angle = 30, size = 10))

furniture_cat %>% 
  group_by(rating) %>% 
  ggplot(aes(x = fct_infreq(rating))) +
  geom_bar(aes(fill = fct_infreq(rating)), show.legend = FALSE) +
  scale_fill_brewer(palette = "Oranges") + 
  labs(x = "Price Range", y = "Frequency", title = "Simulation- Rating") +
  theme(panel.background = element_rect(fill="#F4F6F7"),
        axis.text.x.bottom = element_text(angle = 30, size = 10))




