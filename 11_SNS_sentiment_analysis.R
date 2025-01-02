library(tidyverse)
library(here)
library(dataedu)
library(tidytext)

raw_tweets <- dataedu::tt_tweets

glimpse(raw_tweets)
str(raw_tweets)

tweets <-
  raw_tweets %>%
  # filter for English tweets
  filter(lang == "en") %>%
  select(status_id, text) %>%
  # convert the ID field to the character data type 
  mutate(status_id = as.character(status_id))

# "tokenize" each row to represent a single word
# column of tokens = "word"
# tokenize items in the "text" column
tokens <-
  tweets %>%
  unnest_tokens(output = word, input = text)

data(stop_words)

tokens <-
  tokens %>%
  anti_join(stop_words, by="word")

# which word showed up the most often 
# "sort=TRUE": descending order
tokens %>%
  count(word, sort = TRUE) %>%
  # n as a percent of total words
  mutate(percent = n / sum(n) * 100)

get_sentiments("nrc")

# only positive ini the NRC dataset 
nrc_pos <-
  get_sentiments("nrc") %>%
  filter(sentiment == "positive")

# match to tokens 
pos_tokens_count <-
  tokens %>%
  inner_join(nrc_pos, by = "word") %>%
  # total appearance of positive words
  count(word, sort = TRUE)

pos_tokens_count

# filter words that appear 75 times or more 
# visualize frequency as bar graph
pos_tokens_count %>%
  filter(n>=75) %>%
  ggplot(., aes(x = reorder(word, -n), y = n)) + 
  geom_bar(stat = "identity", fill = dataedu_colors("darkblue")) + 
  labs(
    title = "Count of Words Associated with Positivity",
    subtitle = "Tweets with the hashtag #tidytuesday",
    caption = "Data: Twitter and NRC",
    x = "",
    y = "Count"
  ) +
  theme_dataedu()

# dataset that has status_id with the word "dataviz
dv_tokens <-
  tokens %>%
  filter(word == "dataviz")

# extract vector of status_id from dv_tokens
head(dv_tokens$status_id)

# dataset for tweets that have positive words
# nrc_pos$word: bector of positive words
pos_tokens <-
  tokens %>%
  filter(word %in% nrc_pos$word)

# extract vector of status_id
head(pos_tokens$status_id)

# data frame of unique status_id values
pos_tokens <-
  pos_tokens %>%
  distinct(status_id)

dv_tokens <-
  dv_tokens %>%
  distinct(status_id)

# status_id %in% pos_token$status_id: logibal statement 
# 1: value of positive if the logical statement is TRUE
# 0: value of positive if the logical statement is FALSE
dv_pos <-
  tweets %>%
  # only tweets that have the dataviz status_id
  filter(status_id %in% dv_tokens$status_id) %>%
  # is the status_id from the positive word vector?
  mutate(positive = if_else(status_id %in% pos_tokens$status_id, 1, 0))

dv_pos %>%
  count(positive) %>%
  mutate(perc = n/sum(n))

# dataset of tweets with positive words
pos_tweets <-
  tweets %>%
  mutate(positive = if_else(status_id %in% pos_tokens$status_id, 1, 0)) %>%
  filter(positive == 1)

# random selection
set.seed(2020) #pick the same 10 rows everytime

pos_tweets %>%
  sample_n(., size=10)
