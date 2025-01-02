library(tidyverse)
library(rtweet)
library(dataedu)
library(randomNames)
library(tidygraph)
library(ggraph)

# recent 1,000 tweets with hashtag #rstats
rstats_tweets <-
  search_tweets("#rstats")

tt_tweets <-
  search_tweets("#tidytuesday")

tt_tweets <- dataedu::tt_tweets

regex <- "@([A-Za-z]+[A-Za-z0-9_]+)(?![A-Za-z0-9_]*\\.)"

tt_tweets <-
  tt_tweets %>%
  # use regular expression to identify all the usernames in a tweet
  mutate(all_mentions = str_extract_all(text, regex)) %>%
  unnest(all_mentions)

mentions <-
  tt_tweets %>%
  mutate(all_mentions = str_trim(all_mentions)) %>%
  select(sender = screen_name, all_mentions)

# remove "@" from all_mentions column
edgelist <-
  mentions %>%
  mutate(all_mentions = str_sub(all_mentions, start=2)) %>%
  # rename all_mentions to receiver
  select(sender, receiver = all_mentions)

interactions_sent <- edgelist %>%
  # counts how many times each sender appears in the data frame (how many interactions each individual sent)
  count(sender) %>%
  # arranges data frame in descending order 
  arrange(desc(n))

# those who sent more than one interaction
interactions_sent <-
  interactions_sent %>%
  filter(n>1)

edgelist <- edgelist %>%
  # include those in the interactions_sent data frame
  filter(sender %in% interactions_sent$sender,
         receiver %in% interactions_sent$sender)

# identifies first column of sender and receiver
g <- as_tbl_graph(edgelist)

g %>%
  ggraph(layout = "kk") + 
  # adds points to the graph 
  geom_node_point() + 
  # add links (0.2 = lines are partially transparent)
  geom_edge_link(alpha = .2) + 
  theme_graph()

g %>%
  # calculates centrality of each individual 
  # how influential an individual may be based on the interactions
  mutate(centrality = centrality_authority()) %>%
  ggraph(layout = "kk") + 
  geom_node_point(aes(size = centrality, color = centrality)) + 
  # colors the points based upon their centrality 
  scale_color_continuous(guide = "legend") + 
  geom_edge_link(alpha = .2) + 
  theme_graph()
