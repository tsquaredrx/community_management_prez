#### Analyzing Reddit
#### 15 July, 2021

# Install necessary packages
install.packages(c("RedditExtractoR", "tidyverse", "tidytext", "wordcloud", "widyr", "igraph", "ggraph"))

# call libraries
library(RedditExtractoR)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(widyr)
library(igraph)
library(ggraph)
library(RColorBrewer)

# Get url links to a thread by keyword
links <- reddit_urls(search_terms = "GameStop", page_threshold = 1)
View(links)

# Save as spreadsheet
write.csv(links, "GameStop_reddit_data.csv")

# Examine individual thread url
url <- "http://www.reddit.com/r/Superstonk/comments/na47on/theory_gamestop_received_the_first_batch_of_proxy/"

# Pull data and visualize network
content <- reddit_content(url)
View(content)
user <- user_network(content, include_author = TRUE, agg = FALSE)
user$plot

# Let's do another search
# Get url links to a thread by keyword
aita_links <- reddit_urls(search_terms = "AITA", page_threshold = 1)
View(aita_links)

# Examine new thread url
url <- "http://www.reddit.com/r/AmItheAsshole/comments/oe6gq1/aita_for_refusing_to_let_my_stepson_return_home/"
aita_content <- reddit_content(url)
View(aita_content)

write.csv(aita_content, "aita_reddit_data.csv")

aita_content

# Data cleaning and sentiment analysis
# Basic text analysis
comments <- aita_content %>%
  unnest_tokens(word, comment)

# Remove stopwords
cleaned_comments <- comments %>%
  anti_join(get_stopwords())

# Word count
cleaned_comments %>%
  count(word, sort = TRUE) 

# Get positive and negative words for sentiment analysis
nrcpos <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")

cleaned_comments %>%
  semi_join(nrcpos) %>%
  count(word, sort = TRUE)

nrcneg <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")

cleaned_comments %>%
  semi_join(nrcneg) %>%
  count(word, sort = TRUE)

bing_word_counts <- cleaned_comments %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() +
  theme_minimal()


# Basic visualization
# Wordcloud
cleaned_comments %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 500, scale = c(6, 0.35), 
                 vfont = c("gothic english", "plain"),
                 colors = brewer.pal(max(4, ncol(n)), "Dark2")))

# Word co-ocurrences and correlations
comm_desc <- aita_content %>% 
  unnest_tokens(word, comment) %>% 
  anti_join(stop_words)

desc_word_pairs <- comm_desc %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

# Top 50 word pairs by frequency
print(tbl_df(desc_word_pairs), n = 50)

# Word networks and clusters for frequency 10 or higher
desc_word_pairs %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# Try 20 or higher
desc_word_pairs %>%
  filter(n >= 15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# Bigrams and sequential combinations
aita_bigrams <- aita_content %>%
  unnest_tokens(bigram, comment, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)  

aita_bigrams

# Look at combinations > 3
bigram_graph <- aita_bigrams %>%
  filter(n > 3) %>%
  graph_from_data_frame()

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# Social network analysis

nodes <- read_csv("nodes_data.csv")
ties <- read_csv("edges_data.csv")

nodes
ties

# Create graph object
net <- graph.data.frame(ties, vertices = nodes, directed = TRUE)

net
V(net)
E(net)
V(net)$team

# Centrality measures
degree(net)
betweenness(net)
closeness(net)

# Make a palette of 2 colors
co  <- brewer.pal(3, "Set1")

# create a vector of color
my_colors <- co[as.numeric(as.factor(V(net)$team))]

# Basic plot
plot(net)

# Nicer plot
plot(net,
     layout = layout_with_fr,
     vertex.size = degree(net) * 2.5,
     vertex.color = my_colors,
     vertex.frame.color = NA,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.label.dist = 1.5,
     edge.color = ifelse(E(net)$weight >= 5, "red1", "grey"),
     edge.width = E(net)$weight,
     edge.arrow.size = 0.5)

# Community detection via clustering algorithms
friends_com <- cluster_walktrap(net)
plot(friends_com, net)

plot_dendrogram(friends_com)

# Do the same for undirected network 
net <- graph.data.frame(ties, vertices = nodes, directed = FALSE)

friends_com <- cluster_walktrap(net)
plot(friends_com, net)

