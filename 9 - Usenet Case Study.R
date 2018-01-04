############

# Chapter 9 - Case study: analyzing usenet text

############

# Analysis of 20,000 messages sent to 20 Usenet bulletin
## boards in 1993. Includes newsgroups for topics including
## politics, religion, cars, sports, cryptography, etc.
## Popular for text analysis and machine learning exercises.

# Data are available at:
## http://qwone.com/~jason/20Newsgroups/ 
## (the 20news-bydate.tar.gz file) 

# Note that due to the size of the data, I'm following along
## with the code but not running it at this time.

# Pre-processing ----------------------------------------------------------

# Reading in all the meassages in the 20news-bydate folder,
## which contains sub-folders with one file for each message.

library(dplyr)
library(tidyr)
library(purrr)
library(readr)

training_folder <- "data/20news-bydate/20news-bydate-train/"

# Define a function to read all files from a folder into a data frame
read_folder <- function(infolder) {
  data_frame(file = dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}

# Use unnest() and map() to apply read_folder to each subfolder
raw_text <- data_frame(folder = dir(training_folder, full.names = TRUE)) %>%
  unnest(map(folder, read_folder)) %>%
  transmute(newsgroup = basename(folder), id, text)

raw_text

# Identify newsgroups and number of messages posted in each

library(ggplot2)

raw_text %>%
  group_by(newsgroup) %>%
  summarize(messages = n_distinct(id)) %>%
  ggplot(aes(newsgroup, messages)) +
  geom_col() +
  coord_flip()

# Pre-processing text -----------------------------------------------------

# Lots of extra text to remove prior to analysis (e.g.,
## header, lines -- before email signatures, etc.)

# Pre-processing with dplyr, cumsum() and str_detect() 
## from stringr

library(stringr)

# must occur after the first occurrence of an empty line,
# and before the first occurrence of a line starting with --
cleaned_text <- raw_text %>%
  group_by(newsgroup, id) %>%
  filter(cumsum(text == "") > 0,
         cumsum(str_detect(text, "^--")) == 0) %>%
  ungroup()

# Removing quotes from other users, as well as two messages
## (9704 and 9985) with a large amount of non-text content
cleaned_text <- cleaned_text %>%
  filter(str_detect(text, "^[^>]+[A-Za-z\\d]") | text == "",
         !str_detect(text, "writes(:|\\.\\.\\.)$"),
         !str_detect(text, "^In article <"),
         !id %in% c(9704, 9985))

# Now tokenize and remove stop words
library(tidytext)

usenet_words <- cleaned_text %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

# Words in newsgroups -----------------------------------------------------

# Identify most common words in the entire dataset
usenet_words %>%
  count(word, sort = TRUE)

# Now in each newsgroup
words_by_newsgroup <- usenet_words %>%
  count(newsgroup, word, sort = TRUE) %>%
  ungroup()

words_by_newsgroup

# Finding tf-idf within newsgroups ----------------------------------------

tf_idf <- words_by_newsgroup %>%
  bind_tf_idf(word, newsgroup, n) %>%
  arrange(desc(tf_idf))

tf_idf

# Extract words specific to a few newsgroups (top tf-idf)

tf_idf %>%
  filter(str_detect(newsgroup, "^sci\\.")) %>%
  group_by(newsgroup) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = newsgroup)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ newsgroup, scales = "free") +
  ylab("tf-idf") +
  coord_flip()

# What newsgroups tended to be similar to each other in text
## content? Pairwise correlation of word frequencies within 
## each newsgroup using pairwise_cor() from widyr

library(widyr)

newsgroup_cors <- words_by_newsgroup %>%
  pairwise_cor(newsgroup, word, n, sort = TRUE)

newsgroup_cors

# Filter stronger correlations among newsgroups and visualize
## as network

library(ggraph)
library(igraph)
set.seed(2017)

newsgroup_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") + # what happens when you change this?
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# Four main clusters - computers/electronics, politics/religion,
## motor vehicles, and sports

# Topic modeling ----------------------------------------------------------

# Sort Usenet messages that came from different newsgroups
## using LDA

# First divide up messages from four science-related newsgroups

# include only words that occur at least 50 times
word_sci_newsgroups <- usenet_words %>%
  filter(str_detect(newsgroup, "^sci")) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup() %>%
  filter(word_total > 50)

# convert into a document-term matrix
# with document names such as sci.crypt_14147
sci_dtm <- word_sci_newsgroups %>%
  unite(document, newsgroup, id) %>%
  count(document, word) %>%
  cast_dtm(document, word, n)

library(topicmodels)

sci_lda <- LDA(sci_dtm, k = 4, control = list(seed = 2016))

# Visualize each topic based on most frequent terms

sci_lda %>%
  tidy() %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip()

# Looks pretty good!

# Confirm classification by checking how docs from each newsgroup
## have higher "gamma" on each topic

sci_lda %>%
  tidy(matrix = "gamma") %>%
  separate(document, c("newsgroup", "id"), sep = "_") %>%
  mutate(newsgroup = reorder(newsgroup, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ newsgroup) +
  labs(x = "Topic",
       y = "# of messages where this was the highest % topic")

# Sentiment analysis ------------------------------------------------------

# Examine how often positive and negative words occurred
## in the Usenet posts using AFINN sentiment lexicon 
## (numeric positivity scores for each word) and visualize

newsgroup_sentiments <- words_by_newsgroup %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(newsgroup) %>%
  summarize(score = sum(score * n) / sum(n))

newsgroup_sentiments %>%
  mutate(newsgroup = reorder(newsgroup, score)) %>%
  ggplot(aes(newsgroup, score, fill = score > 0)) + # I like this
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("Average sentiment score")

# Sentiment analysis by word ----------------------------------------------

# Examine why some newsgroups were more positive/negative by 
## looking at the total positive and negative contributions
## of each word

contributions <- usenet_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(score))

contributions

# Words that had the greatest impact on sentiment scores
contributions %>%
  top_n(25, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip()

# Examine which words contributed most within each newsgroup
## as a check for context (e.g., "god" in one group may
## have a different sentiment than "god" in another)

top_sentiment_words <- words_by_newsgroup %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(contribution = score * n / sum(n))

top_sentiment_words

# Shows how sentiment analysis can be confounded...should
## examine influential words

# Sentiment analysis by message -------------------------------------------

# Find most positive/negative individual messages by 
## grouping and summarizing by id rather than newsgroup

sentiment_messages <- usenet_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(newsgroup, id) %>%
  summarize(sentiment = mean(score),
            words = n()) %>%
  ungroup() %>%
  filter(words >= 5) # to reduce noise

# Most positive messages
sentiment_messages %>%
  arrange(desc(sentiment))

# Look at most positive message in the whole dataset, 
## writing a function to print the message
print_message <- function(group, message_id) {
  result <- cleaned_text %>%
    filter(newsgroup == group, id == message_id, text != "")
  
  cat(result$text, sep = "\n")
}

print_message("rec.sport.hockey", 53560)

# Message appears to be chosen because it says 
## "winner" a lot

# Most negative message
sentiment_messages %>%
  arrange(sentiment)

print_message("rec.sport.hockey", 53907)

# N-gram analysis ---------------------------------------------------------

# Explore n-grams for use of negatives (e.g., "don't like")

usenet_bigrams <- cleaned_text %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

usenet_bigram_counts <- usenet_bigrams %>%
  count(newsgroup, bigram, sort = TRUE) %>%
  ungroup() %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Define words likely used in negation and visualize
## sentiment-associated words that most often followed
## (i.e., contributed in wrong direction)

usenet_bigram_counts %>%
  filter(word1 %in% negate_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = score * nn) %>%
  group_by(word1) %>%
  top_n(10, abs(contribution)) %>%
  ungroup() %>%
  mutate(word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by a negation") +
  ylab("Sentiment score * # of occurrences") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()




