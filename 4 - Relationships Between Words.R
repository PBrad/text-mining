############

# Chapter 4 - Relationships Between Words

############

# Packages ----------------------------------------------------------------

library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(widyr)

# Tokenizing by n-gram ----------------------------------------------------

# Tokenize by consecutive sequences of words (n-grams). Specify the number of 
## words in the sequence with n. Two-word n-grams are bigrams

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

# Counting and filtering n-grams ------------------------------------------

# Most common bigrams
austen_bigrams %>%
  count(bigram, sort = TRUE)

# Lots of stop words. Use separate() from tidyr to split bigrams into two columns 
## and remove stop words

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>% # Filtering out bigrams with stop words
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts # Characters

# Now re-combine with unite() - inverse of separate() function
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

# Trigrams
austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# Analyzing bigrams -------------------------------------------------------
# Most common streets
bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

# Tf-idf for bigrams
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% # for the sort?
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

# Using bigrams to provide context in sentiment analysis ------------------

# Helpful for determining when words are preceded by "not"
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

# Can use bigrams with not to reassign sentiments
## Using AFINN which gives a numeric score to words based on positive/negative
## sentiment.
AFINN <- get_sentiments("afinn")

AFINN

# Examine words most commonly preceded by "not" AND associated with a sentiment
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words

# Determine which words contributed the most to the "wrong" direction of sentiment
## Multiply by the number of times the word appeared with a "not" preceding it
not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) + # This is a good trick. stat_identity as default
  xlab("Words preceded by \"not\"") + # escapes for " !
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

# Additional negation words
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup() %>% 
  group_by(word1) %>% 
  arrange(desc(n)) %>% 
  filter(row_number() <= 20) %>% 
  ungroup()

  
negated_words %>% 
  mutate(contribution = n * score) %>%
  arrange(desc(contribution)) %>% 
  # mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(reorder(word2, contribution), contribution, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) + # This is a good trick. stat_identity as default
  xlab("Words preceded by negation term") + 
  ylab("Sentiment score * number of occurrences") +
  facet_wrap(~word1, ncol = 2, scales = "free") +
  coord_flip()

# Visualizing a network of bigrams with ggraph ----------------------------

# First igraph package - graph_from_data_frame() - takes columns of the 
## node an edge is coming "from" and where it is going "to", 
## as well as edge attributes

bigram_counts

# Filter for most common
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

# Interesting - notes that igraph has graphing functions built in but that 
## they are not its main purpose, so other packages have been built to visualize
## igraph objects. They recommend ggraph because it uses the grammar of graphics
## like ggplot2.

set.seed(2017)

ggraph(bigram_graph, layout = "fr") + # converts to ggraph object
  geom_edge_link() + # add layers
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Now some polishing operations
set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  # make edges transparent based on how common the bigram is
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, 
                 # add directionality with an arrow ("directed graph")
                 arrow = a, end_cap = circle(.07, 'inches')) +
  # Change color and size of nodes
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  # helpful theme for plotting networks
  theme_void()

# Notes that this is a Markov chain, where each choice of word depends only 
## on the previous word

# Visualizing bigrams in other texts --------------------------------------

# First wrap the cleaning and visualizing bigrams code into functions for reuse

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

# Try the King James version of the bible (book 10 on Project Gutenberg):
library(gutenbergr)
kjv <- gutenberg_download(10)

# filter out rare combinations, as well as digits
library(stringr)

kjv_bigrams <- kjv %>%
  count_bigrams()

kjv_bigrams %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()

# Counting and correlating pairs of words with widyr ----------------------

# Can be interesting to identify words that tend to co-occur in documents and
## chapters, even if they don't appear next to each other

# Correlations can be challenging for tidy data, since you typically need data
## in a wide format in order to run correlations.

# However, "the philosophy behind the widyr package, which can perform operations 
## such as counting and correlating on pairs of values in a tidy dataset. 
## The widyr package first ‘casts’ a tidy dataset into a wide matrix, performs 
## an operation such as a correlation on it, then re-tidies the result.

# cast-operarte-melt

# Counting and correlating among sections ---------------------------------

# Look at Pride & Prejudice in 10-line sections and see what words tend to 
## co-occur within sections

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words

# pairwise_count() function will count common pairs of words in each section
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

# Find words that most commonly co-occur with "Darcy"
word_pairs %>%
  filter(item1 == "darcy")

# Pairwise correlation ----------------------------------------------------

# Examining correlations between words tells us how often they occur together
## relative to how often they occur separately

# Will use the "phi coefficient" which is a common measure of binary correlation.
## This is equivalent to Pearson when applied to binary data (I've heard this
## described as point-biserial as well, I believe)

# Use pairwise_cor() function to determine phi coefficient between words
## appearing in the same section

# we need to filter for at least relatively common words first
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors

# Find words most correlated with "pounds"
word_cors %>%
  filter(item1 == "pounds")

# Let's us identify other interesting words and find correlations with them
word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") + # could do geom_col() here with identity as default
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# Now visualize correlations/clusters of words that co-occur often as a network

set.seed(2016)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# Unlike bigrams, this is not a directed graph (relationships are symmetrical 
## rather than directional, so no arrows)