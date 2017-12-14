############

# Chapter 2 - Sentiment Analysis with Tidy Data

############


# Packages ----------------------------------------------------------------

library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(reshape2)

# The sentiments dataset --------------------------------------------------

# Three general purpose lexicons contained in the sentiments dataset (pre-loaded
## in the tidytext package). These are based on unigrams (single words).

## # AFINN - words scored from -5 (negative) to 5 (positive)
## # bing - binary (positive/negative)
## # nrc - binary (yes/no classes of emotions)

sentiments

get_sentiments("afinn") # pick specific lexicons

get_sentiments("bing")

get_sentiments("nrc")

# Sentiment analysis with inner_join() ------------------------------------

# Find the most common "joy" words in Emma

# First get the data to one word per row (tokenize)
## Note that "word" is the output column. This is helpful because the sentiment
## lexicons and stop words have "word" as columns as well (easier for inner_join
## and anti_join)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# Filter lexicon for joy
nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

# Filter Emma and join with joy words
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

# Track how sentiment changes throughout the novels
janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>% # positive vs. negative
  count(book, index = linenumber %/% 80, sentiment) %>% # integer division (%/%)
                                        # breaks up text into 80 line segments
                                        # tracked by index
  spread(sentiment, n, fill = 0) %>% # place negative and positive in sep columns
  mutate(sentiment = positive - negative) # calc net positive

# Plot how sentiment changes throughout the novels. Note that x is the index
## tracking where we are in the novel

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# Comparing the three sentiment dictionaries ------------------------------

# Test the three lexicons on Pride and Prejudice
pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

pride_prejudice

# Apply the dictionaries. Note that afinn assigns a sentiment score, while 
## bing and nrc are binary. Will need different approach for afinn
afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(score)) %>% # Adding up the score (-5 to 5, neg to pos)
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(pride_prejudice %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          pride_prejudice %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) # net positive

# Visualize the results from the three dictionaries
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# To better understand differences in results, check how many positive versus 
## negative results are contained in the bing and nrc lexicons
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment) # higher ratio of negative to positive words than nrc

# Most common positive and negative words ---------------------------------

# Check how much each word contributed to each sentiment
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>% # that's a pretty good trick - takes top 10 obs using min_rank
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# "miss" is most common negative word but this is a misfire, since it's commonly
## used as a title in Jane Austen's work. Remove it as a custom stop word

custom_stop_words <- bind_rows(data_frame(word = c("miss"), 
                                          lexicon = c("custom")), 
                               stop_words)

custom_stop_words


# Wordclouds -------------------------------------------------------------

# Using wordcloud package to visualize most common words
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Comparison cloud of most common positive and negative words
## Note that sizes are not comparable across segements

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% # cast to matrix
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

# Looking at units beyond just words --------------------------------------

# Tokenize into sentences 

PandP_sentences <- data_frame(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

PandP_sentences$sentence[2] # look at one sentence

# Tokenize into chapters

austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())

# What are the most negative chapters in each Austen novel? (highest proportion
## of negative workds in a chapter to normalize across chapters of diff lengths)

bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative") # get the negative words from bing lexicon

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n()) # get word counts for each chapter (for normalizing)

tidy_books %>%
  semi_join(bingnegative) %>% # think this is semi_join() to anchor to x 
                              ## (tidy_books). Inner_join() would duplicate x-rows 
                              ## if they matched with y-rows, right?
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>% # numerator - count of negative words
  left_join(wordcounts, by = c("book", "chapter")) %>% # bring in denominators
  mutate(ratio = negativewords/words) %>% # calculate percentage
  filter(chapter != 0) %>%
  top_n(1) %>% # grab your most negative chapters for each book
  ungroup()
