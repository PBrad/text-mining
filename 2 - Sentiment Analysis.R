############

# Chapter 2 - Sentiment Analysis with Tidy Data

############


# Packages ----------------------------------------------------------------

library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)

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

# Filter joy
nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

# Filter Emma and join
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)