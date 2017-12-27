############

# Chapter 5 - Converting to and from tidy and non-tidy formats

############

# Most R tools for natural language processing aren't compatible with tidy format
## data. Chapter discusses how to work between tidy data and such tools that
## require data in other formats.

# Note that I'm going to place the packages where they're used, rather than
## at the head of the script to help keep track of what functions are associated
## with what packages.

# Tidying a document-term matrix ------------------------------------------

# DTR is a common structure for working with text mining packages. Features the 
## following:
    # Each row represents one document (e.g., book or article)
    # Each column represents one term
    # Each value contains the number of appearances of that term in the document

# tidytext package provides two functions for working with DTMs
    # tidy() turns DTM into a tidy data frame
    # cast() turns a tidy dataframe into a matrix. Three versions:
      # cast_sparse() (converting to a sparse matrix from the Matrix package), 
      # cast_dtm() (converting to a DocumentTermMatrix object from tm), 
      # cast_dfm() (converting to a dfm object from quanteda).

# Tidying a DocumentTermMatrix object -------------------------------------

library(tm) # topic models package
library(topicmodels) # Note that the book only tells you to load "tm"
                     ## but if you don't install topicmodels, the data won't load

# Collection of AP articles
data("AssociatedPress", package = "topicmodels")
AssociatedPress

# Use Terms() function to access terms in the data set

terms <- Terms(AssociatedPress)
head(terms)

# Use tidy() function to convert this to a tidy data frame. This is similar to
## melt() for non-sparse data frames (whereas tidy() is intended for 
## sparse data frames?)
library(dplyr)
library(tidytext)

ap_td <- tidy(AssociatedPress)
ap_td

# Notes that only non-zero values are included in the tidied output (gets rid of
# the sparsity...) No rows where count is zero.

# Try a sentiment analysis on the tidied result
ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments

# Now can see which terms contribute most to positive or negative sentiment
## Comments that the inclusion of "vice" as a negative term is likely a misfire
## as it is likely most commonly used in "vice president" given the nature of the
## source.

library(ggplot2)

ap_sentiments %>%
  count(sentiment, term, wt = count) %>% # not familiar with wt in this context.
  # documentation notes that wt sums the (non-missing) values of variable wt
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>% # nice trick - negative val
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

# Tidying dfm objects -----------------------------------------------------

# Other packages use different object types, like document-feature-matrix (dfm) 
## objects from the quanteda package

library(quanteda)

data("data_corpus_inaugural", package = "quanteda") # Presidential inaugural speeches

inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)

inaug_dfm

# tidy() lets us tokenize the object

inaug_td <- tidy(inaug_dfm)
inaug_td

# Find words most specific to each speech with tf-idf
inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf

# Check out four specific presidents
inaug_tf_idf %>%
  filter(document %in% c("1861-Lincoln",
                         "1933-Roosevelt",
                         "1961-Kennedy",
                         "2009-Obama")) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term)))) %>% 
  group_by(document) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~document, ncol = 2, scales = "free") +
  coord_flip()

# Now extract the year from each document's name and calculate the number of
## words by year

library(tidyr)

year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>% # this is a good trick
  # complete includes 0's (indicating words that didn't appear in a doc)
  complete(year, term, fill = list(count = 0)) %>% # 
  group_by(year) %>%
  mutate(year_total = sum(count))

# Select words and see how they change in frequency over time

year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")

# Casting tidy text data into a matrix ------------------------------------

# Use cast_ verbs from tidytext to create document-term matrices for algorithms
## that expect such objects

# E.g., cast tidy AP data back into a DTM

ap_td %>%
  cast_dtm(document, term, count)

# Alternatively, could cast into a document-feature-matrix
ap_td %>%
  cast_dfm(term, document, count)

# Some tools require a sparse matrix
library(Matrix)

# cast into a Matrix object
m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)

dim(m)

# Create a DTM of Jane Austen's books
library(janeaustenr)

austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

austen_dtm

# This approach allows for reading, filtering, and processing with dplyr
## and other tidy tools, then conversion into DTMs/DFMs for machine learning

# Tidying corpus objects with metadata ------------------------------------

# Corpus structures (e.g., the Corpus objects in the tm package) store additional
## metadata alongside the text, such as D, date/time, title, or language for 
## each document.

# The acq corpus from tm includes 50 Reuters articles
data("acq")
acq

# Corpus objects are structured like lists.
## First document
acq[[1]]

# While this is a flexible storage format, it isn't conducive to using tidy tools
## Use tidy() to create one row per document, with metadata included in separate
## columns alongside the text

acq_td <- tidy(acq)
acq_td

# Can now use unnest_tokens() to find most common words across the documents
acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# most common words across all docs
acq_tokens %>%
  count(word, sort = TRUE)

# Or words specific to each doc
# tf-idf
acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))


# Example: Mining financial articles --------------------------------------

# Notes that corpus objects are common output format for data ingesting (scraping?)
## packages. Provides example of tm.plugin.webmining - connects to online feeds
## to retrieve news artciles based on a keyword. 
## WebCorpus(GoogleFinanceSource("NASDAQ:MSFT")) retrieves 20 most recent articles
## related to Microsoft (MSFT) stock

# As an example, we're going to retrieve articles relevant to nine tech stocks

library(tm.plugin.webmining) # Looks like you need Java for this. Getting an error.
library(purrr)

company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook",
             "Twitter", "IBM", "Yahoo", "Netflix")
symbol <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", "TWTR", "IBM", "YHOO", "NFLX")

download_articles <- function(symbol) {
  WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))
}

stock_articles <- data_frame(company = company,
                             symbol = symbol) %>%
  # map applies function to every object in symbol
  mutate(corpus = map(symbol, download_articles)) 

# Convert to data frame with tidy() then unnest with unnest_tokens()
stock_tokens <- stock_articles %>%
  unnest(map(corpus, tidy)) %>%
  unnest_tokens(word, text) %>%
  select(company, datetimestamp, word, id, heading)

# tf-idf to find words most specific to each stock
library(stringr)

stock_tf_idf <- stock_tokens %>%
  count(company, word) %>%
  filter(!str_detect(word, "\\d+")) %>%
  bind_tf_idf(word, company, n) %>%
  arrange(-tf_idf)

# Determine what words contribute most to positive/negative sentiments
stock_tokens %>%
  anti_join(stop_words, by = "word") %>%
  count(word, id, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(contribution = sum(n * score)) %>%
  top_n(12, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution)) +
  geom_col() +
  coord_flip() +
  labs(y = "Frequency of word * AFINN score")

# Need to use another sentiment lexicon better suited to finance: 
## Loughran and McDonald dictionary of financial sentiment terms
## avoids words like "share", "fool", "liability", "risk"
## Divides words into six sentiments - positive, negative, litigious,
## uncertain, constraining, and superfluous.

stock_tokens %>%
  count(word) %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  group_by(sentiment) %>%
  top_n(5, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free") +
  ylab("Frequency of this word in the recent financial articles")

# Count number of uses of each sentiment-associated word in each corpus
stock_sentiment_count <- stock_tokens %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  count(sentiment, company) %>%
  spread(sentiment, n, fill = 0)

# Rough measure of sentiment --> (positive - negative) / (positive + negative)
stock_sentiment_count %>%
  mutate(score = (positive - negative) / (positive + negative)) %>%
  mutate(company = reorder(company, score)) %>%
  ggplot(aes(company, score, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Company",
       y = "Positivity score among 20 recent news articles")