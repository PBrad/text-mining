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
