############

# Chapter 4 - Relationships Between Words

############

# Packages ----------------------------------------------------------------

library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)
library(ggraph)
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
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()
