############

# Chapter 3 - Analyzing Word and Document Frequency: tf-idf

############


# Packages ----------------------------------------------------------------

library(dplyr)
library(janeaustenr)
library(gutenbergr)
library(tidytext)
library(stringr)
library(ggplot2)

# Intro -------------------------------------------------------------------

# tf (term frequency) - how often a word appears in document

# idf (inverse document frequency) - decreases weight for commonly used words 
## and increases weight for words not used very much in a collection of docs

# tf-idf (tf multiplied by idf) - frequency of a term adjusted for how rarely
## it is used

# "The statistic tf-idf is intended to measure how important a word is to a 
## document in a collection (or corpus) of documents, for example, to one novel 
## in a collection of novels or to one website in a collection of websites."

# Term Frequency in Jane Austen Novels ------------------------------------

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words

# Distribution of n/total (i.e., term frequency)
## Shows lots of terms that rarely occur and fewer terms that occur frequently
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

# Zipf’s Law --------------------------------------------------------------
## The frequency that a word appears is inversely proportional to its rank

freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), # already ordered by n, so can use row_number()
         `term frequency` = n/total)

freq_by_rank

# Zipf's law commonly visualized by plotting rank on the x-axis 
## and term frequency on the y-axis, on logarithmic scales
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() # Looks right to me!

# Closer look at the middle subset
rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

# Plot it
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

# The bind_tf_idf function ------------------------------------------------
## tf-idf attempts to find the words that are important (i.e., common) 
##in a text, but not too common

book_words <- book_words %>%
  bind_tf_idf(word, book, n) # tokens-documents-counts
book_words

# Check for high tf-idf words
book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf)) # Characters of each novel top the list - nice

# Visualize
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

## Main takeaway: Jane Austen uses a lot of similar language throughout her novels
## and distinguishing features are her characters. "This is the point of tf-idf; 
## "it identifies words that are important to one document within a collection 
## of documents."

# Corpus of Physics Docs --------------------------------------------------

## Discourse on Floating Bodies by Galileo Galilei, 
## Treatise on Light by Christiaan Huygens, 
## Experiments with Alternate Currents of High Potential and High Frequency 
### by Nikola Tesla, 
## Relativity: The Special and General Theory by Albert Einstein

physics <- gutenberg_download(c(37729, 14725, 13476, 5001), 
                              meta_fields = "author")

# Word frequency
physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE) %>%
  ungroup()

physics_words

# Now tf-idf
plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan", 
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

plot_physics %>% 
  group_by(author) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

# What's up with "eq" in Einstein?
physics %>% 
  filter(str_detect(text, "eq\\.")) %>% 
  select(text)

# K1 is a coordinate system in Einstein
physics %>% 
  filter(str_detect(text, "K1")) %>% 
  select(text)

# “AB”, “RC”, and so forth are names of rays, circles, angles, and so forth 
## for Huygens
physics %>% 
  filter(str_detect(text, "AK")) %>% 
  select(text)

# Remove some of the less meaningful words (based on further exploration)
## Basically creating custom stop-words

# define
mystopwords <- data_frame(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                                   "fig", "file", "cg", "cb", "cm"))

# apply
physics_words <- anti_join(physics_words, mystopwords, by = "word")

# tf-dif
plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(author) %>% 
  top_n(15, tf_idf) %>%
  ungroup %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

# plot
ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()
