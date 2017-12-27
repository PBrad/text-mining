############

# Chapter 6 - Topic Modeling

############

# Topic modeling is a method of unsupervised classification of documents, similar to 
## clustering, which finds natural groups of items. Latent Dirichlet allocation (LDA)
## is a popular method for fitting topic models. Treats each document as a mixture
## of topics and each topic as a mixture of words. Allows documents to overlap each other
## in terms of content, rather than being separated into discrete groups.

# Latent Dirichlet allocation ---------------------------------------------

# Two main principles
  # Every document is a mixture of topics: e.g., "Document 1 is 90% topic A and 10%
  ## topic B, while Document 2 is 30% topic A and 70% topic B

  # Every topic is a mixture of words: e.g., two-topic model of politics and entertainment
  ## may have president, congress, and government as common words in politics
  ## and movies, television, and actor in entertainment, but budget in both

# get the AP dataset

library(topicmodels)

data("AssociatedPress")
AssociatedPress

# Use LDA() function from topicmodels package, setting k = 2 to create a 2-topic LDA model
# set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

# Notes that fitting the model is the easy part - now need to explore and interpret the 
## model using the tidy approach

# Word-topic probabilities ------------------------------------------------

# Use tidy() from tidytext to extract per-topic-per-word probabilities from the model

library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

# For each topic-term combination, the model copmutes the probability of that term being
## generated from that topic.

# Can use dplyr's top_n() to find the 10 terms that are most common within each topic and
## then visualize with ggplot2

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#Looks like business and politics

# Alternative - look at terms that had the greatest difference in beta between the
## two topics using the log ratio (helpful because it makes the difference
## symmetrical). Also limit it to a set of the most common words (beta > 1/1000)

library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

beta_spread %>% 
  mutate(abs_log = abs(log_ratio)) %>% 
  mutate(rank = dense_rank(desc(abs_log))) %>% 
  filter(rank <= 20) %>% 
  ggplot(aes(reorder(term, log_ratio), log_ratio)) + 
  geom_bar(stat = "identity") +
  ylab("Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()
