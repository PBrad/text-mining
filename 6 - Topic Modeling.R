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

# Document-topic probabilities --------------------------------------------

# Examing per-document-per-topic probabilities, called "gamma"
## Each value is an estimated proportion of words from that document that are generated
## from that topic.

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

# Looks like most documents are a mix of topics but doc 6 is almost exclusively
## topic 2. Check this by looking at most common words in that doc

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

# Example: the great library heist ----------------------------------------
## Four books - chapters torn apart, unlabeled, and placed in a pile
## Use topic modeling to see how the chapters cluster - should tell us
## which chapters belongs to which book

# Retrieve text with gutenbergr package
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

# Tokenize into separate words, remove stop words, and treat each chapter as sep document

library(stringr)

# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

# LDA on chapters ---------------------------------------------------------

# topicmodels package requires DocumentTermMatrix, so convert tidy to DTM

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

# Use LDA to create a four-topic model
chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

# Check per-topic-per-word probabilities
chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

# top five terms in each topic
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

# Visualize
library(ggplot2)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Per-document classification ---------------------------------------------
# Try to put the chapters back in the right books by looking at gamma 
## - the per-document-per-topic probabilities

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

# Would expect chapters within a book to be mostly generated from the corresponding topic

# Separate doc name into title and chapter
chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma

# reorder titles in order of topic 1, topic 2, etc before plotting
chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

# Check for cases where topic most associated w/ a chapter belonged to another book
# First find topic most associated with each chapter using top_n()
chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications

# Then compare to the "consensus" topic for each book to see which were misidentified
book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)

# Just two chapters from Great Expectations are misclassified

# By word assignments: augment --------------------------------------------

# Use augment() to identify which words in each document were assigned to which topic
## Augment adds information to each observation in the original data. In this case,
## will add a column (".topic") with the topic each term was assigned to within the doc

assignments <- augment(chapters_lda, data = chapters_dtm)
assignments

# Now join with consensus titles to see which were misclassified
assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))

assignments

# Visualize a confusion matrix, showing how often words from one book were assigned
## to another

library(scales)

assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")

# Find most commonly mistaken words
wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

# Alternative LDA implementations -----------------------------------------

# LDA function in topicmodels package is only one implementation of the latent
## Dirichlet allocation algorithm. mallet package is another example. Requires
## non-tokenized docs as inputs, performs tokenization itselt. Also requires
## separate file of stopwords. 

library(mallet) # issue loading mallet due to requirement of Java

# create a vector with one string per chapter (for non-tokenized input)
collapsed <- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word, "'", "")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = " "))

# create an empty file of "stopwords"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100)

# Once model is created, use tidy approach to explore
# word-topic pairs
tidy(mallet_model)

# document-topic pairs
tidy(mallet_model, matrix = "gamma")

# column needs to be named "term" for "augment"
term_counts <- rename(word_counts, term = word)
augment(mallet_model, term_counts)
