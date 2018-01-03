############

# Chapter 8 - Case study: mining NASA metadata

############

# Uses metadata on NASA datasets - includes info like 
## title of the dataset, description field, orgs responsible
## within NASA, keywords, etc. Metadata for all NASA
## datasets are available publicly online

# Case study treats the metadata as a text dataset. Will use
## word co-occurences and correlations, tf-idf, and topic modeling
## to examine connections between datasets.

# How data is organized at NASA -------------------------------------------

# Download the JSON file of metadata and check out the names
## of what's stored within

library(jsonlite)
metadata <- fromJSON("https://data.nasa.gov/data.json")
names(metadata$dataset)

# Check out the title, description, and keywords 

class(metadata$dataset$title) # character

class(metadata$dataset$description) # character

class(metadata$dataset$keyword) # list

# Wrangling and tidying the data ------------------------------------------

# Creating separate tidy data frames for title, desc, and kw

library(dplyr)

nasa_title <- data_frame(id = metadata$dataset$`_id`$`$oid`,
                         title = metadata$dataset$title)

nasa_title

nasa_desc <- data_frame(id = metadata$dataset$`_id`$`$oid`, 
                        desc = metadata$dataset$description)

nasa_desc %>% 
  select(desc) %>% 
  sample_n(5) # nice - samples n rows from a table

# for the keywords, need unnest() bc they're in a list-column

library(tidyr)

nasa_keyword <- data_frame(id = metadata$dataset$`_id`$`$oid`, 
                           keyword = metadata$dataset$keyword) %>%
  unnest(keyword)

nasa_keyword

# result is one row for each keyword and multiple rows for each
## dataset bc a dataset can have multiple keywords

# Now tokenize the title and description fields, as well as
## remove stop words. Will keep stop words in the keywords
## bc they are human-assigned

library(tidytext)

nasa_title <- nasa_title %>% 
  unnest_tokens(word, title) %>% 
  anti_join(stop_words)

nasa_desc <- nasa_desc %>% 
  unnest_tokens(word, desc) %>% 
  anti_join(stop_words)

nasa_title

nasa_desc

# Some initial exploration ------------------------------------------------

# Most common words in the NASA dataset titles

nasa_title %>% 
  count(word, sort = TRUE)

# Now descriptions

nasa_desc %>% 
  count(word, sort = TRUE)

# Create a custom stop words list to remove some less meaningful
## text (e.g., digits)

my_stopwords <- data_frame(word = c(as.character(1:10),
                                    "v1", "v03", "l2", "l3", "l4", "v5.2.0", 
                                    "v003", "v004", "v005", "v006", "v7"))

nasa_title <- nasa_title %>% 
  anti_join(my_stopwords)
nasa_desc <- nasa_desc %>% 
  anti_join(my_stopwords)

# Most common keywords

nasa_keyword %>% 
  group_by(keyword) %>% 
  count(sort = TRUE)

# Change all keywords to upper case to avoid dups

nasa_keyword <- nasa_keyword %>% 
  mutate(keyword = toupper(keyword))


# Word co-occurrences and correlations -------------------------------------

# First examining words that commonly occur togetehr, then
## examine word networks to determine which datasets are
## related to each other

# Networks of descriptions and title words --------------------------------

# Use pairwise_count() from widyr to count how many times
## pairs of words occur together in title or desc fields

library(widyr)

title_word_pairs <- nasa_title %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

title_word_pairs

desc_word_pairs <- nasa_desc %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

desc_word_pairs

# Plot networks of co-occurring words using ggraph

library(ggplot2)
library(igraph)
library(ggraph)

set.seed(1234)
title_word_pairs %>%
  filter(n >= 250) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# We see some clear clusters in the title words

set.seed(1234)
desc_word_pairs %>%
  filter(n >= 5000) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# Not so much in the descriptions - may be a better candidate
## for tf-idf

# Networks of keywords ----------------------------------------------------

# See which keywords commonly occur together in the same
## datasets

keyword_pairs <- nasa_keyword %>% 
  pairwise_count(keyword, id, sort = TRUE, upper = FALSE)

keyword_pairs

set.seed(1234)
keyword_pairs %>%
  filter(n >= 700) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# Clear clustering in the keywords

# To look at this another way, check correlations among
## keywords, to see those that are more likely to occur together
## than with other keywords in a description field

keyword_cors <- nasa_keyword %>% 
  group_by(keyword) %>%
  filter(n() >= 50) %>%
  pairwise_cor(keyword, id, sort = TRUE, upper = FALSE)

keyword_cors

# Some keywords always occur together (coefficient of 1)
## Could remove these as redundant

# Visualize keyword correlations network

set.seed(1234)
keyword_cors %>%
  filter(correlation > .6) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# I think the data have changed a fair amount since the authors
## ran their analysis as there is a ton of noise in my version
## while there's looks much simpler. Still get some clear
## clusters though.

# Authors note that the correlation network looks much different
## than the co-occurrence network bc the co-occurrence 
## network asks a question about which keyword pairs occur
## together most often, and the correlation network asks
## a question about which keywords occur together more often 
## than with other keywords

# Also notes that the network structure can be extracted
## for further analysis with graph_from_data_frame()

# Calculating tf-idf for the description fields ---------------------------

# Since description fields were dominated by a few common
## words (data, global, resolution), this is a good fit for
## tf-idf to find characteristic words for individual description
## fields. This is term frequency * inverse document frequency
## to identify words that are especially important within a 
## collection of documents

# What is tf-idf for description field words? -----------------------------

desc_tf_idf <- nasa_desc %>% 
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)

# Highest tf-idf words in the description fields

desc_tf_idf %>% 
  arrange(-tf_idf)

# Notes that n and term frequency are both equal to 1 for
## the top tf-idf words, meaning that the description fields
## only had one word in them. In such cases, the tf-idf
## algorightm will think this is a very important word.
## May be helpful to throw out description fields containing
## only a single word

# Connecting description fields to keywords -------------------------------

# We have a df of words in the description fields that have
## high tf-idf, as well as labels for those descriptions
## in the keywords.

# Full join of the keyword and description words with 
## tf-idf

desc_tf_idf <- full_join(desc_tf_idf, nasa_keyword, by = "id")

# Filter for top 15 words for each keyword, then plot
desc_tf_idf %>% 
  filter(!near(tf, 1)) %>%
  filter(keyword %in% c("SOLAR ACTIVITY", "CLOUDS", 
                        "SEISMOLOGY", "ASTROPHYSICS",
                        "HUMAN HEALTH", "BUDGET")) %>%
  arrange(desc(tf_idf)) %>%
  group_by(keyword) %>%
  distinct(word, keyword, .keep_all = TRUE) %>%
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, tf_idf, fill = keyword)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~keyword, ncol = 3, scales = "free") +
  coord_flip() +
  labs(title = "Highest tf-idf words in NASA metadata description fields",
       caption = "NASA metadata from https://data.nasa.gov/data.json",
       x = NULL, y = "tf-idf")

# Topic modeling ----------------------------------------------------------

# Alternative approach to finding what the description
## fields are about - use topic modeling. Model each document
## (description field) as a mixture of topics and each topic
## as a mixture of words. Use latent Dirichlet allocation (LDA)

# Casting to a document-term matrix ---------------------------------------

# Need to convert to DocumentTermMatrix for the tm package
## Rows correspond to documents (description texts) and columns
## correspond to terms (i.e., words).

# First clean up the text to remove nonsense words leftover
## from HTML or other character encoding.

my_stop_words <- bind_rows(stop_words, 
                           data_frame(word = c("nbsp", "amp", "gt", "lt",
                                               "timesnewromanpsmt", "font",
                                               "td", "li", "br", "tr", "quot",
                                               "st", "img", "src", "strong",
                                               "http", "file", "files",
                                               as.character(1:12)), 
                                      lexicon = rep("custom", 30)))

word_counts <- nasa_desc %>%
  anti_join(my_stop_words) %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

word_counts

# Now cast to DTM
desc_dtm <- word_counts %>%
  cast_dtm(id, word, n)

desc_dtm

# Commence topic modeling -------------------------------------------------

# Use topicmodels package to create an LDA model

# Sounds like the authors used trial and error to determine
## how many topics to specify, commenting that it's similar
## to k-means clustering where you don't know ahead of time.
## Notes that they tried 8-64 topics but that they don't get
## much additional traction after 24

library(topicmodels)

# be aware that running this model is time intensive
desc_lda <- LDA(desc_dtm, k = 24, control = list(seed = 1234))
desc_lda

# Interpreting the topic model --------------------------------------------

# Tidy the results of the model

tidy_lda <- tidy(desc_lda)

tidy_lda

# Beta tells probability of a term belonging to a given topic

# Examine top 10 terms for each topic

top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

# Visualize
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")

# Now explore what topics are associated with what description
## fields by looking at gamma

lda_gamma <- tidy(desc_lda, matrix = "gamma")

lda_gamma

# Visualize how the probabilities are distributed
ggplot(lda_gamma, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))

# Distribution shows that there are many documents that do 
## not belong in each topic (gamma near 0) and some documents
## that do belong in each topic (gamma near 1). This indicates
## that documents are being well discriminated as belonging
## to a topic or not.

# Look at how the probabilities are distributed within each
## topic

ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))

# Notes that these distribution plots helped the authors
## settle on the number of topics (24). When trying higher
## than 24, gamma distributions were looking very flat
## toward 1 indicating that documents were not being sorted
## into topics very well.

# Connecting topic modeling with keywords ---------------------------------

# full_join() the topic models with the human-tagged keywords
## to see which keywords are associated with which topic

lda_gamma <- full_join(lda_gamma, nasa_keyword, by = c("document" = "id"))

lda_gamma

# Filter by a gamma threshold (0.9)

top_keywords <- lda_gamma %>% 
  filter(gamma > 0.9) %>% 
  count(topic, keyword, sort = TRUE)

top_keywords

# Visualize top keywords for each topic

top_keywords %>%
  group_by(topic) %>%
  top_n(5, n) %>%
  group_by(topic, keyword) %>%
  arrange(desc(n)) %>%  
  ungroup() %>%
  mutate(keyword = factor(paste(keyword, topic, sep = "__"), 
                          levels = rev(paste(keyword, topic, sep = "__")))) %>%
  ggplot(aes(keyword, n, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Top keywords for each LDA topic",
       x = NULL, y = "Number of documents") +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")

# The plot answers "For the datasets with description fields 
## that have a high probability of belonging to a given topic, 
## what are the most common human-assigned keywords?"