############

# Chapter 7 - Case study: comparing Twitter archives

############

# Case study comparing the Twitter archives of the co-authors
## (Julia and David)

# Getting the data and distribution of tweets -----------------------------

# Note - authors made their tweets available on Github

library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)

tweets_julia <- read_csv("https://github.com/dgrtwo/tidy-text-mining/raw/master/data/juliasilge_tweets.csv")
tweets_dave <- read_csv("https://github.com/dgrtwo/tidy-text-mining/raw/master/data/drob_tweets.csv")

tweets <- bind_rows(tweets_julia %>% 
                      mutate(person = "Julia"),
                    tweets_dave %>% 
                      mutate(person = "David")) %>%
  mutate(timestamp = ymd_hms(timestamp)) # issue with the timestamp

ggplot(tweets, aes(x = timestamp, fill = person)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~person, ncol = 1)

#### Current installation of R has a bug with respect to handling
#### dates on Mac. Creating an alternative version of the 
#### tweets df without the date variable for now

tweets <- bind_rows(tweets_julia %>% 
                      mutate(person = "Julia"),
                    tweets_dave %>% 
                      mutate(person = "David"))

# Word frequencies --------------------------------------------------------

# Tokenizing the data and removing stop words, as well as some
## additional cleanup specific for Twitter

# Remove retweets, links, and clean out certain characters (e.g., &)
## Note that tokenizing with a regex pattern to retain
## hashtags and @ symbols

library(tidytext)
library(stringr)

replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

# Now calculate word frequencies for each person

frequency <- tidy_tweets %>% 
  group_by(person) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>%  # add column of total number of words used by each person
              group_by(person) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency

# df above is tidy but want to plot freq on x andy axes, so spread to get David and Julia
## side by side

library(tidyr)

frequency <- frequency %>% 
  select(person, word, freq) %>% 
  spread(person, freq) %>%
  arrange(Julia, David)

frequency

# Now plot
library(scales)

ggplot(frequency, aes(Julia, David)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

# Comparing word usage ----------------------------------------------------

# identify which words are more/less likely to come from each account using log odds ratio

# Note that they first restrict the analysis to 2016. Due to bug in R date-handling
## (will update later), going to run on the full set

# tidy_tweets <- tidy_tweets %>%
#   filter(timestamp >= as.Date("2016-01-01"),
#          timestamp < as.Date("2017-01-01"))

# Now remove usernames from the word column to avoid list being dominated by people
## one knows that the other does not; then count how many times each person uses
## each word, and keep only words used more than 10 times. Then spread() and calculate
## log odds ratio

word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, person) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(person, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log(David / Julia)) %>%
  arrange(desc(logratio))

# Words equally likely to come from David or Julia's account
word_ratios %>% 
  arrange(abs(logratio))

# Take top 15 most distinctive words from each account and plot
word_ratios %>%
  group_by(logratio < 0) %>% # didn't know you could do this
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (David/Julia)") +
  scale_fill_discrete(name = "", labels = c("David", "Julia"))

# Changes in word use -----------------------------------------------------

# Here's another one that's difficult due to the date bug

# Can examine which words' frequencies have changed the most
## over time (i.e., what words are being used at a higher or
## lower rate as time has passed)

# Creates time-bins using floor_date() from lubridate package
## Specifies "1 month" as the bin size

# Then filters to keep only words that have been used at least
## 30 times

words_by_time <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(timestamp, unit = "1 month")) %>%
  count(time_floor, person, word) %>%
  ungroup() %>%
  group_by(person, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 30)

words_by_time

# Now uses nest() function to make a data frame with a list
## column that contains mini data frames for each word
## This is cool...basically a summary with expandable detail

nested_data <- words_by_time %>% 
  nest(-word, -person)

# Result is a data frame with one row for each person-word
## combination, with a list column that contains data frames,
## one for each combination of person and word

# Now use map() from purrr to model on the little
## data frames within the big data frame, using
## glm() with family = "binomial" for modeling

# This modeling can be thought of "was a given word mentioned
## in a given time bin? Yes or no? How does the count of word 
## mentions depend on time?"

library(purrr)

nested_models <- nested_data %>% 
  mutate(models = map(data, 
                      ~glm(cbind(count, time_total) ~ time_floor,
                           .,
                           family = "binomial")))

# Now we have a column for the modeling results (another
## list column that contains glm objects)

# Now use map() and tidy() to pull out slopes for each of these
## models and find the important ones. Uses an adjustment
## for multiple comparisons (documentation doesn't state
## which correction is default but looks like bonferroni)

library(broom)

slopes <- nested_models %>% 
  unnest(map(models, tidy)) %>% 
  filter(term == "time_floor") %>% 
  mutate(adjusted.p.value = p.adjust(p.value))

# Now find words that changed in frequency at a moderately
## significant level (90%)

top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.1)

top_slopes

# Now visualize results - David first
words_by_time %>%
  inner_join(top_slopes, by = c("word", "person")) %>%
  filter(person == "David") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")

# Now Julia
words_by_time %>%
  inner_join(top_slopes, by = c("word", "person")) %>%
  filter(person == "Julia") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")


# Favorites and retweets --------------------------------------------------

# Examine which words are more likely to be retweeted
## or favorited. Note that the authors needed to create
## an additional dataset via the Twitter API

# Note that my original download of their data from GitHub
## included the retweet data. Df is "tweets"

# Use unnest_tokens() to tidy the dataset. Remove retweets
## and replies to just look at regular tweets posted directly
## by the authors

tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^(RT|@)")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  anti_join(stop_words)

tidy_tweets

# Check number of times tweets were retweeted

totals <- tidy_tweets %>% 
  group_by(person, id) %>% 
  summarise(rts = sum(retweets)) %>% 
  group_by(person) %>% 
  summarise(total_rts = sum(rts))

totals

# Now find median number of retweets per word per person

word_by_rts <- tidy_tweets %>% 
  group_by(id, word, person) %>% 
  summarise(rts = first(retweets)) %>% 
  group_by(person, word) %>% 
  summarise(retweets = median(rts), uses = n()) %>%
  left_join(totals) %>%
  filter(retweets != 0) %>%
  ungroup()

word_by_rts %>% 
  filter(uses >= 5) %>%
  arrange(desc(retweets))

# Plot

word_by_rts %>%
  filter(uses >= 5) %>%
  group_by(person) %>%
  top_n(10, retweets) %>%
  arrange(retweets) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, retweets, fill = person)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ person, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of retweets for tweets containing each word")

# Which words led to more favorites

totals <- tidy_tweets %>% 
  group_by(person, id) %>% 
  summarise(favs = sum(favorites)) %>% 
  group_by(person) %>% 
  summarise(total_favs = sum(favs))

word_by_favs <- tidy_tweets %>% 
  group_by(id, word, person) %>% 
  summarise(favs = first(favorites)) %>% # grabs first value
  group_by(person, word) %>% 
  summarise(favorites = median(favs), uses = n()) %>%
  left_join(totals) %>%
  filter(favorites != 0) %>%
  ungroup()

# Visualize - very similar to retweets results

word_by_favs %>%
  filter(uses >= 5) %>%
  group_by(person) %>%
  top_n(10, favorites) %>%
  arrange(favorites) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, favorites, fill = person)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ person, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of favorites for tweets containing each word")
