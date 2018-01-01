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


