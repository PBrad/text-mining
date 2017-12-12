############

# Chapter 1 - The Tidy Text Format

############


# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(janeaustenr)
library(ggplot2)
library(scales)
library(gutenbergr)

# Unnest Tokens -----------------------------------------------------------

# Sample text
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

# Convert to data frame with a line variable to track placement
text_df <- data_frame(line = 1:4, text = text)

# Tokenization - break text into one token per line
text_df %>% 
  tidytext:: unnest_tokens(word, # output column into which the text is unnested
                           text) # input column - where the text comes from

    ## Strips punctuation, converts to lower case, retains all other vars (line)


# Working with Jane Austen text -------------------------------------------

# Pulling six Jane Austen novels
original_books <- janeaustenr::austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(), # track row number
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]", # track chapters
                                                 ignore_case = TRUE)))) %>% 
  ungroup()

original_books

# Tokenize
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

# Remove stop words (e.g., "the", "of")
data(stop_words) # from tidytext

tidy_books <- tidy_books %>%
  anti_join(stop_words) # remove matches

# Most common words
tidy_books %>% 
  count(word, sort = TRUE)

# Plot it
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + 
  theme_minimal()


# Gutenbergr Package ------------------------------------------------------

# HG Wells
  # The Time Machine, 
  # The War of the Worlds, 
  # The Invisible Man, 
  # The Island of Doctor Moreau
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort = TRUE)

# Bronte
  # Jane Eyre, 
  # Wuthering Heights, 
  # The Tenant of Wildfell Hall, 
  # Villette, 
  # Agnes Grey

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

count(tidy_bronte, word, sort = TRUE)

# All together now
frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>%
  # Avoid UTF-8 encoded text with underscores recording the underscores as words
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

# Plot 
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

# Correlations
cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)
