#----------------------------------------------------------------------
#
#   word frequencies for the pq dataset
#
#----------------------------------------------------------------------

library("dplyr")
library("tidytext")
library("janeaustenr")

book_words <- austen_books() %>%
  unnest_tokens(word, text)
  
book_words <- book_words %>%
  anti_join(stop_words, by = "word") %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

book_words

total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))

total_words

#----------------------------------------------------------------------
#
#   using proprotion of total words in book to standarise word
#   frequencies
#
#----------------------------------------------------------------------

book_words <- book_words %>%
  left_join(total_words, by = "book") %>%
  mutate(perc_word = (n/total) * 100)

book_words

book_words %>%
  arrange(desc(perc_word)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(10, perc_word) %>%
  ungroup() %>%
  ggplot(aes(x = word, y = perc_word, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Perc. Word") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

