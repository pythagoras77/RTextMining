
#----------------------------------------------------------------------
#
#   
#
#----------------------------------------------------------------------

library("dplyr")
library("tidytext")
library("tidyverse")
library("textdata")
library("reshape2")
library("wordcloud")

#----------------------------------------------------------------------
#
#   this is a way to read in lots of text files in a folder
#
#----------------------------------------------------------------------

path = "/home/pythagoras77/RTextMining/corpora/dbWeeklyNotes"
filenames <- dir(path, pattern =".txt")

plannames <- paste("/home/pythagoras77/RTextMining/corpora/dbWeeklyNotes/", filenames, sep = "")
filedata <- map_chr(plannames, read_file)

wkNotes <- tibble(
  plan = filenames,
  text = filedata
)

wkNotesW <- wkNotes %>%
  unnest_tokens(word, text)

wkNotesW

#----------------------------------------------------------------------
#
#   analysing with bing 
#
#----------------------------------------------------------------------

bing_word_counts <- wkNotesW %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

jpeg("/home/pythagoras77/RTextMining/output/comparison_wordcloud.jpg",
     width = 2400,
     height = 1400,
     res = 300)

bing_word_counts %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("gray20", "gray80"), max.words = 100)
  
dev.off()


