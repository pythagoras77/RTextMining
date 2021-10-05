
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

path = "C:\\Users\\krf58p\\OneDrive - Ministry of Justice\\Documents\\DASD_Automation\\MOJ_NLP_Group\\Text_Mining_Sessions\\Session_One\\Corpora\\dbWeeklyNote"
filenames <- dir(path, pattern =".txt")

plannames <- paste("C:\\Users\\krf58p\\OneDrive - Ministry of Justice\\Documents\\DASD_Automation\\MOJ_NLP_Group\\Text_Mining_Sessions\\Session_One\\Corpora\\dbWeeklyNote\\", filenames, sep = "")
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

jpeg("C:\\Users\\krf58p\\OneDrive - Ministry of Justice\\Documents\\DASD_Automation\\MOJ_NLP_Group\\Text_Mining_Sessions\\Session_One\\outputs\\comparison_wordcloud.jpg",
     width = 2400,
     height = 1400,
     res = 300)

bing_word_counts %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("gray20", "gray80"), max.words = 100)
  
dev.off()

