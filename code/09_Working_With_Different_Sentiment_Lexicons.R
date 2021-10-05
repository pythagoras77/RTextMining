#----------------------------------------------------------------------
#
#   r libraries
#
#----------------------------------------------------------------------

library("dplyr")
library("tidytext")
library("tidyverse")
library("textdata")

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
#   different ways of obtaining the sentiment scoring
#
#----------------------------------------------------------------------

#----------------------------------------------------------------------
#
#   analysing bing 
#
#----------------------------------------------------------------------

get_sentiments("bing")

wkNotesW_bing <- wkNotesW %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

wkNotesW_bing

#----------------------------------------------------------------------
#
#   analysing afinn
#
#----------------------------------------------------------------------

get_sentiments("afinn")

wkNotesW_afinn <- wkNotesW %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  summarise(sentiment = sum(value))

wkNotesW_afinn

#----------------------------------------------------------------------
#
#   analysing nrc
#
#----------------------------------------------------------------------

get_sentiments("nrc")

wkNotesW_nrc <- wkNotesW %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

wkNotesW_nrc
  
