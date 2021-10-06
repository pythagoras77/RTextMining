
#----------------------------------------------------------------------
#
#   
#
#----------------------------------------------------------------------

library("dplyr")
library("plotly")
library("tidytext")
library("tidyverse")
library("textdata")
library("stringr")
library("lubridate")

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
#   extract the date part from the note file name
#
#----------------------------------------------------------------------

wkNotesW$T_DATE <- str_replace_all(wkNotesW$plan, "_|\\.", " ")

fDate <- regex("
               [0-9]{2} # day of month
               \\s{1}
               [\\bJanuary\\b|\\bFebruary\\b|\\bMarch\\b|\\bApril\\b|\\bMay\\b|\\bJune\\b|\\bJuly\\b|\\bAugust\\b|\\bSeptember\\b|\\bOctober\\b|\\bNovember\\b|\\bDecember\\b]+
               \\s{1}
               [0-9]{4}",
               comments = TRUE)

wkNotesW$T_DATE <- str_extract(wkNotesW$T_DATE, fDate)
wkNotesW$R_DATE <- dmy(wkNotesW$T_DATE)
wkNotesW$FLR_DATE <- floor_date(wkNotesW$R_DATE, unit = "1 month")

#----------------------------------------------------------------------
#
#   analysing bing 
#
#----------------------------------------------------------------------

afinn <- wkNotesW %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(FLR_DATE) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  wkNotesW %>% 
    inner_join(get_sentiments("bing"), by = "word") %>%
    mutate(method = "Bing et al."),
  wkNotesW %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative")), by = "word"
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, FLR_DATE, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

x <- bind_rows(afinn, 
               bing_and_nrc) %>%
  ggplot(aes(FLR_DATE, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

ggplotly(x)
