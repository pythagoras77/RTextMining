#----------------------------------------------------------------------
#
#   word frequencies for the pq dataset
#
#----------------------------------------------------------------------

library("tidyverse")
library("stringr")

#----------------------------------------------------------------------
#
#   this is one way read in a csv file & account for different
#   encodings
#
#----------------------------------------------------------------------

guess_encoding("/home/pythagoras77/RTextMining/corpora/pqs/Parliament_Data_Answered_PQs_v1.csv")

pqData <- read_csv("/home/pythagoras77/RTextMining/corpora/pqs/Parliament_Data_Answered_PQs_v1.csv",
                     col_types = cols(.default = "c"),
                     locale = locale(encoding = "UTF-8", asciify = TRUE)
  )

#----------------------------------------------------------------------
#
#   clean/process the text to remove the html markup
#
#----------------------------------------------------------------------

pqData$pqAnswer <- str_replace_all(pqData$pqAnswer, "<.*?>", "")

#----------------------------------------------------------------------
#
#
#----------------------------------------------------------------------

pqData1 <- pqData %>%
  select(pqAnswer) %>%
  unnest_tokens(word, pqAnswer)
                
pqData1

pqData1 %>%
  count(word, sort = TRUE) %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#----------------------------------------------------------------------
#
#   stop_words
#
#----------------------------------------------------------------------

stop_words

#----------------------------------------------------------------------
#
#   removing stop_words
#
#----------------------------------------------------------------------

pqData2 <- pqData %>%
  unnest_tokens(word, pqAnswer) %>%
  anti_join(stop_words, by = "word")

pqData2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

