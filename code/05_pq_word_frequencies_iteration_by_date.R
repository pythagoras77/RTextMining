
library("dplyr")
library("tidytext")
library("lubridate")
library("stringr")

#----------------------------------------------------------------------
#
#   this is one way read in a csv file & account for different
#   encodings
#
#----------------------------------------------------------------------

guess_encoding("C:\\Users\\krf58p\\OneDrive - Ministry of Justice\\Documents\\DASD_Automation\\MOJ_NLP_Group\\Text_Mining_Sessions\\Session_One\\Corpora\\pqs\\Parliament_Data_Answered_PQs_v1.csv")

pqData <- read_csv("C:\\Users\\krf58p\\OneDrive - Ministry of Justice\\Documents\\DASD_Automation\\MOJ_NLP_Group\\Text_Mining_Sessions\\Session_One\\Corpora\\pqs\\Parliament_Data_Answered_PQs_v1.csv",
                   col_types = cols(.default = "c"),
                   locale = locale(encoding = "UTF-8", asciify = TRUE)
)

pqData$pqANS_DATE <- dmy(pqData$pqDateofAnswer)
pqData$pqFLR_DATE <- floor_date(pqData$pqANS_DATE, unit = "1 month")

# ONE

# pqData$pqAnswer <- str_replace_all(pqData$pqAnswer, "<.*?>", "")

# TWO

# pqData$pqAnswer <- str_replace_all(pqData$pqAnswer, "[0-9]+", "")

# THREE

# custom_stop_words <- bind_rows(
#  data_frame(word = c("government", "uk"), lexicon = c("custom")),
#  stop_words
#  )

pq_words <- pqData %>%
  select(pqFLR_DATE, pqAnswer) %>%
  unnest_tokens(word, pqAnswer)
  
pq_words <- pq_words %>%
  anti_join(stop_words, by = "word") %>%
  count(pqFLR_DATE, word, sort = TRUE) %>%
  ungroup()

pq_total_words <- pq_words %>%
  group_by(pqFLR_DATE) %>%
  summarize(total = sum(n))

#----------------------------------------------------------------------
#
#   using proprotion of total words in book to standarise word
#   frequencies
#
#----------------------------------------------------------------------

pq_words <- pq_words %>%
  left_join(pq_total_words, by = "pqFLR_DATE") %>%
  mutate(perc_word = (n/total) * 100)

pq_words %>%
  arrange(desc(perc_word)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(pqFLR_DATE) %>%
  top_n(10, perc_word) %>%
  ungroup() %>%
  ggplot(aes(x = word, y = perc_word, fill = pqFLR_DATE)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Perc. Word") +
  facet_wrap(~pqFLR_DATE, ncol = 2, scales = "free") +
  coord_flip()
