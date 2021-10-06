#----------------------------------------------------------------------
#
#   now with some text that might be something we would text mine
#   here in DASD
#
#----------------------------------------------------------------------

library("tidyverse")
library("tidytext")

#----------------------------------------------------------------------
#
#   this is a way to read in many txt files in a folder
#
#----------------------------------------------------------------------

path = "/home/pythagoras77/RTextMining/corpora/dbWeeklyNotes"
filenames <- dir(path, pattern =".txt")

plannames <- paste("/home/pythagoras77/RTextMining/corpora/dbWeeklyNotes/", filenames, sep = "")
filedata <- map_chr(plannames, read_file)

yjPlans <- tibble(
  plan = filenames,
  text = filedata
)

yjPlansW1 <- yjPlans %>%
  unnest_tokens(word, text)

yjPlansW1

#----------------------------------------------------------------------
#
#   use the tidy_text stopwords to remove the unimportant words
#
#----------------------------------------------------------------------

stop_words

yjPlansW1 <- yjPlansW1 %>%
  anti_join(stop_words, by = "word")

yjPlansW1

#----------------------------------------------------------------------
#
#   now we can visualise the word frequencies using ggplot
#
#----------------------------------------------------------------------

yjPlansW1 %>%
  count(word, sort = TRUE) %>%
  filter(n > 40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#----------------------------------------------------------------------
#
#   create your own custom stop words
#
#----------------------------------------------------------------------

custom_stopwords <- bind_rows(
  data_frame(word = c("week"), lexicon = c("custom")),
  stop_words
  )

yjPlansW2 <- yjPlansW %>%
  anti_join(custom_stopwords, by = "word")

yjPlansW2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()





