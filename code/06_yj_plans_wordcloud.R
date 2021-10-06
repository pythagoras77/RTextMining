#----------------------------------------------------------------------
#
#   now with some text that might be something we would text mine
#   here in DASD
#
#----------------------------------------------------------------------

library("dplyr")
library("tidytext")
library("wordcloud")

#----------------------------------------------------------------------
#
#   this is a way to read in lots of text files in a folder
#
#----------------------------------------------------------------------

path = "/home/pythagoras77/RTextMining/corpora/yjPlans2019_20"
filenames <- dir(path, pattern =".txt")

plannames <- paste("/home/pythagoras77/RTextMining/corpora/yjPlans2019_20/", filenames, sep = "")
filedata <- map_chr(plannames, read_file)

yjPlans <- tibble(
  plan = filenames,
  text = filedata
)

yjPlansW <- yjPlans %>%
  unnest_tokens(word, text)

yjPlansW

yjPlansW <- yjPlansW %>%
  anti_join(stop_words, by = "word")

#----------------------------------------------------------------------
#
#   now we can visualise the word frequencies using a wordcloud
#
#----------------------------------------------------------------------

jpeg("/home/pythagoras77/RTextMining/output/wordcloud.jpg",
     width = 2400,
     height = 1800,
     res = 300)

yjPlansW %>% select(word) %>%
  count(word) %>%
  with(wordcloud(word, n,
                 max.words = 75,            
                 random.order = FALSE,
                 rot.per = 0.2,
                 colors = brewer.pal(8, "Set1")))

dev.off()




