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

path = "C:\\Users\\krf58p\\OneDrive - Ministry of Justice\\Documents\\DASD_Automation\\MOJ_NLP_Group\\Text_Mining_Sessions\\Session_One\\Corpora\\yjPlans"
filenames <- dir(path, pattern =".txt")

plannames <- paste("C:\\Users\\krf58p\\OneDrive - Ministry of Justice\\Documents\\DASD_Automation\\MOJ_NLP_Group\\Text_Mining_Sessions\\Session_One\\Corpora\\yjPlans\\", filenames, sep = "")
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

# jpeg("C:\\Users\\krf58p\\OneDrive - Ministry of Justice\\Documents\\DASD_Automation\\MOJ_NLP_Group\\Text_Mining_Sessions\\Session_One\\outputs\\wordcloud.jpg",
#     width = 1400,
#     height = 1400,
#     res = 300)

yjPlansW %>% select(word) %>%
  count(word) %>%
  with(wordcloud(word, n,
                 max.words = 75,            
                 random.order = FALSE,
                 rot.per = 0.2,
                 colors = brewer.pal(8, "Set1")))

# dev.off()




