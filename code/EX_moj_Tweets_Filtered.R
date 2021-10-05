#----------------------------------------------------------------------
#
#   libraries
#
#----------------------------------------------------------------------

library("tidytext")

#----------------------------------------------------------------------
#
#   read in the tweets   
#
#----------------------------------------------------------------------

guess_encoding("C:\\Users\\krf58p\\OneDrive - Ministry of Justice\\Documents\\DASD_Automation\\MOJ_NLP_Group\\Text_Mining_Sessions\\Session_One\\Corpora\\tweets\\MOJ_Tweets.csv")

mojTweets <- read_csv("C:\\Users\\krf58p\\OneDrive - Ministry of Justice\\Documents\\DASD_Automation\\MOJ_NLP_Group\\Text_Mining_Sessions\\Session_One\\Corpora\\tweets\\MOJ_Tweets.csv",
                   col_types = cols(.default = "c"),
                   locale = locale(encoding = "UTF-8", asciify = TRUE)
)

#----------------------------------------------------------------------
#
#  
#
#----------------------------------------------------------------------

replace_reg1 <- "https://t.co/[A-Za-z\\d]+|"
replace_reg2 <- "http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
replace_reg <- paste0(replace_reg1, replace_reg2)

tidy_tweets <- mojTweets %>%
  select(created_at, text) %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  mutate(word = str_replace_all(word, "'", "")) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"),
         !str_detect(word, "^@"),
         !str_detect(word, "^#"),
         !word %in% str_remove_all(stop_words$word, "'"),
         !str_detect(word, "^[0-9]+"))

tidy_tweets$TWEET_CREATED <- strptime(tidy_tweets$created_at, "%a %b %d %H:%M:%S %z %Y", tz = "GMT")

tidy_tweets$FINAL_DATE <- make_date(year = year(tidy_tweets$TWEET_CREATED),
                                    month = month(tidy_tweets$TWEET_CREATED),
                                    day = day(tidy_tweets$TWEET_CREATED))

#----------------------------------------------------------------------
#
#  
#
#----------------------------------------------------------------------

