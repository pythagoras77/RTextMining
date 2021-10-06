
library("dplyr")
library("tidytext")


text <- c("Ministry of Justice publishes a range of statistics",
          "relating to the operation of the criminal and",
          "civil justice systems, on aspects of criminal justice policy",
          "and on other areas of the department's responsibility.")

text_df <- tibble(line = 1:4, 
                  text = text)

#----------------------------------------------------------------------
#
#     unnest_tokes has two basic arguements:
#       the output column name the text will be unnested into [word]
#       the column name of the source iof text to be unnested [text]
#       the default tokenisation for unnest_tokens is single words
#
#----------------------------------------------------------------------

text_df %>%
  unnest_tokens(word, text)

text_df <- text_df %>%
  unnest_tokens(word, text)



