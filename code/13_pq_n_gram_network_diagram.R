#----------------------------------------------------------------------
#
#   now with some text that might be something we would text mine
#   here in DASD
#
#----------------------------------------------------------------------

library("dplyr")
library("tidytext")
library("tidyverse")
library("ggraph")
library("igraph")

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

#----------------------------------------------------------------------
#
#   clean the text to remove the html markup
#
#----------------------------------------------------------------------

pqData$pqAnswer <- str_replace_all(pqData$pqAnswer, "<.*?>", "")

pq_bigrams <- pqData %>%
  select(pqBody, pqAnswer) %>%
  unnest_tokens(bigram, pqAnswer, token = "ngrams", n = 2)

pq_bigrams %>%
  count(bigram, sort = TRUE)

#----------------------------------------------------------------------
#
#   
#
#----------------------------------------------------------------------

# serarate

pq_bigrams_separated <- pq_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filter with stopwords

pq_bigrams_filtered <- pq_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!str_detect(word1, "[0-9]+")) %>%
  filter(!str_detect(word2, "[0-9]+"))

# count

pq_bigram_counts <- pq_bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

# unite back into bigrams

pq_bigrams_united <- pq_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

jpeg("C:\\Users\\krf58p\\OneDrive - Ministry of Justice\\Documents\\DASD_Automation\\MOJ_NLP_Group\\Text_Mining_Sessions\\Session_One\\outputs\\network_diagram.jpg", 
     width = 2400, 
     height = 1200,
     res = 180) 

pq_bigram_counts %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
  geom_node_point(colour = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

dev.off() 

#----------------------------------------------------------------------
#
#   
#
#----------------------------------------------------------------------

g <- pq_bigram_counts %>%
  filter(n >= 10) %>%
  graph_from_data_frame()

Degree <- degree(g)

DG <- tibble(
  word = names(Degree),
  DGV = Degree
)

DG %>% arrange(desc(DGV)) %>%
  top_n(10, DGV)
