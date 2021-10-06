#----------------------------------------------------------------------
#
#   now with some text that might be something we would text mine
#   here in DASD
#
#----------------------------------------------------------------------

library("dplyr")
library("tidytext")

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

#----------------------------------------------------------------------
#
#   this is a way to read in lots of text files in a folder
#
#----------------------------------------------------------------------

yjFiles <- tibble(plan = c(
  "Lambeth Youth Justice Plan 2019-21v10a.txt",
  "Lancashire YOT - Youth Justice Plan 2019-20.txt",
  "Leicester City Youth Justice Plan 2019-20 290719.txt",
  "Leicestershire YJB plan 2019-20_.txt",
  "Lewisham Youth Justice Plan 2018-21 v3.txt",
  "lincolnshire YJB Plan - final Word Doc v2 Draft (3).txt",
  "Liverpool YOUTH JUSTICE PLAN 2019 update with signature.txt",
  "Manchester Youth Justice Business Plan 2019 20 - v.2 - 3.10.19 final.txt",
  "Medway YOT Plan 2017 to 2020 Final signed.txt",
  "Merton Youth Justice and Crime Prevention Plan 2019-2024 YCPEB chair signed off.txt",
  "MK Youth Justice Plan 2019-20 final 5 August 2019.txt",
  "Monmouthshire YJ Plan Cymru 2019 20 final draft.txt",
  "N Somerset Youth Justice Partnership Plan 19-22 Draft (003) (002).txt",
  "NE Lincs Service Plan 18 - 20 refresh (09-08-2018) authorised DHA (19-20 refresh agreed by YJB).txt",
  "Newham  Youth Justice Plan 2019-20 v4.txt",
  "Newport Youth Justice Strategic Plan 2019 - 2020 V1.txt",
  "Norfolk- Youth Justice Strategic Plan 2019-20 Refresh.txt",
  "North Lincolnshire Youth Justice Plan 2019 - 2021.txt",
  "North Tyneside YJ Plan 2019 to 2021 V3.txt",
  "North Yorkshire  Strategic Youth Justice Plan 2018-19 Review (Amended Final).txt",
  "Northamptonshire Youth Justice Plan 2019-20.txt",
  "Northumberland  YJP (Full)_v3.txt",
  "Nottingham YJ Plan 2019-20 Final 01.08.09.txt",
  "Nottinghamshire 74629  Youth Justice Plan v4.txt",
  "NPT YJEIS Youth Justice Plan 2019-2020.txt"),
  yot = c("Lambeth",
              "Lancashire",
              "Leicester City",
              "Leicestershire",
              "Lewisham",
              "lincolnshire",
              "liverpool",
              "Manchester",
              "Medway",
              "Merton",
              "Milton Keynes",
              "Monmouthshire",
              "North Somerset",
              "North East Lincolnshire",
              "Newham",
              "Newport",
              "Norfolk",
              "North Lincolnshire",
              "North Tyneside",
              "North Yorkshire",
              "Northamptonshire",
              "Northumberland",
              "Nottingham",
              "Nottinghamshire",
              "NPT"))

yjPlans <- yjPlans %>%
  inner_join(yjFiles, by = "plan") %>%
  select(-plan)

yj_bigrams <- yjPlans %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

yj_bigrams %>%
  count(bigram, sort = TRUE)

#----------------------------------------------------------------------
#
#   
#
#----------------------------------------------------------------------

# serarate

yj_bigrams_separated <- yj_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filter with stopwords

yj_bigrams_filtered <- yj_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!str_detect(word1, "[0-9]+")) %>%
  filter(!str_detect(word2, "[0-9]+"))

# count

bigram_counts <- yj_bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

# unite back into bigrams

yj_bigrams_united <- yj_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

yj_bigrams_united %>%
  filter(yot == "Lambeth" | yot == "Medway" | yot == "Merton" | yot == "Milton Keynes") %>%
  count(yot, bigram, sort = TRUE) %>%
  arrange(desc(n)) %>%
  group_by(yot) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(x = n, y = bigram, fill = yot)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~yot, ncol = 2, scales = "free") +
  labs(x = "Count of Words", y = NULL)
