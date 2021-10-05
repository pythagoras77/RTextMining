#--------------------------------------------------------------------------------
#
#
#
#--------------------------------------------------------------------------------

library('tidytext')
library('tidyverse')

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
    yot = filenames,
    text = filedata
)

#----------------------------------------------------------------------
#
#   this is a way to read in lots of text files in a folder
#
#----------------------------------------------------------------------

yjFiles <- tibble(yot = c(
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
    yotName = c("Lambeth",
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
    inner_join(yjFiles, by = "yot")

#----------------------------------------------------------------------
#
#
#
#----------------------------------------------------------------------

yjPlans <- yjPlans %>%
    unnest_tokens(word, text)

yjPlans <- yjPlans %>%
    anti_join(stop_words, by = "word")

#--------------------------------------------------------------------------------
#
#
#
#--------------------------------------------------------------------------------

plan_words <- yjPlans %>%
    select(-yot) %>%
    count(yotName, word, sort = TRUE) %>%
    ungroup()

plan_tot_words <- plan_words %>%
    group_by(yotName) %>%
    summarize(totWords = sum(n),
              cntWords = n())

#--------------------------------------------------------------------------------
#
#
#
#--------------------------------------------------------------------------------

plan_tot_words <- plan_tot_words %>%
    mutate(lexDiversity = totWords/cntWords)

plan_tot_words %>%
    arrange(desc(lexDiversity))

plan_tot_words %>%
    arrange(lexDiversity)

#--------------------------------------------------------------------------------
#
#
#
#--------------------------------------------------------------------------------

plan_tot_words %>% 
    ggplot(aes(x = lexDiversity)) +
    geom_histogram(binwidth = 0.5, fill = "white", colour = "black") +
    theme_classic() +
    stat_function(fun = function(x) dnorm(x, mean = mean(plan_tot_words$lexDiversity), sd = sd(plan_tot_words$lexDiversity)) * 12.5, color = "darkred")

