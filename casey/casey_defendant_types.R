#11/5/21

#packages

library(tidyverse)
library(stringr)

#load data

data <- read_csv("cases_coded_prelim_clean1.csv")

#create defendant type dataframe and arrange to see unique values

data_defendant_type <- data %>%
  select(
    `Defendant Types`
  ) %>%
  group_by(
    `Defendant Types`
  ) %>%
  filter(
    row_number()== 1
  ) %>%
  ungroup() %>%
  arrange(
    desc(`Defendant Types`)
  )

#make blanks none

data_defendant_type[is.na(data_defendant_type)] <- "none"

#separate multiple variables for identifying data that needs fixed

ncols <- max(stringr::str_count(data_defendant_type$'Defendant Types', "%")) + 1
colmn <- paste("col", 1:ncols)

sep_defendant_types <-
  tidyr::separate(
    data = data_defendant_type,
    col = 'Defendant Types',
    sep = "%",
    into = colmn,
    remove = FALSE
  )

#make separate blank columns none

sep_defendant_types[is.na(sep_defendant_types)] <- "none"

#fixing incorrect codes and typos 11/5/21

defendant_type_df <- data_defendant_type %>%
  mutate(
    `Defendant Types` = str_replace_all(`Defendant Types`,"city","local"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"city%local%industry","local%industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"corporation","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"fed%fed","fed"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indsutry","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"ipublic_org","public_org"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"fed%local%local","fed%local"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"federal","fed"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indididual","individual"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indistry","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indivdual","individual"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"individual%industry%industry","individual%industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"individuals","individual"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indiviudal","individual"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indsustry","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indsutry","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"industruy","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"industry%industry%industry%industry","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"industry%local%industry","industry%local"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"publi_org","public_org"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"publi_org","public_org"),
  )

#re-run separate multiple variables for identifying data that needs fixed

ncols <- max(stringr::str_count(data_defendant_type$'Defendant Types', "%")) + 1
colmn <- paste("col", 1:ncols)

sep_defendant_types_2 <-
  tidyr::separate(
    data = defendant_type_df,
    col = 'Defendant Types',
    sep = "%",
    into = colmn,
    remove = FALSE
  )
sep_defendant_types_2[is.na(sep_defendant_types_2)] <- "none"  
    


