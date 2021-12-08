#11/5/21

#packages

library(tidyverse)
library(stringr)

#load data

data <- read_csv("cases_coded_prelim_clean1.csv")

#create defendant type dataframe and arrange to see unique values

data_defendant_type <- data

#make blanks none

data_defendant_type[is.na(data_defendant_type)] <- "none"

#fixing incorrect codes and typos 

defendant_type_df <- data_defendant_type %>%
  mutate(
    `Defendant Types` = str_replace_all(`Defendant Types`,"city","local"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"city%local%industry","local%industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"civiv_assn","civic_assn"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"corporation","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"fed%fed","fed"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indsutry","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"ipublic_org","public_org"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"fed%local%local","fed%local"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"federal","fed"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indididual","individual"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indivudal","individual"),
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
    `Defendant Types` = str_replace_all(`Defendant Types`,"industry\\$individual","industry%individual"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"industy","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indvidual","individual"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"insutry","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"inudstry","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"industrt","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"public org","public_org"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"university","public_org"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"local%local%industry","local%industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"local%local%local%local%fed","local%fed"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"local%local%state%trade_assn","local%state%trade_assn"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"militay","military"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"ngo\\?","ngo"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"unclear","other"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"county","local"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"seek damages","fed"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"sought recovery costs for damages","local")
  )

#defendant types are now cleaned

#create final dataframe and rename column for binding data

defendant_types_df_c <- defendant_type_df %>%
  select(
    `ID`, `Defendant Types`
    ) %>%
  rename(
    `Defendant Types_c` = `Defendant Types`)


