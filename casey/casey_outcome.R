#11/10/21

#packages

library(tidyverse)
library(stringr)

#load data

data <- read_csv("cases_coded_prelim_clean1.csv")

#create outcome dataframe and arrange to see unique values

data_outcome <- data %>%

#make blanks none

data_outcome[is.na(data_outcome)] <- "none"

#fix typos and incorrect codes

outcome_df <- data_outcome %>%
  mutate(
    `Outcome` = str_replace_all(`Outcome`,"insurance cost","plaintiff"),
    `Outcome` = str_replace_all(`Outcome`,"environmental nature is second at best","none"),
    `Outcome` = str_replace_all(`Outcome`,"plaitiff","plaintiff"),
    `Outcome` = str_replace_all(`Outcome`,"plaintiffs","plaintiff"),
    `Outcome` = str_replace_all(`Outcome`,"plaintifff","plaintiff"),
    `Outcome` = str_replace_all(`Outcome`,"plaintff","plaintiff"),
    `Outcome` = str_replace_all(`Outcome`,"plainitff","plaintiff"),
    `Outcome` = str_replace_all(`Outcome`,"defenfant","defendant"),
    `Outcome` = str_replace_all(`Outcome`,"defendent","defendant"),
    `Outcome` = str_replace_all(`Outcome`,"defendants","defendant"),
    `Outcome` = str_replace_all(`Outcome`,"defenant","defendant"),
    `Outcome` = str_replace_all(`Outcome`,"defedant","defendant"),
    `Outcome` = str_replace_all(`Outcome`,"denied","defendant"),
    `Outcome` = str_replace_all(`Outcome`,"unsure","unknown"),
    `Outcome` = str_replace_all(`Outcome`,"unclear","unknown"),
    `Outcome` = str_replace_all(`Outcome`,"industry%individual","mixed"),
    `Outcome` = str_replace_all(`Outcome`,"neither all motions denied","mixed"),
    `Outcome` = str_replace_all(`Outcome`,"neither all motions defendant","mixed"),
    `Outcome` = str_replace_all(`Outcome`,"neither","mixed"),
    `Outcome` = str_replace_all(`Outcome`,"neither\\?","mixed"),
    `Outcome` = str_replace_all(`Outcome`,"mixed\\?","mixed")
  )
    
#outcome is cleaned