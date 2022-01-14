### load data and make blanks none -----------

library(tidyverse)
library(stringr)

#load data

data <- read_csv("cases_coded_prelim_clean1.csv")

#create outcome dataframe and arrange to see unique values

data_ton <- data %>%
  select(
    `Type of Nature`
  ) %>%
  group_by(
    `Type of Nature`
  ) %>%
  filter(
    row_number()== 1
  ) %>%
  ungroup() %>%
  arrange(
    desc(`Type of Nature`)
  )

#make blanks none

data_ton[is.na(data_ton)] <- "none"

### fix typos and incorrect codes **Type of Nature** -------

ton_non_c <- data_ton %>%
  mutate(
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-envrionmental","non-environmental"),
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-environemntal","non-environmental"),
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-environemental\\?","non-environmental"),
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-environemental","non-environmental"),
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-envirnomental","non-environmental"),
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-enviornmental","non-environmental"),
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-enviornemental","non-environmental"),
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-enironmental","non-environmental")
  )
