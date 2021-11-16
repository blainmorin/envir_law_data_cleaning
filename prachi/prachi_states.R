##loading the states data

library(tidyverse)
library(stringr)
data <- read_csv("cases_coded_prelim_clean1.csv")

states_data <- data %>%
  select(`Location (state) of conflict`)

states_data[is.na(states_data)] <- "none"

##making the data uniform

states_df <- states_data %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "dc", "district of columbia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "washington dc", "district of columbia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "washington, d.c.", "district of columbia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "washington d.c.", "district of columbia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "washington district of columbia", "district of columbia")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "eastern district of tennessee", "tennessee"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "northern district of ohio", "ohio"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "western district of kentucky", "kentucky"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "western district of texas", "texas")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "state of illinois", "illinois")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "west", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "west virginia virginia", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, ", bluefield", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "west virginiaern", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "west virginiawest virginia", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "american west virginia", "west virginia")
  )

