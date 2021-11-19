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
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "d.c.", "district of columbia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "washington dc", "district of columbia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "washington, d.c.", "district of columbia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "washington d.c.", "district of columbia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "washington district of columbia", "district of columbia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "columbia", "district of columbia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "district of district of columbia", "district of columbia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "washington, district of columbia", "district of columbia")
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
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "west virginia virigina", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, ", bluefield", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "west virginiaern", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "west virginiawest virginia", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "american west virginia", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "west virginia great lakes region", "west virginia")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "pacific northwest virginia", "virginia")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "califonia", "california"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "californa", "california"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "califronia", "california"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "california, san francisco", "california")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "district of guam", "guam"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "district of virgin islands", "virgin islands"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "district of washington", "washington")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "gulf of mexico", "louisiana")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "conneticut", "connecticut"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "connnecticut", "connecticut")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "newjersey", "new jersey")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "masachusetts", "massachusetts"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "massachuetts", "massachusetts"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "massachussetts", "massachusetts"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "massachuttes", "massachusetts")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "deleware", "delaware"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "delware", "delaware")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "hawai'i", "hawaii")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "illionois", "illinois")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "louisianna", "louisiana"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "lousiana", "louisiana")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "michagan", "michigan")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "missouris", "missouri")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "new meico", "new mexico")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "new york%south africa", "new york")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "okahoma", "oklahoma")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "pennslyvania", "pennsylvania")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "puerto rice", "puerto rico")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "rhoade island", "rhode island")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "south caroline", "south carolina")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "texas%mexico", "texas")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "nationwide", "united states")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "unknown", "oregon")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "viriginia", "virginia")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "alaksa", "alaska")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "antartica", "antarctica")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "u.s. fish and wildlife service", "alaska"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "clean water act", "west virginia")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "pennsylvania%several", "several")
  )

states_df$`Location (state) of conflict` [7123] <- "north carolina"

states_df$`Location (state) of conflict` [1543] <- "north dakota"

states_df$`Location (state) of conflict` [1185] <- "texas%gulf of mexico"

states_df$`Location (state) of conflict` [1172] <- "new england states"

states_df$`Location (state) of conflict` [3542] <- "new jersey%pennsylvania"


