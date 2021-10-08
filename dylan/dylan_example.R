### Dylan example

## Hi everyone,

## I think I did this right.


# Load data set

install.packages("tidyverse")
library(tidyverse)

data <- read_csv("cases_coded_prelim_clean1.csv")

data.df <- data %>%
  select(
    court, year, case_date
  ) %>%
  filter(
    year >= 1990
  ) %>%
  mutate(
    year_1 = year+1, 
    year=year_1+1, 
    year_2000= case_when(
      year >= 2018 ~ "recent",
      year < 2000 ~ "old",
      TRUE ~ "between"
    ),
    court_3 = str_c(
      court, "friday", sep = ""
    ),
    friday_7 = str_replace_all(court, "United", "Divided"),
    friday_7 = str_replace_all(court, "USA", "Divided")
  )

data$Aim
data$`Federal Agencies`


data_fed_agency <- data %>%
  select(
    `Federal Agencies`
  ) %>%
  group_by(
    `Federal Agencies`
  ) %>%
  filter(
    row_number()== 1
  ) %>%
  ungroup()


