#install packages

install.packages("tidyverse")
library(tidyverse)
library(stringr)

#load data

data <- read_csv("cases_coded_prelim_clean1.csv")

#import dataset

data_fed_statutes <- data %>%
  select(
    `Federal Statutes`
  ) %>%
  group_by(
    `Federal Statutes`
  ) %>%
  filter(
    row_number()== 1
  ) %>%
  arrange(
    desc(`Federal Statutes`)
  )


#replace all NA with "none"

data_fed_statutes[is.na(data_fed_statutes)] <- "none"

#separate Federal Statutes into multiple columns

ncols <- max(stringr::str_count(data_fed_statutes$'Federal Statutes', "%")) + 1
colmn <- paste("col", 1:ncols)

fed_statutes_sep <-
  tidyr::separate(
    data = data_fed_statutes,
    col = 'Federal Statutes',
    sep = "%",
    into = colmn,
    remove = FALSE
  )

fed_statutes_sep[is.na(fed_statutes_sep)] <- "none"
