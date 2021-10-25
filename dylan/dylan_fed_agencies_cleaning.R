install.packages("tidyverse")
library(tidyverse)

data <- read_csv("cases_coded_prelim_clean1.csv")

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
  ungroup() %>%
  arrange( 
    desc(`Federal Agencies`)
  )



## 10/16/21

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
  ungroup() %>%
  arrange( 
    desc(`Federal Agencies`)
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "animal and plant health inspection services", "aphis"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "animal and plant health inspection service", "aphis")
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "appalachian regional commission", "arc"),
  ) %>% 
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "atomic energy commission", "aec"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "acoe", "acoe"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "aoce", "acoe"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us army corps of engineers", "acoe"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "usacoe", "acoe"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "usace", "acoe"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us acoe", "acoe"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "army corps of engineers", "acoe"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "army corp of engineers", "acoe"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "army acoe", "acoe"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states acoe", "acoe"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "corp of engineers", "acoe"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "corps of engineers", "acoe"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states army corp. of engineers", "acoe"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. army corps of engineers", "acoe"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states army acoe", "acoe")
  ) %>% 
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "blm", "blm"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. blm", "blm"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of land management", "blm"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of land management", "blm"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of land management", "blm"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of land managment", "blm"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states blm", "blm"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. blm", "blm"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal land management", "blm"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of indian affairs", "bif"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of indian affairs", "bif"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of indian affairs", "bif"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of reclamation", "bor"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us bureau of reclamation", "bor"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of reclamation", "bor"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bor", "bor"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of reclaimation", "bor"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of ocean energy management", "boem"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of ocean energy management", "boem"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of ocean energy management", "boem"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of safety and environmental enforcement", "bsee"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of safety and environmental enforcement", "bsee"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of safety and environmental enforcement", "bsee"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "environmental protection agency", "epa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. epa", "epa"),
  ) %>%
  mutate( 
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal food and drug administration", "fda"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "food and drug administration", "fda"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal aviation administration", "faa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal highway adminstration", "fha"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal housing finance agency", "fhfa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal transit administration", "fta"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us forest service", "fs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. forest service", "fs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "usfs", "fs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "forest service", "fs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "forest serive", "fs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states forest service", "fs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states fs", "fs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national fs", "fs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. forest management", "fs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. national fs", "fs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "untied states fs", "fs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united stated fs", "fs"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. fish and wildlife service", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "usfws", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "usfwl", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us fws", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us fish and wildlife service", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us fish and wildilfe service", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states fish and wildlife service", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "fish and wildlife service", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "fish and wildlife services", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "fish & wildlife service", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us fish and wildlife", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "wildlife services", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "fwss", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. fish and wildlifer service", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. wildlife service", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united state fish and wildlife service", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states fish and wildlife service, et al.", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states fish and wildlife servie", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united state fws", "fws"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "general service administration", "gsa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national aeronautics and space administration", "nasa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national marine fisheries service", "nmfs"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national oceanic and atmospheric administration", "noaa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national park service", "nps"),
  )












