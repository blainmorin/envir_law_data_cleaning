### installing packages -----

#packages

library(tidyverse)
library(stringr)

#load cleaned data with sector and diversity sector counts that Nik created

data <- read_csv("clean_sector_diversity.csv")

#create subset of filtered cases

published_cases <- data %>% 
  filter(!str_detect(cite, 'F. Supp.'))

write.csv(published_cases, "/Users/caseyr/Documents/School/OSU/Rea Environmental Law/Capstone/published_cases_only.csv")
