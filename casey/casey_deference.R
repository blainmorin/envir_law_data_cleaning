#packages

library(tidyverse)
library(stringr)

#load data

data <- read_csv("cases_coded_prelim_clean1.csv")

#clean typos

data_deference <- data %>%
  mutate(
    deference = str_replace_all(deference,"Yes","yes"),
    deference = str_replace_all(deference,"scientific expertise","yes"),
    deference = str_replace_all(deference,"NULL","no"),
    deference = str_replace_all(deference,"null","no"),
    deference = str_replace_all(deference,"None","no"),
    deference = str_replace_all(deference,"none","no"),
    deference = str_replace_all(deference,"No","no"),
    deference = str_replace_all(deference,"no","no"),
    deference = str_replace_all(deference,"n","no"),
    deference = str_replace_all(deference,"mixed","yes"),
    deference = str_replace_all(deference,"agency expertise","yes"),
    deference = str_replace_all(deference,"12","yes"),
    deference = str_replace_all(deference,"1","yes"),
    deference = str_replace_all(deference,"0","no"),
    deference = str_replace_all(deference,"agenocy expertise","yes"),
    deference = str_replace_all(deference,"noo","no")
  )

#deference is clean

#create final dataframe and rename column for binding data

deference_c <- data_deference %>%
  select(
    `ID`, `deference`
  ) %>%
  rename(
    `deference_c` = `deference`)
    
    
    