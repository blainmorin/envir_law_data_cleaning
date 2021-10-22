### Prachi example

#test
#testing

##working with the species data

library(tidyverse)
library(stringr)
data <- read_csv("cases_coded_prelim_clean1.csv")

species_data <- data %>%
  select(Species)

species_data[is.na(species_data)] <- "none"

##separating multiple values

ncols <- max(stringr::str_count(species_data$Species, "%")) + 1
colmn <- paste("col", 1:ncols)

sep_df <-
  tidyr::separate(
    data = species_data,
    col = Species,
    sep = "%",
    into = colmn,
    remove = FALSE
  )

sep_df[is.na(sep_df)] <- "none"
