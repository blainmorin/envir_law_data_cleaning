### Prachi example

#test
#testing

##loading species data

library(tidyverse)
library(stringr)
data <- read_csv("cases_coded_prelim_clean1.csv")

species_data <- data %>%
  select(Species)

species_data[is.na(species_data)] <- "none"

##separating multiple values to examine data better

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


## plural to singular
species_df <- species_data %>%
  mutate(
    Species = str_replace_all(Species, "grizzly bears", "grizzly bear"),
    Species = str_replace_all(Species, "grizzlies", "grizzly bear"),
    Species = str_replace_all(Species, "louisiana blackbear", "lousiana black bear")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "whales", "whale")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "stellar sea lions", "stellar's sea lion"),
    Species = str_replace_all(Species, "california sea lions", "california lion"),
    Species = str_replace_all(Species, "lions", "lion")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "gray wolves", "gray wolf"),
    Species = str_replace_all(Species, "red wolves", "red wolf"),
    Species = str_replace_all(Species, "mexican gray wolves", "mexican gray wolf"),
    Species = str_replace_all(Species, "wolves", "wolf")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "tigers", "tiger")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "chickens", "chicken")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "coastal sharks", "coastal shark"),
    Species = str_replace_all(Species, "sharks", "shark"),
    Species = str_replace_all(Species, "dusky sharks", "dusky shark")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "surf clams", "surf clam")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "ocean quahogs", "ocean quahog")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "turtles", "turtle"),
    Species = str_replace_all(Species, "green sea turtles", "green sea turtle"),
    Species = str_replace_all(Species, "loggerhead sea turtles", "loggerhead sea turtle"),
    Species = str_replace_all(Species, "sea turtles", "sea turtle"),
    Species = str_replace_all(Species, "loggerhead turtles", "loggerhead sea turtle"),
    Species = str_replace_all(Species, "sea turtles inhabiting the pacific ocean", "sea turtle")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "chubs", "chub")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "cats", "cat"),
    Species = str_replace_all(Species, "house cats", "cats")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "dogs", "dog"),
    Species = str_replace_all(Species, "utah prarie dogs", "utah prarie dog")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "marbled murrelets", "marbled murrelet")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "cattails", "cattail")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "oysters", "oyster")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "geese", "goose"),
    Species = str_replace_all(Species, "canadian geese", "canadian goose"),
    Species = str_replace_all(Species, "canada goose", "canadian goose")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "whooping cranes", "whooping crane")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "horses", "horse")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "wood ducks", "wood duck"),
    Species = str_replace_all(Species, "wild ducks", "wild duck")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "fishes", "fish")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "snakes", "snake")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "owls", "owl")
  )

