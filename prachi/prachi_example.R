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
    Species = str_replace_all(Species, "baby turtle", "turtle"),
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
  ) %>%
  mutate(
    Species = str_replace_all(Species, "swallows", "swallow")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "pelicans", "pelican")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "cows", "cow")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "mussels", "mussel")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "falcons", "falcon")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "dolphins", "dolphin")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "elephants", "elephant")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "seals", "seal")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "shortnose suckers", "shortnose sucker"),
    Species = str_replace_all(Species, "razorback suckers", "razorback sucker")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "lemurs", "lemur")
  ) %>%
  mutate(
    Species = str_replace_all(Species, ", ", "%")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "mollusks", "mollusk")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "tunas", "tuna")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "pigs", "pig")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "certain types of alligators and crocodiles",
                              "crocodile%alligator")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "salmonids", "salmonid")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "egrets", "egret")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "terns", "tern")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "oranges", "orange"),
    Species = str_replace_all(Species, "grapefruits", "grapefruit")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "plants", "plant")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "trees", "tree")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "other shore and wading birds", "shore birds%wading birds")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "gb haddock", "georges bank haddock"),
    Species = str_replace_all(Species, "gb yellowtail flounder", "georges bank yellowtail flounder"),
    Species = str_replace_all(Species, "gb winter flounder", "georges bank winter flounder"),
    Species = str_replace_all(Species, "gom haddock", "gulf of maine haddock"),
    Species = str_replace_all(Species, "gom winter flounder", "gulf of maine winter flounder"),
    Species = str_replace_all(Species, "atlantic wolffish.", "atlantic wolffish"),
    Species = str_replace_all(Species, "cape cod/gom yellowtail flounder", "cape cod/gulf of maine yellowtail flounder"),
    Species = str_replace_all(Species, "sne/ma winter flounder", "southern new england/mid-atlantic winter flounder")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "donkeys\\(wild\\)", "wild donkey")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "blueback herring \\(alosa aestivalis\\)", "blueback herring"),
    Species = str_replace_all(Species, "delta smelt \\(fish\\)", "delta smelt"),
    Species = str_replace_all(Species, "chinook salmon \\(eggs and fry\\)", "chinook salmon"),
    Species = str_replace_all(Species, "cicurina cueva \\(spider\\)", "cicurina cueva"),
    Species = str_replace_all(Species, "georges bank \\(gb\\) cod", "georges bank cod"),
    Species = str_replace_all(Species, "gulf of maine \\(gom\\) cod", "gulf of maine cod"),
    Species = str_replace_all(Species, "southern new england/mid-atlantic \\(sne/ma\\) yellowtail flounder",
                              "southern new england/mid-atlantic yellowtail flounder")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "gom dps atlantic salmon", "gulf of maine distinct population segment atlantic salmon")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "raccoons", "raccoon")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "woodpeckers", "woodpecker"),
    Species = str_replace_all(Species, "blacked-backed woodpecker", "black-backed woodpecker"),
    Species = str_replace_all(Species, "hawks", "hawk")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "orcas", "orca")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "sage-grouse", "sage grouse")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "branchinecta lynchi", "fairy shrimp")
  )

species_df$Species [4938] <-
  "freshwater mussel%dark pigtoe%plicate rocksnail%black warrior waterdog%flattened musk turtle%rush darter%cahaba shiner%indiana bat%northern long-eared bat"

species_df$Species [728] <- 
  "butterflies%birds%seabirds%neotropical birds%flamingo%wading birds%grebe%grouse%parakeets%curassows%quail%flightless birds%pigeon%warbler%passerine birds"

##separating by flora, fauna, and ecosystem

fauna <- c("elk", "oyster", "chub", "alabama beach mouse", "american eel", "american lobster", "artic grayling", "asian carp", 
           "asian elephant", "atlantic bluefin tuna", "atlantic salmon", "atlantic summer flounder", "bald eagle", "bank swallow", 
           "bats", "bear", "beluga sturgeon", "bi-state sage grouse", "bighorn sheep", "bison", "black bear", "black-backed woodpecker", 
           "blueback herring", "brown pelican", "bull trout")

df_species <- species_df %>%
  mutate(
    Species_Type = case_when(
      Species %in% fauna ~ "fauna"
    )
  )








