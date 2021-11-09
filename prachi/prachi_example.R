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
    Species = str_replace_all(Species, "louisiana blackbear", "lousiana black bear"),
    Species = str_replace_all(Species, "greater yellowstone ecosystem grizzly bear", "grizzly bear")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "whales", "whale")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "stellar sea lions", "steller's sea lion"),
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
    Species = str_replace_all(Species, "loggerhead turtle", "loggerhead sea turtle"),
    Species = str_replace_all(Species, "sea turtle inhabiting the pacific ocean", "sea turtle")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "chubs", "chub")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "cats", "cat"),
    Species = str_replace_all(Species, "house cats", "cat"),
    Species = str_replace_all(Species, "house cat", "cat")
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
    Species = str_replace_all(Species, "pigs", "pig"),
    Species = str_replace_all(Species, "swine", "domestic pig")
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
    Species = str_replace_all(Species, "red-cockaded woodpecker", "red cockaded woodpecker"),
    Species = str_replace_all(Species, "hawks", "hawk"),
    Species = str_replace_all(Species, "pengrine falcon", "peregrine falcon")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "orcas", "orca")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "sage-grouse", "sage grouse"), 
    Species = str_replace_all(Species, "imperiled sage grouse", "sage grouse")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "branchinecta lynchi", "fairy shrimp"),
    Species = str_replace_all(Species, "san diego fairy shrimp", "fairy shrimp"),
    Species = str_replace_all(Species, "san diego dairy shrimp", "fairy shrimp")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "eagles", "eagle")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "golden-cheeked warbler", "golden cheeked warbler")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "gulo gulo luscus", "wolverine")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "west antelope", "antelope")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "water fowl and game birds", "water fowl%game birds"),
    Species = str_replace_all(Species, "waterfowl", "water fowl")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "shortnose sucker", "shortnose sucker fish"),
    Species = str_replace_all(Species, "shortnose sucker fish fish", "shortnose sucker fish"),
  ) %>%
  mutate(
    Species = str_replace_all(Species, "pacific lampry", "pacific lamprey")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "red-legged frog", "red legged frog")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "scimitarhorned oryx", "scimitar-horned oryx")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "bull-trout", "bull trout")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "steelhead", "steelhead trout"),
    Species = str_replace_all(Species, "steelhead trout trout", "steelhead trout")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "sacramento river winter-run chinook salmon", "winter chinook salmon"),
    Species = str_replace_all(Species, "sacramento river spring-run chinook salmon", "spring chinook salmon"),
    Species = str_replace_all(Species, "chinook", "chinook salmon"),
    Species = str_replace_all(Species, "chinook salmon salmon", "chinook salmon"),
    Species = str_replace_all(Species, "sockeye", "sockeye salmon"),
    Species = str_replace_all(Species, "sockeye salmon salmon", "sockeye salmon"),
    Species = str_replace_all(Species, "coho", "coho salmon"),
    Species = str_replace_all(Species, "coho salmon salmon", "coho salmon"),
    Species = str_replace_all(Species, "west coast coho salmon", "coho salmon"),
    Species = str_replace_all(Species, "winter-run chinook salmon", "winter chinook salmon"),
    Species = str_replace_all(Species, "winter chinook salmon slamon", "winter chinook salmon"),
    Species = str_replace_all(Species, "spring-run chinook salmon", "spring chinook salmon"),
    Species = str_replace_all(Species, "puget sound chinook salmon", "chinook salmon"),
    Species = str_replace_all(Species, "lower columbia river chinook salmon", "chinook salmon"),
    Species = str_replace_all(Species, "snake river spring and summer chinook salmon", "spring chinook salmon%summer chinook salmon"),
    Species = str_replace_all(Species, "central valley spring chinook salmon", "spring chinook salmon"),
    Species = str_replace_all(Species, "central valley spring-run chinook salmon", "spring chinook salmon"),
    Species = str_replace_all(Species, "wild spring chinook salmon", "spring chinook salmon"),
    Species = str_replace_all(Species, "snake river sockeye salmon", "sockeye salmon"),
    Species = str_replace_all(Species, "upper columbia river spring chinook salmon", "spring chinook salmon"),
    Species = str_replace_all(Species, "columbia river chum salmon", "chum salmon"),
    Species = str_replace_all(Species, "lower columbia river/southwest washington coho salmon", "coho salmon"),
    Species = str_replace_all(Species, "upper willamettte river chinook salmon", "chinook salmon"),
    Species = str_replace_all(Species, "snake river fall chinook salmon", "fall chinook salmon"),
    Species = str_replace_all(Species, "hood canal summer run chum", "summer chum salmon"),
    Species = str_replace_all(Species, "oregon coastal coho salmon", "coho salmon"),
    Species = str_replace_all(Species, "ozette lake sockeye salmon", "sockeye salmon"),
    Species = str_replace_all(Species, "steelhead trout salmon", "steelhead trout%salmon"),
    Species = str_replace_all(Species, "southern oregon/northern california coast coho salmon", "coho salmon")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "old forest growth dependent species", "old growth forest dependent species")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "protected fish", "fish")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "palila", "palila finch"),
    Species = str_replace_all(Species, "palila finch finch", "palila finch")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "northern stopped owl", "northern spotted owl")
  )

species_df$Species [4938] <-
  "freshwater mussel%dark pigtoe%plicate rocksnail%black warrior waterdog%flattened musk turtle%rush darter%cahaba shiner%indiana bat%northern long-eared bat"

species_df$Species [728] <- 
  "butterflies%birds%seabirds%neotropical birds%flamingo%wading birds%grebe%grouse%parakeets%curassows%quail%flightless birds%pigeon%warbler%passerine birds"

species_df$Species [5856] <- "cod%haddock%yellowtail flounder%american plaice%witch flounder%winter flounder%redfish%white hake%pollock%windowpane flounder%ocean pout%atlantic halibut%atlantic wolffish"

##separating by % so I can code for flora, fauna, and ecosystem

ncols <- max(stringr::str_count(species_df$Species, "%")) + 1
colmn <- paste("col", 1:ncols)

sep_df <-
  tidyr::separate(
    data = species_df,
    col = Species,
    sep = "%",
    into = colmn,
    remove = FALSE
  )

sep_df[is.na(sep_df)] <- "none"



##separating by flora, fauna, and ecosystem

fauna <- c("elk", "oyster", "chub", "alabama beach mouse", "american eel", "american lobster", "artic grayling", "asian carp", 
           "asian elephant", "atlantic bluefin tuna", "atlantic salmon", "atlantic summer flounder", "bald eagle", "bank swallow", 
           "bats", "bear", "beluga sturgeon", "bi-state sage grouse", "bighorn sheep", "bison", "black bear", "black-backed woodpecker", 
           "blueback herring", "brown pelican", "bull trout", "cactus ferruginous pygmy-owl", "california central valley steelhead", 
           "california condor", "california gnatcatcher", "california sea otter", "california spotted owl", "california tiger salamander", 
           "canada lynx", "canadian goose", "canadian wood bison", "cape fur seal", "caribbean electric ray", "cat", "cattle", 
           "central california coast steelhead", "chicken", "chimpanzee", "chinook salmon", "cicurina cueva", "cliff swallow", 
           "coastal migratory pelagic fish", "coastal shark", "coho salmon", "columbian sharp-tailed grouse", "common snook", "cow", 
           "crawfish", "darkblotched rockfish", "deer", "delta smelt", "desert bald eagle", "desert tortoise", "dog", "dolphin", 
           "domestic pig", "douglas fir tussock moth", "dunes sagebrush lizard", "dusky shark", "eagle", "eastern timber wolf", 
           "elephant", "ewe", "fairy shrimp", "false killer whale", "fish", "flat-tailed horned lizard", "flatwoods salamander", 
           "florida keys mole skink", "florida manatee", "florida panther", "flycatcher", "fur seal", "golden cheeked warbler", 
           "golden eagle", "goshawk", "gray squirrel", "gray wolf", "greater yellowstone ecosystem grizzly bear", "green sea turtle", 
           "grizzly bear", "groundfish", "gulf king mackerel", "gulf of maine distinct population segment atlantic salmon", 
           "gulf sturgeon", "gunnison sage grouse", "gypsy moth", "harris' hawk", "horse", "husky dog", "humpback chub", "humpback whale", 
           "indiana bat", "interior mountain quail", "jaguar", "jemez mountain salamander", "jumping mouse", "lamprey", "lizard", 
           "lobster", "loggerhead sea turtle", "loligo squid", "lousiana black bear", "lynx", "maopa dace fish", "marbled murrelet", 
           "mexican gray wolf", "mexican spotted owl", "mexican wolf", "migratory birds", "mojave desert tortoise", "monk seal", 
           "morning dove", "mountain lynx", "mountain plover", "mud swallow", "mussel", "neotropical birds", "new mexico meadow jumping mouse", 
           "new mexico ridgenosed rattlesnake", "yosemite toad", "yellow flycatcher", "woodland caribou", "wood duck", "wolverine", 
           "wild spring chinook salmon", "wild horse", "wild california condor", "whooping crane", "white-tailed deer", "whale", 
           "western pond turtle", "western gray squirrel", "west coast coho salmon", "antelope", "waterfowl", "walleye", "turtle", 
           "utah prarie dog", "tuna", "trumpeter swan", "trout", "tilefish", "tiger", "tehachapi slender salamander", "swainson's hawk", 
           "summer flounder", "sucker fish", "sturgeon", "striped bass", "steller's sea lion", "steelhead trout", "spotted owl", 
           "spiny dogfish", "spinedace", "sperm whale", "sparrow", "spalding's catchfly", "southwestern willow flycatcher", 
           "southern resident killer whale", "southern pine beetle", "snook", "snapper grouper", "snake river mollusk", "snail kite", 
           "snail darter", "silvery minnow", "shrimp", "shortnose sucker fish", "shellfish", "sheep", "sharp-tailed grouse", "shark", 
           "selkirk mountains caribou", "selkirk grizzly bear", "seal", "sea turtle", "sea otter", "sea lion", "scallop", 
           "san diego fairy shrimp", "salmonid", "salmon", "sage grouse", "rusty patched bumble bee", "rocky mountain gray wolf", 
           "rock lobster", "rio grande silvery minnow", "rio grande cutthroat trout", "right whale", "reptiles", "red-legged frog", 
           "red cockaded woodpecker", "red wolf", "red tree vole", "red tailed hawk", "red snapper", "pygmy-owl", "pygmy rabbit", 
           "preble's meadow jumping mouse", "polar bear", "piping plover", "pilot whale", "pig", "perdido key beach mouse", 
           "peregrine falcon", "patagonian toothfish", "pallid sturgeon", "palila finch", "palila", "pacific lamprey", "pacific halibut", 
           "pacific fisher", "oregon chub", "orca", "ocean mammals", "northern spotted owl", "northern right whale", "northern riffleshell mussel", 
           "northern long-eared bat", "northern goshawk", "northern aplomado falcon", "northern anchovy", "alameda whipsnake", 
           "california red-legged frog", "beringia bearded seal", "okhotsk bearded seal", "atka mackerel", "pacific cod", "atlantic cod",
           "haddock", "austin blind salamander", "barton springs salamander", "bee creek cave harvestman", "kretschmarr cave mold beetle", 
           "tooth cave pseudo scorpion", "tooth cave spider", "tooth cave ground beetle", "cactus ferruginous pygmy owl", "razorback sucker", 
           "california brown pelican", "california least tern", "beldings savannah sparrow", "california red legged frog", 
           "san francisco garter snake", "raccoon", "cape sable seaside sparrow", "everglade snail kite", "wood stork", 
           "central valley spring-run chinook", "central valley steelhead", "green sturgeon", "coastal blackfin shark", 
           "coastal bull shark", "coastal california gnatcatcher", "least bell's vireo", "western snowy plover", "pacific pocket mouse", 
           "colorado river fish species", "crocodile", "alligator", "georges bank cod", "gulf of maine cod", "georges bank haddock", 
           "gulf of maine haddock", "georges bank yellowtail flounder", "southern new england/mid-atlantic yellowtail flounder", 
           "cape cod/gulf of maine yellowtail flounder", "american plaice", "witch flounder", "georges bank winter flounder", 
           "gulf of maine winter flounder", "southern new england/mid-atlantic winter flounder", "redfish", "white hake", "pollock", 
           "northern windowpane flounder", "southern windowpane flounder", "ocean pout", "atlantic halibut", "atlantic wolffish", 
           "goose", "migritory birds", "halibut", "crab", "southwest willow flycatcher", "hawaiian hawk", "hawaiian hoary bat", 
           "heron", "egret", "tern", "shore birds", "wading birds", "key largo cotton mouse", "key deer", "key largo woodrat", 
           "lower keys marsh rabbit", "schaus' swallowtail butterfly", "silver rice rat", "stock island tree snail", "mammals",
           "birds", "mollusk", "oregon spotted frog", "neotropical migratory songbirds", "wood thrush", "scarlet tanager", 
           "cerulean warbler", "wild turkey", "ruffed grouse", "woodcock", "wild duck", "migratory game birds", "western arctic caribou", 
           "wolf", "water fowl", "game birds", "utah prairie dog", "texas blind salamander", "fountain darter", "san marcos salamander", 
           "san marcos gambusia", "swordfish", "bluefin tuna", "surf clam", "ocean quahog", "spruce beetle", "spikedace", 
           "loach minnow", "spider tortoise", "flat-tailed tortoise", "siskiyou mountains salamander", "scott bar salamander", 
           "shad and river herring", "riverside fariy shrimp", "rough-legged hawk", "sacramento river winter-run chinook salmon", 
           "red-tailed hawk", "central valley spring-run chinook salmon", "vernal pool tadpole shrimp", "vernal pool custaceans", 
           "rhadine exilis", "rhadine internalis", "meshweaver spider", "red legged frog", "red fox", "least tern", 
           "light-footed clapper rail", "rainbow trout", "bat", "rail", "loon", "pacific salmon", "pacific groundfish", 
           "pacific whiting", "nova salmon", "whitefish", "scimitar-horned oryx")

flora <- c("245 unnamed plant species", "alpine plant", "mint", "orange", "grapefruit", "corn", "dudley bluffs bladderpod", 
           "dudley bluffs twinpod", "douglas-fir tree", "englemann spruce tree", "alfalfa", "hauchuca water umbel", "marijuana", 
           "capsule dung moss", "cattail", "texas wild-rice", "sugar beets", "skunk cabbage", "slickspot peppergrass", 
           "sequoia sempervirens", "sebastopol meadowfoam", "san fernando valley spineflower", "sagebrush", "western juniper", 
           "sacramento ocrutt grass", "slender orcutt grass", "riparian vegetation", "red maple", "ash", "hickory", "water oak", 
           "sweet bay", "red bay tree", "red mangrove", "port orford cedar", "penstemon", "pea plant", "mixed northern hardwoods")

ecosystem <- c("old growth forest dependent species", "aspen stands", "coral reef")


df_species <- sep_df %>%
  mutate(
    Species_Type = case_when(
      `col 1` %in% fauna ~ "fauna", 
      `col 2` %in% fauna ~ "fauna",
      `col 3` %in% fauna ~ "fauna",
      `col 4` %in% fauna ~ "fauna",
      `col 5` %in% fauna ~ "fauna",
      `col 6` %in% fauna ~ "fauna",
      `col 7` %in% fauna ~ "fauna",
      `col 8` %in% fauna ~ "fauna",
      `col 9` %in% fauna ~ "fauna",
      `col 10` %in% fauna ~ "fauna",
      `col 11` %in% fauna ~ "fauna",
      `col 12` %in% fauna ~ "fauna",
      `col 13` %in% fauna ~ "fauna",
      `col 14` %in% fauna ~ "fauna",
      `col 15` %in% fauna ~ "fauna",
      `col 16` %in% fauna ~ "fauna",
      `col 17` %in% fauna ~ "fauna",
      `col 18` %in% fauna ~ "fauna",
      `col 19` %in% fauna ~ "fauna",
      `col 20` %in% fauna ~ "fauna",
      `col 1` %in% flora ~ "flora", 
      `col 2` %in% flora ~ "flora",
      `col 3` %in% flora ~ "flora",
      `col 4` %in% flora ~ "flora",
      `col 5` %in% flora ~ "flora",
      `col 6` %in% flora ~ "flora",
      `col 7` %in% flora ~ "flora",
      `col 8` %in% flora ~ "flora",
      `col 9` %in% flora ~ "flora",
      `col 10` %in% flora ~ "flora",
      `col 11` %in% flora ~ "flora",
      `col 12` %in% flora ~ "flora",
      `col 13` %in% flora ~ "flora",
      `col 14` %in% flora ~ "flora",
      `col 15` %in% flora ~ "flora",
      `col 16` %in% flora ~ "flora",
      `col 17` %in% flora ~ "flora",
      `col 18` %in% flora ~ "flora",
      `col 19` %in% flora ~ "flora",
      `col 20` %in% flora ~ "flora",
      `col 1` %in% ecosystem ~ "ecosystem", 
      `col 2` %in% ecosystem ~ "ecosystem",
      `col 3` %in% ecosystem ~ "ecosystem",
      `col 4` %in% ecosystem ~ "ecosystem",
      `col 5` %in% ecosystem ~ "ecosystem",
      `col 6` %in% ecosystem ~ "ecosystem",
      `col 7` %in% ecosystem ~ "ecosystem",
      `col 8` %in% ecosystem ~ "ecosystem",
      `col 9` %in% ecosystem ~ "ecosystem",
      `col 10` %in% ecosystem ~ "ecosystem",
      `col 11` %in% ecosystem ~ "ecosystem",
      `col 12` %in% ecosystem ~ "ecosystem",
      `col 13` %in% ecosystem ~ "ecosystem",
      `col 14` %in% ecosystem ~ "ecosystem",
      `col 15` %in% ecosystem ~ "ecosystem",
      `col 16` %in% ecosystem ~ "ecosystem",
      `col 17` %in% ecosystem ~ "ecosystem",
      `col 18` %in% ecosystem ~ "ecosystem",
      `col 19` %in% ecosystem ~ "ecosystem",
      `col 20` %in% ecosystem ~ "ecosystem")
  )






