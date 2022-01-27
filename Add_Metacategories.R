# Script to append meta-categories to data after cleaning
# Last modified: January 26, 2022
# By Chris Rea

library(tidyverse)
library(googlesheets4)

### LOAD DATA; SEPARATE BINARY VALUES ######

# load data
# note: in the future, modify to simply take input from cleaning scripts
cases <- read_csv("/Users/rea.115/Dropbox/Professional/Research/_RESL/Environmental_Law_Research/Data/Clean/data_clean_no_non_w_analysis_1_21_22.csv")

# break away pt_, dt_, fa_ vars
pt_dt <- cases %>%
  select(
    ID,
    contains(c("pt_","dt_"))
  )

fa <- cases %>%
  select(
    ID,
    contains(c("fa_"))
  )

stat <- cases %>%
  select(
    ID,
    contains(c("statutes_"))
  )

out <- cases %>%
  select(
    ID,
    contains(c("outcome_"))
  )

# remove pt_, dt_, fa_, statutes_ and outcome_ vars from primary dataframe
cases <- cases %>%
  select(
    !contains(c("pt_","dt_","fa_","statutes_","outcome_"))
  )

# make orginal TON and OOC values into lists
cases <- cases %>%
  mutate(
    `Type of Nature` = str_split(
      `Type of Nature`,"%"
    ),
    `Object of Contention` = str_split(
      `Type of Nature`,"%"
    ),
  )

### LOAD TON METACATEGORY ######
ton_mc <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1F0d8W_1JTSw4kKpoaNzZ7-A9gseuwXvXIdXnx9hxeoM/edit?usp=sharing"
)

ton_mc_top = ton_mc %>%
  filter(
    (row_number() == 1 |
      row_number() == 2)
  )

ton_mc = ton_mc %>%
  filter(
    (row_number() != 1 &
      row_number() != 2)
  )



### CODE TOP-LEVEL TON METACATEGORIES ######

cases <- cases %>%
  rowwise() %>%
  mutate(
    # first, code all top-level TON codes as "unknown"
    ton_mc1 = "unknown",
    # now, start coding top level TON codes
    # TON 1
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON1) ~ "Biosphere",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON1) ~ str_c(ton_mc1,"Biosphere",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 2
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON2) ~ "Biosphere",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON2) ~ str_c(ton_mc1,"Biosphere",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 3
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON3) ~ "EMR",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON3) ~ str_c(ton_mc1,"EMR",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 4
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON4) ~ "EMR",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON4) ~ str_c(ton_mc1,"EMR",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 5
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON5) ~ "EMR",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON5) ~ str_c(ton_mc1,"EMR",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 6
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON6) ~ "Landscapes",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON6) ~ str_c(ton_mc1,"Landscapes",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 7
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON7) ~ "Landscapes",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON7) ~ str_c(ton_mc1,"Landscapes",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 8
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON8) ~ "Landscapes",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON8) ~ str_c(ton_mc1,"Landscapes",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 9
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON9) ~ "Atmosphere",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON9) ~ str_c(ton_mc1,"Atmosphere",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 10
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON10) ~ "Anthropogenic",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON10) ~ str_c(ton_mc1,"Anthropogenic",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 11
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON11) ~ "Anthropogenic",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON11) ~ str_c(ton_mc1,"Anthropogenic",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 12
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON12) ~ "Anthropogenic",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON12) ~ str_c(ton_mc1,"Anthropogenic",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 13
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON3) ~ "Anthropogenic",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON13) ~ str_c(ton_mc1,"Anthropogenic",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 14
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON14) ~ "Non-environmenal",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON14) ~ str_c(ton_mc1,"Non-environmenal",sep = "%"),
        TRUE ~ ton_mc1
      )
    )
  )
        
        
        
        
 


test <- cases %>%
  select(
    ID,`Type of Nature`,ton_mc1
  )


### CODE LOWER-LEVEL TON METACATEGORIES ######
cases <- cases %>%
  rowwise() %>%
  mutate(
    # first, code all top-level TON codes as "unknown"
    ton_mc2 = "unknown",
    # now, start coding top level TON codes
    # TON 1
    ton_mc2 = case_when(
      ton_mc2 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON1) ~ "Flora",
        TRUE ~ ton_mc2
      ),
      ton_mc2 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON1) ~ str_c(ton_mc2,"Flora",sep = "%"),
        TRUE ~ ton_mc2
      )
    ),
    # TON 2
    ton_mc2 = case_when(
      ton_mc2 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON2) ~ "Fauna",
        TRUE ~ ton_mc2
      ),
      ton_mc2 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON2) ~ str_c(ton_mc2,"Fauna",sep = "%"),
        TRUE ~ ton_mc2
      )
    ),
    # TON 3
    ton_mc2 = case_when(
      ton_mc2 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON3) ~ "Fossil Fuels",
        TRUE ~ ton_mc2
      ),
      ton_mc2 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON3) ~ str_c(ton_mc2,"Fossil Fuels",sep = "%"),
        TRUE ~ ton_mc2
      )
    ),
    # TON 4
    ton_mc2 = case_when(
      ton_mc2 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON4) ~ "Renewable",
        TRUE ~ ton_mc2
      ),
      ton_mc2 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON4) ~ str_c(ton_mc2,"Renewable",sep = "%"),
        TRUE ~ ton_mc2
      )
    ),
    # TON 5
    ton_mc2 = case_when(
      ton_mc2 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON5) ~ "Industrial/Materials",
        TRUE ~ ton_mc2
      ),
      ton_mc2 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON5) ~ str_c(ton_mc2,"Industrial/Materials",sep = "%"),
        TRUE ~ ton_mc2
      )
    ),
    # TON 6
    ton_mc2 = case_when(
      ton_mc2 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON6) ~ "Landscapes",
        TRUE ~ ton_mc2
      ),
      ton_mc2 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON6) ~ str_c(ton_mc2,"Landscapes",sep = "%"),
        TRUE ~ ton_mc2
      )
    ),
    # TON 7
    ton_mc2 = case_when(
      ton_mc2 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON7) ~ "Marine",
        TRUE ~ ton_mc2
      ),
      ton_mc2 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON7) ~ str_c(ton_mc2,"Marine",sep = "%"),
        TRUE ~ ton_mc2
      )
    ),
    # TON 8
    ton_mc2 = case_when(
      ton_mc2 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON8) ~ "Freshwater",
        TRUE ~ ton_mc2
      ),
      ton_mc2 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON8) ~ str_c(ton_mc2,"Freshwater",sep = "%"),
        TRUE ~ ton_mc2
      )
    ),
    # TON 9
    ton_mc2 = case_when(
      ton_mc2 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON9) ~ "Atmosphere",
        TRUE ~ ton_mc2
      ),
      ton_mc2 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON9) ~ str_c(ton_mc2,"Atmosphere",sep = "%"),
        TRUE ~ ton_mc2
      )
    ),
    # TON 10
    ton_mc2 = case_when(
      ton_mc2 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON10) ~ "Hazardous Substance",
        TRUE ~ ton_mc2
      ),
      ton_mc2 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON10) ~ str_c(ton_mc2,"Hazardous Substance",sep = "%"),
        TRUE ~ ton_mc2
      )
    ),
    # TON 11
    ton_mc2 = case_when(
      ton_mc2 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON11) ~ "Nuclear",
        TRUE ~ ton_mc2
      ),
      ton_mc2 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON11) ~ str_c(ton_mc2,"Nuclear",sep = "%"),
        TRUE ~ ton_mc2
      )
    ),
    # TON 12
    ton_mc2 = case_when(
      ton_mc2 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON12) ~ "Construction",
        TRUE ~ ton_mc2
      ),
      ton_mc2 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON12) ~ str_c(ton_mc2,"Construction",sep = "%"),
        TRUE ~ ton_mc2
      )
    ),
    # TON 13
    ton_mc2 = case_when(
      ton_mc2 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON3) ~ "Other",
        TRUE ~ ton_mc2
      ),
      ton_mc2 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON13) ~ str_c(ton_mc2,"Other",sep = "%"),
        TRUE ~ ton_mc2
      )
    ),
    # TON 14
    ton_mc2 = case_when(
      ton_mc2 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON14) ~ "Non-environmenal",
        TRUE ~ ton_mc2
      ),
      ton_mc2 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON14) ~ str_c(ton_mc2,"Non-environmenal",sep = "%"),
        TRUE ~ ton_mc2
      )
    )
  )







test <- cases %>%
  select(
    ID,`Type of Nature`,ton_mc2
  )




### LOAD OOC METACATEGORY ######
ooc_mc <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1zLvWX2TYLskFThpzELBJWKN-M2xISGlZE1BPN01j_3Q/edit#gid=0"
)

ooc_mc_top = ooc_mc %>%
  filter(
    row_number() <= 4
  )

ooc_mc = ooc_mc %>%
  filter(
    row_number() > 4
  )



### CODE TOP-LEVEL OOC METACATEGORIES ######

cases <- cases %>%
  rowwise() %>%
  mutate(
    # first, code all top-level TON codes as "unknown"
    ton_mc1 = "unknown",
    # now, start coding top level TON codes
    # TON 1
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON1) ~ "Biosphere",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON1) ~ str_c(ton_mc1,"Biosphere",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 2
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON2) ~ "Biosphere",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON2) ~ str_c(ton_mc1,"Biosphere",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 3
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON3) ~ "EMR",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON3) ~ str_c(ton_mc1,"EMR",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 4
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON4) ~ "EMR",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON4) ~ str_c(ton_mc1,"EMR",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 5
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON5) ~ "EMR",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON5) ~ str_c(ton_mc1,"EMR",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 6
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON6) ~ "Landscapes",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON6) ~ str_c(ton_mc1,"Landscapes",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 7
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON7) ~ "Landscapes",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON7) ~ str_c(ton_mc1,"Landscapes",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 8
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON8) ~ "Landscapes",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON8) ~ str_c(ton_mc1,"Landscapes",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 9
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON9) ~ "Atmosphere",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON9) ~ str_c(ton_mc1,"Atmosphere",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 10
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON10) ~ "Anthropogenic",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON10) ~ str_c(ton_mc1,"Anthropogenic",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 11
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON11) ~ "Anthropogenic",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON11) ~ str_c(ton_mc1,"Anthropogenic",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 12
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON12) ~ "Anthropogenic",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON12) ~ str_c(ton_mc1,"Anthropogenic",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 13
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON3) ~ "Anthropogenic",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON13) ~ str_c(ton_mc1,"Anthropogenic",sep = "%"),
        TRUE ~ ton_mc1
      )
    ),
    # TON 14
    ton_mc1 = case_when(
      ton_mc1 == "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON14) ~ "Non-environmenal",
        TRUE ~ ton_mc1
      ),
      ton_mc1 != "unknown" ~ case_when(
        any(`Type of Nature` %in% ton_mc$TON14) ~ str_c(ton_mc1,"Non-environmenal",sep = "%"),
        TRUE ~ ton_mc1
      )
    )
  )







test <- cases %>%
  select(
    ID,`Type of Nature`,ton_mc1
  )


### MISC ######

any(cases$`Type of Nature`[cases$rand_num==2257])

test1 <- c("a","b","c","d")
test2 <- c("b","a")
result <- test2 %in% test1
any(result)
