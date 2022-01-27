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

# remove pt_, dt_, fa_, statutes_ and outcome_ vars from primary dataframe - but
# keep cercla flag
cases <- cases %>%
  select(
    !contains(c("pt_","dt_","fa_","statutes_","outcome_")),
  )

# make original TON and OOC values into lists
cases <- cases %>%
  mutate(
    `Type of Nature` = str_split(
      `Type of Nature`,"%"
    ),
    `Object of Contention` = str_split(
      `Object of Contention`,"%"
    ),
  )

### LOAD TON METACATEGORY DATA ######
ton_mc <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1F0d8W_1JTSw4kKpoaNzZ7-A9gseuwXvXIdXnx9hxeoM/edit?usp=sharing"
)

ton_mc_top <- ton_mc %>%
  filter(
    (row_number() == 1 |
      row_number() == 2)
  )

ton_mc <- ton_mc %>%
  filter(
    (row_number() != 1 &
      row_number() != 2)
  )

# make all ton codes lower case
ton_mc <- ton_mc %>%
  mutate(
    across(
      .cols = everything(),
      .fns = str_to_lower
    )
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




### CODE TON FLAG ######

# Nearly all (all?) "anthropogenic" types of nature are not actually types of 
# nature - they are objects of contention. As such, most need to be recoded
# to identify what type of nature is actually at issue. Thus, all 
# anthropogenic codes are flagged for re-examination.

cases <- cases %>%
  mutate(
    ton_flag = case_when(
      ton_mc1 == "Anthropogenic" ~ 1,
      TRUE ~ 0
    )
  )



### RETURN TON TO STRING ######
cases <- cases %>%
  mutate(
    `Type of Nature` = str_c(`Type of Nature`,collapse = ",")
  )
### LOAD OOC METACATEGORY DATA ######
ooc_mc <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1zLvWX2TYLskFThpzELBJWKN-M2xISGlZE1BPN01j_3Q/edit#gid=0"
)

ooc_mc_top = ooc_mc %>%
  filter(
    row_number() <= 3
  )

ooc_mc = ooc_mc %>%
  filter(
    row_number() > 3
  )

# make all ooc codes lower case
ooc_mc <- ooc_mc %>%
  mutate(
    across(
      .cols = everything(),
      .fns = str_to_lower
    )
  )



### CODE TOP-LEVEL OOC METACATEGORIES ######

cases <- cases %>%
  rowwise() %>%
  mutate(
    # first, code all top-level OOC codes as "unknown"
    ooc_mc1 = "unknown",
    # now, start coding second-level OOC codes
    # OOC 1
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC1) ~ "Agricultural",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC1) ~ str_c(ooc_mc1,"Agricultural",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 2
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC2) ~ "Agricultural",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC2) ~ str_c(ooc_mc1,"Agricultural",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 3
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC3) ~ "Energy and Resoures",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC3) ~ str_c(ooc_mc1,"Energy and Resoures",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 4
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC4) ~ "Energy and Resoures",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC4) ~ str_c(ooc_mc1,"Energy and Resoures",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 5
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC5) ~ "Energy and Resoures",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC5) ~ str_c(ooc_mc1,"Energy and Resoures",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 6
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC6) ~ "Energy and Resoures",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC6) ~ str_c(ooc_mc1,"Energy and Resoures",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 7
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC7) ~ "Energy and Resoures",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC7) ~ str_c(ooc_mc1,"Energy and Resoures",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 8
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC8) ~ "Recreation",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC8) ~ str_c(ooc_mc1,"Recreation",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 9
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC9) ~ "Recreation",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC9) ~ str_c(ooc_mc1,"Recreation",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 10
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC10) ~ "Military",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC10) ~ str_c(ooc_mc1,"Military",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 11
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC11) ~ "Waste/Disposal/Pollution",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC11) ~ str_c(ooc_mc1,"Waste/Disposal/Pollution",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 12
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC12) ~ "Waste/Disposal/Pollution",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC12) ~ str_c(ooc_mc1,"Waste/Disposal/Pollution",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 13
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC3) ~ "Waste/Disposal/Pollution",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC13) ~ str_c(ooc_mc1,"Waste/Disposal/Pollution",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 14
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC14) ~ "Waste/Disposal/Pollution",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC14) ~ str_c(ooc_mc1,"Waste/Disposal/Pollution",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 15
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC15) ~ "Waste/Disposal/Pollution",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC15) ~ str_c(ooc_mc1,"Waste/Disposal/Pollution",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 16
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC16) ~ "Biophysical",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC16) ~ str_c(ooc_mc1,"Biophysical",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 17
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC17) ~ "Biophysical",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC17) ~ str_c(ooc_mc1,"Biophysical",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 18
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC18) ~ "Biophysical",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC18) ~ str_c(ooc_mc1,"Biophysical",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 19
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC19) ~ "Biophysical",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC19) ~ str_c(ooc_mc1,"Biophysical",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 20
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC20) ~ "Construction",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC20) ~ str_c(ooc_mc1,"Construction",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 21
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC21) ~ "Construction",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC21) ~ str_c(ooc_mc1,"Construction",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 22
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC22) ~ "Construction",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC22) ~ str_c(ooc_mc1,"Construction",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 23
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC23) ~ "Construction",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC23) ~ str_c(ooc_mc1,"Construction",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 24
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC24) ~ "Regulations/Policy",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC24) ~ str_c(ooc_mc1,"Regulations/Policy",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 25
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC25) ~ "Legal",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC25) ~ str_c(ooc_mc1,"Legal",sep = "%"),
        TRUE ~ ooc_mc1
      )
    ),
    # OOC 26
    ooc_mc1 = case_when(
      ooc_mc1 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC26) ~ "Non-environmental",
        TRUE ~ ooc_mc1
      ),
      ooc_mc1 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC26) ~ str_c(ooc_mc1,"Non-environmental",sep = "%"),
        TRUE ~ ooc_mc1
      )
    )
  )



test <- cases %>%
  select(
    ID,`Object of Contention`,ooc_mc1
  )






### CODE LOWER-LEVEL OOC METACATEGORIES ######

cases <- cases %>%
  rowwise() %>%
  mutate(
    # first, code all second-level OOC codes as "unknown"
    ooc_mc2 = "unknown",
    # now, start coding secind-level OOC codes
    # OOC 1
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC1) ~ "Farming",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC1) ~ str_c(ooc_mc2,"Farming",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 2
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC2) ~ "Fishing",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC2) ~ str_c(ooc_mc2,"Fishing",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 3
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC3) ~ "Renewables",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC3) ~ str_c(ooc_mc2,"Renewables",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 4
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC4) ~ "Fossil Fuel Extraction",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC4) ~ str_c(ooc_mc2,"Fossil Fuel Extraction",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 5
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC5) ~ "Production/Transmission/Use",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC5) ~ str_c(ooc_mc2,"Production/Transmission/Use",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 6
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC6) ~ "Nuclear",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC6) ~ str_c(ooc_mc2,"Nuclear",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 7
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC7) ~ "Other mineral extraction",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC7) ~ str_c(ooc_mc2,"Other mineral extraction",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 8
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC8) ~ "Hunting",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC8) ~ str_c(ooc_mc2,"Hunting",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 9
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC9) ~ "Other",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC9) ~ str_c(ooc_mc2,"Other",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 10
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC10) ~ "Military",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC10) ~ str_c(ooc_mc2,"Military",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 11
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC11) ~ "Production",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC11) ~ str_c(ooc_mc2,"Production",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 12
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC12) ~ "Production",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC12) ~ str_c(ooc_mc2,"Production",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 13
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC3) ~ "Waste Management",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC13) ~ str_c(ooc_mc2,"Waste Management",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 14
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC14) ~ "Waste Management",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC14) ~ str_c(ooc_mc2,"Waste Management",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 15
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC15) ~ "Cost and Liability",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC15) ~ str_c(ooc_mc2,"Cost and Liability",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 16
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC16) ~ "Flora/Fauna/Ecosystems",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC16) ~ str_c(ooc_mc2,"Flora/Fauna/Ecosystems",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 17
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC17) ~ "Flora/Fauna/Ecosystems",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC17) ~ str_c(ooc_mc2,"Flora/Fauna/Ecosystems",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 18
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC18) ~ "Water",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC18) ~ str_c(ooc_mc2,"Water",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 19
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC19) ~ "Land",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC19) ~ str_c(ooc_mc2,"Land",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 20
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC20) ~ "General Construction",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC20) ~ str_c(ooc_mc2,"General Construction",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 21
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC21) ~ "Water Infrastructure",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC21) ~ str_c(ooc_mc2,"Water Infrastructure",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 22
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC22) ~ "Structures/Buildings",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC22) ~ str_c(ooc_mc2,"Structures/Buildings",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 23
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC23) ~ "Transportation Infrastructure",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC23) ~ str_c(ooc_mc2,"Transportation Infrastructure",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 24
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC24) ~ "Regulations/Policy",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC24) ~ str_c(ooc_mc2,"Regulations/Policy",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 25
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC25) ~ "Legal",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC25) ~ str_c(ooc_mc2,"Legal",sep = "%"),
        TRUE ~ ooc_mc2
      )
    ),
    # OOC 26
    ooc_mc2 = case_when(
      ooc_mc2 == "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC26) ~ "Non-environmental",
        TRUE ~ ooc_mc2
      ),
      ooc_mc2 != "unknown" ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC26) ~ str_c(ooc_mc2,"Non-environmental",sep = "%"),
        TRUE ~ ooc_mc2
      )
    )
  )



test <- cases %>%
  select(
    ID,`Object of Contention`,ooc_mc1,ooc_mc2
  ) %>%
  group_by(
    `Object of Contention`
  ) %>%
  mutate(
    count = n()
  ) %>%
  ungroup()









### CODE HIGH LEVEL AND LOW LEVEL CASES FOR OOC METACATEGORIES ######

cases <- cases %>%
  rowwise() %>%
  mutate(
    # first, code lowest-level OOC codes as NA_character_
    ooc_mc3 = NA_character_,
    # now, start coding lowest-level OOC codes
    # OOC 11
    ooc_mc3 = case_when(
      is.na(ooc_mc3) ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC11) ~ "Production-HLC",
        TRUE ~ ooc_mc3
      ),
      !is.na(ooc_mc3) ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC11) ~ str_c(ooc_mc3,"Production-HLC",sep = "%"),
        TRUE ~ ooc_mc3
      )
    ),
    # OOC 12
    ooc_mc3 = case_when(
      is.na(ooc_mc3) ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC12) ~ "Production-LLC",
        TRUE ~ ooc_mc3
      ),
      !is.na(ooc_mc3) ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC12) ~ str_c(ooc_mc3,"Production-LLC",sep = "%"),
        TRUE ~ ooc_mc3
      )
    ),
    # OOC 13
    ooc_mc3 = case_when(
      is.na(ooc_mc3) ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC13) ~ "Waste Management-HLC",
        TRUE ~ ooc_mc3
      ),
      !is.na(ooc_mc3) ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC13) ~ str_c(ooc_mc3,"Waste Management-HLC",sep = "%"),
        TRUE ~ ooc_mc3
      )
    ),
    # OOC 14
    ooc_mc3 = case_when(
      is.na(ooc_mc3) ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC14) ~ "Waste Management-LLC",
        TRUE ~ ooc_mc3
      ),
      !is.na(ooc_mc3) ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC14) ~ str_c(ooc_mc3,"Waste Management-LLC",sep = "%"),
        TRUE ~ ooc_mc3
      )
    ),
    # OOC 16
    ooc_mc3 = case_when(
      is.na(ooc_mc3) ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC16) ~ "Flora/Fauna/Ecosystems-HLC",
        TRUE ~ ooc_mc3
      ),
      !is.na(ooc_mc3) ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC16) ~ str_c(ooc_mc3,"Flora/Fauna/Ecosystems-HLC",sep = "%"),
        TRUE ~ ooc_mc3
      )
    ),
    # OOC 17
    ooc_mc3 = case_when(
      is.na(ooc_mc3) ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC17) ~ "Flora/Fauna/Ecosystems-HLC",
        TRUE ~ ooc_mc3
      ),
      !is.na(ooc_mc3) ~ case_when(
        any(`Object of Contention` %in% ooc_mc$OOC17) ~ str_c(ooc_mc3,"Flora/Fauna/Ecosystems-LLC",sep = "%"),
        TRUE ~ ooc_mc3
      )
    )
  )

test <- cases %>%
  select(
    ID,`Object of Contention`,ooc_mc1,ooc_mc2,ooc_mc3
  )


### CODE OOC FLAG ######

# There are several categories of OOC codes that need to be flagged for further
# investigation. That includes all Waste/Disposal/Pollution: Production, all
# Waste/Disposal/Pollution: Waste Management, and all # Biophysical:
# Flora/Fauna/Ecosystems codes. All three of these categories need to be broken
# into higher and lower-level codes to differentiate between detailed sources of
# harm/contention, and the higher-level sources of those harms. E.g.,
# contaminated ground water (a lower-level object of contention) comes from a
# landfill (a higher-level object of contention).

# There are also several other OOC codes scattered throughout the data that indicate
# the need for further review. Thes are listed in a separate Google Sheet, loaded here:
ooc_flags <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1ACEeYSxZo7sKSodn3UEcSEJjm1MICuVIayNgctGeMrU/edit#gid=0"
)

cases <- cases %>%
  mutate(
    ooc_flag = case_when(
      (ooc_mc3 == "Production--HLC" |
        ooc_mc3 == "Production--LLC" |
        ooc_mc3 == "Waste Management-HLC" |
        ooc_mc3 == "Waste Management-LLC" |
        ooc_mc3 == "Flora/Fauna/Ecosystems-HLC" |
        ooc_mc3 == "Flora/Fauna/Ecosystems-LLC"
       ) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  rowwise() %>%
  mutate(
    ooc_flag = case_when(
      any(`Object of Contention` %in% ooc_flags$ooc_flag_term) ~ 1,
      TRUE ~ ooc_flag
      )
  )

# make any_flag column to count total flagged cases
cases <- cases %>%
  mutate(
    any_flag = case_when(
      ton_flag == 1 | ooc_flag == 1 ~ 1,
      TRUE ~ 0
    )
  )

# as of 1/27/2022, there are 2,031 cases that need further examination

### RETURN OOC TO STRING ######
cases <- cases %>%
  mutate(
    `Object of Contention` = str_c(`Object of Contention`,collapse = ",")
  )

### CODE CERCLA FLAG ######

# join cercla flag back
stat_cercla <- stat %>%
  select(
    ID,statutes_cercla
  ) %>%
  rename(
    "cercla_flag" = "statutes_cercla"
  )

cases <- left_join(cases,stat_cercla, by = "ID")

### CODE CLIMATE CATEGORIES ######

# create climate change terms code for find+ search
cases <- cases %>% 
  mutate(
  climate_num = "unknown",
  climate_keywords = "unknown"
)





### WRITE OUT DF FOR FURHTER RECODING ######

cases_out <- cases %>%
  select(
    ID,
    case_name,
    url,
    cite,
    summary,
    case_date,
    Plaintiffs,
    `Plaintiff Types`,
    Defendants,
    `Defendant Types`,
    Aim,
    `Type of Nature`,
    ton_flag,
    ton_mc1,
    ton_mc2,
    Species,
    `Object of Contention`,
    ooc_flag,
    ooc_mc1,
    ooc_mc2,
    ooc_mc3,
    Outcome,
    `Outcome Notes`,
    `Federal Agencies`,
    `Federal Statutes`,
    EJ_num,
    EJ_keywords,
    climate_num,
    climate_keywords,
    cercla_flag
  )

# write out clean data, ready for further examination
f = "/Users/rea.115/Dropbox/Professional/Research/_RESL/Environmental_Law_Research/Data/Clean/cases_for_reexamination.csv"
write_csv(cases_out,f)


### THE END ######
