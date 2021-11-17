### Casey Sample

### check

### hello world take 2

# load data set

install.packages("tidyverse")
library(tidyverse)

data <- read_csv("cases_coded_prelim_clean1.csv")

data1 <- data %>%
  select(
    court, year, Outcome
  ) %>%
  filter(
    year >= 1990
  ) %>%
  mutate(
    year_1 = year+1,
    year = year_1+1,
    year_2000 = case_when(
      year >= 2018 ~ "recent",
      year < 2000 ~ "old",
      TRUE ~ "between"
    ),
    court_1 = str_c(
      court,"weeeee",sep = ""
    ),
    friday_7 = str_replace_all(court, "United", "Divided"),
    friday_7 = str_replace_all(court, "USA", "Divided")
)

data$Aim
data$'Federal Agencies'


data_fed_agency <- data %>%
  select(
    'Federal Agencies'
  ) %>%
  group_by(
    'Federal Agencies'
  ) %>%
  filter(
    row_number()== 1
  ) %>%
  ungroup()


#DEFENDANT TYPES
#11/5/21

#packages

library(tidyverse)
library(stringr)

#load data

data <- read_csv("cases_coded_prelim_clean1.csv")

#create defendant type dataframe and arrange to see unique values

data_defendant_type <- data %>%
  select(
    `Defendant Types`
  ) %>%
  group_by(
    `Defendant Types`
  ) %>%
  filter(
    row_number()== 1
  ) %>%
  ungroup() %>%
  arrange(
    desc(`Defendant Types`)
  )

#make blanks none

data_defendant_type[is.na(data_defendant_type)] <- "none"

#separate multiple variables for identifying data that needs fixed

ncols <- max(stringr::str_count(data_defendant_type$'Defendant Types', "%")) + 1
colmn <- paste("col", 1:ncols)

sep_defendant_types <-
  tidyr::separate(
    data = data_defendant_type,
    col = 'Defendant Types',
    sep = "%",
    into = colmn,
    remove = FALSE
  )

#make separate blank columns none

sep_defendant_types[is.na(sep_defendant_types)] <- "none"

#fixing incorrect codes and typos 11/5/21

defendant_type_df <- data_defendant_type %>%
  mutate(
    `Defendant Types` = str_replace_all(`Defendant Types`,"city","local"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"city%local%industry","local%industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"civiv_assn","civic_assn"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"corporation","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"fed%fed","fed"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indsutry","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"ipublic_org","public_org"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"fed%local%local","fed%local"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"federal","fed"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indididual","individual"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indivudal","individual"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indistry","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indivdual","individual"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"individual%industry%industry","individual%industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"individuals","individual"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indiviudal","individual"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indsustry","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indsutry","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"industruy","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"industry%industry%industry%industry","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"industry%local%industry","industry%local"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"publi_org","public_org"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"industry\\$individual","industry%individual"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"industy","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"indvidual","individual"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"insutry","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"inudstry","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"industrt","industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"public org","public_org"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"university","public_org"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"local%local%industry","local%industry"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"local%local%local%local%fed","local%fed"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"local%local%state%trade_assn","local%state%trade_assn"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"militay","military"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"ngo\\?","ngo"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"unclear","other"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"county","local"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"seek damages","fed"),
    `Defendant Types` = str_replace_all(`Defendant Types`,"sought recovery costs for damages","local")
  )


#re-run separate multiple variables for identifying data that needs fixed

ncols <- max(stringr::str_count(data_defendant_type$'Defendant Types', "%")) + 1
colmn <- paste("col", 1:ncols)

sep_defendant_types_2 <-
  tidyr::separate(
    data = defendant_type_df,
    col = 'Defendant Types',
    sep = "%",
    into = colmn,
    remove = FALSE
  )

#view unique values of cleaned data to double check work and catch any typos I missed
#to view each column in unique_sep_pt, grouped by and arranged by "`col 1`, `col 2`, etc."

unique_sep_dt <- sep_defendant_types_2 %>%
  select(
    `Defendant Types`,`col 1`,`col 2`,`col 3`,`col 4`,`col 5`, `col 6`
  ) %>%
  group_by(
    `col 1`
  ) %>%
  filter(
    row_number()== 1
  ) %>%
  arrange(
    desc(`col 1`)
  )

#defendant types are now cleaned


#PLAINTIFF TYPES
#11/3/21

#install packages

install.packages("tidyverse")
library(tidyverse)
library(stringr)

#load data

data <- read_csv("cases_coded_prelim_clean1.csv")

#create plaintiff type dataframe

data_plaintiff_type <- data %>%
  select(
    `Plaintiff Types`
  ) %>%
  group_by(
    `Plaintiff Types`
  ) %>%
  filter(
    row_number()== 1
  ) %>%
  arrange(
    desc(`Plaintiff Types`)
  )


#make blanks none

data_plaintiff_type[is.na(data_plaintiff_type)] <- "none"

#separate multiple variables for identifying data that needs fixed

ncols <- max(stringr::str_count(data_plaintiff_type$'Plaintiff Types', "%")) + 1
colmn <- paste("col", 1:ncols)

sep_df <-
  tidyr::separate(
    data = data_plaintiff_type,
    col = 'Plaintiff Types',
    sep = "%",
    into = colmn,
    remove = FALSE
  )

sep_df[is.na(sep_df)] <- "none"

#fixing incorrect codes and typos

plaintiff_type_df <- data_plaintiff_type %>%
  mutate(
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"u.s. department of labor federal credit union","fed"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"unclear","other"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"fed, ngo", "fed%ngo"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"inc", "industry"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"individaul", "individual"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"individiual%ngo", "individual%ngo"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"individauk", "individual"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"individuak", "individual"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"individual%ciy%state%ngo%other", "individual%local%state%ngo%other"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"individual%estate", "individual%state"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"individual%trade_assm%industry", "individual%trade_assn%industry"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"individuals", "individual"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"individuals%industry", "individual%industry"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"individuals%local", "individual%local"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"indsutry", "industry"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"indsutries", "industry"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"industries", "industry"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"industry%indivudal", "industry%individual"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"industy%indivudal", "industry%individual"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"industy%individual", "industry%individual"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"n/a", "none"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"ngi", "ngo"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"ngo%civic_assn%civic_assn", "ngo%civic_assn"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"ngo%indivdual", "ngo%individual"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"ngo%individuals", "ngo%individual"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"ngo%tribal", "ngo%tribe"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"party", "ngo"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"quasi-public", "public_org"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"raymond s. hardman", "individual"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"religious", "religious_org"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"religious_org%individuals", "religious_org%individual"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"religious_org_org", "religious_org"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"religious_org_org%individual", "religious_org%individual"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"religious_org_org%individual%civic_assn", "religious_org%individual%civic_assn"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"city", "local"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"city%civic_assn", "local%civic_assn"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"city%individual", "local%individual"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"city%local%individual", "local%individual"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"city%ngo", "local%ngo"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"city%state", "local%state"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"state%city", "state%local"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"individual%city", "individual%local"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"individual%city%industry", "individual%local%industry"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"ngo%city%local","ngo%local"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"civic_assn&ngo","civic_assn%ngo"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"civil_assn","civic_assn"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"fed%civiv_assn","fed%civic_assn"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"local%ngo%inudstry%individual","local%ngo%industry%individual"),
    `Plaintiff Types` = str_replace_all(`Plaintiff Types`,"unknown","other")
  )

#re-run separate multiple variables for identifying data that needs fixed

ncols <- max(stringr::str_count(data_plaintiff_type$'Plaintiff Types', "%")) + 1
colmn <- paste("col", 1:ncols)

sep_plaintiff_type_df <-
  tidyr::separate(
    data = plaintiff_type_df,
    col = 'Plaintiff Types',
    sep = "%",
    into = colmn,
    remove = FALSE
  )
sep_plaintiff_type_df[is.na(sep_plaintiff_type_df)] <- "none"  

#view unique values of cleaned data to double check work and catch any typos I missed
#to view each column in unique_sep_pt, grouped by and arranged by "`col 1`, `col 2`, etc."

unique_sep_pt <- sep_plaintiff_type_df %>%
  select(
    `Plaintiff Types`,`col 1`,`col 2`,`col 3`,`col 4`,`col 5`, `col 6`
  ) %>%
  group_by(
    `col 6`
  ) %>%
  filter(
    row_number()== 1
  ) %>%
  arrange(
    desc(`col 6`)
  )

# plaintiff_type_df should be fully cleaned now! 

#OUTCOME
#11/10/21

#packages

library(tidyverse)
library(stringr)

#load data

data <- read_csv("cases_coded_prelim_clean1.csv")

#create outcome dataframe and arrange to see unique values

data_outcome <- data %>%
  select(
    `Outcome`
  ) %>%
  group_by(
    `Outcome`
  ) %>%
  filter(
    row_number()== 1
  ) %>%
  ungroup() %>%
  arrange(
    desc(`Outcome`)
  )

#make blanks none

data_outcome[is.na(data_outcome)] <- "none"

#fix typos and incorrect codes

outcome_df <- data_outcome %>%
  mutate(
    `Outcome` = str_replace_all(`Outcome`,"insurance cost","plaintiff"),
    `Outcome` = str_replace_all(`Outcome`,"environmental nature is second at best","none"),
    `Outcome` = str_replace_all(`Outcome`,"plaitiff","plaintiff"),
    `Outcome` = str_replace_all(`Outcome`,"plaintiffs","plaintiff"),
    `Outcome` = str_replace_all(`Outcome`,"plaintifff","plaintiff"),
    `Outcome` = str_replace_all(`Outcome`,"plaintff","plaintiff"),
    `Outcome` = str_replace_all(`Outcome`,"plainitff","plaintiff"),
    `Outcome` = str_replace_all(`Outcome`,"defenfant","defendant"),
    `Outcome` = str_replace_all(`Outcome`,"defendent","defendant"),
    `Outcome` = str_replace_all(`Outcome`,"defendants","defendant"),
    `Outcome` = str_replace_all(`Outcome`,"defenant","defendant"),
    `Outcome` = str_replace_all(`Outcome`,"defedant","defendant"),
    `Outcome` = str_replace_all(`Outcome`,"denied","defendant"),
    `Outcome` = str_replace_all(`Outcome`,"unsure","unknown"),
    `Outcome` = str_replace_all(`Outcome`,"unclear","unknown"),
    `Outcome` = str_replace_all(`Outcome`,"industry%individual","mixed"),
    `Outcome` = str_replace_all(`Outcome`,"neither all motions denied","mixed"),
    `Outcome` = str_replace_all(`Outcome`,"neither all motions defendant","mixed"),
    `Outcome` = str_replace_all(`Outcome`,"neither","mixed"),
    `Outcome` = str_replace_all(`Outcome`,"neither\\?","mixed"),
    `Outcome` = str_replace_all(`Outcome`,"mixed\\?","mixed")
  )


