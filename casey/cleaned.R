### installing packages, create data_clean -----

#packages

library(tidyverse)
library(stringr)

#load data

data <- read_csv("cases_coded_prelim_clean1.csv")

#create new df

df_og_tojoin <- data

### join with correct pt_types ----
#load corrected data
data_ngo <- read_csv("/Users/caseyr/Downloads/full_data_ngo.csv")
#create df_to_join and select the columns to join
data_to_join <- data_ngo %>%
  select(
    `ID`,`ngo_env`,`ngo_big10`,`pt_type_new`) 
#join to new df (df_corrected)
df_corrected <- left_join(df_og_tojoin, data_to_join, by = "ID")
#remove old Plaintiff Types column
df_correct = subset(df_corrected, select = -c(`Plaintiff Types`))
#rename pt_type_new to old name (Plaintiff Types)
names(df_correct)[names(df_correct) == "pt_type_new"] <- "Plaintiff Types"
#reorder so Plaintiff Types is in the right place
df_inorder <- df_correct %>%
  select(
    `assigned`,`group`,`ID`,`case_name`,`url`,`cite`,`summary`,`jurisdiction`,`court`,`case_date`,`year`,`Plaintiffs`,`Plaintiff Types`,everything())

df_to_clean <- df_inorder
#make character
df_to_clean$ngo_big10 <- as.character(df_to_clean$ngo_big10)

### clean data for data_clean -------

#make blanks none
df_to_clean[is.na(df_to_clean)] <- "none"

data_clean <- df_to_clean %>%
  ### outcome to data_clean ---------
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
  ### plaintiff type and defendant type to data_clean ---------
data_clean <- data_clean %>% 
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
  ) %>%
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
  ### agencies to data_clean ---------
data_clean <- data_clean %>% 
  mutate(
  `Federal Agencies` = str_replace_all(`Federal Agencies`, "animal and plant health inspection services", "aphis"),
  `Federal Agencies` = str_replace_all(`Federal Agencies`, "animal and plant health inspection service", "aphis")
) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "appalachian regional commission", "arc"),
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
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "atomic energy commission", "aec"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of indian affairs", "bif"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of indian affairs", "bif"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of indian affairs", "bif"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "indian affairs", "bif"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "interior board of land appeals", "ibla"), 
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
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united state blm", "blm"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "office of legacy management", "blm"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "lands and mineral management", "blm"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of ocean energy management, regulation, and enforcement", "boem"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of ocean energy management", "boem"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of ocean energy management", "boem"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of ocean energy management", "boem"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of ocean energy enforcement", "boem"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of reclamation", "bor"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of reclamation", "bor"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us bureau of reclamation", "bor"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of reclamation", "bor"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of reclaimation", "bor"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bor", "bor"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "water resources council", "bor"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. department of reclamation", "bor"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of safety and environmental enforcement", "bsee"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of safety and environmental enforcement", "bsee"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of safety and environmental enforcement", "bsee"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "economic development administration", "eda"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "economic development agency", "eda"), 
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states environmental protection agency", "epa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. environmental protection agency", "epa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us environmental protection agency", "epa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. epa", "epa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us epa", "epa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "usepa", "epa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "untied states environmental protection agency", "epa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "environmental protection agency", "epa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "enivormental protection agency", "epa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "environmental crimes section", "epa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "environmental defense section", "epa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "environmental protection agenct", "epa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "nepa", "epa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal aviation administration", "faa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal energy regulatory commission", "ferc"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal power commission", "ferc"), 
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal emergency management agency", "fema"),
  ) %>%
  mutate( 
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us federal food and drug administration", "fda"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. federal food and drug administration", "fda"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal food and drug administration", "fda"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "food and drug administration", "fda"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us fda", "fda"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. fda", "fda"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal and drug administration", "fda"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal drug administration", "fda"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "food & drug administration", "fda"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. fmsa", "fda"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of drug abuse control", "fda"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "fmsa", "fda"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states federal highway administration", "fha"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. federal highway administration", "fha"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal highway administration", "fha"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal highway administratio", "fha"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal highway administrative", "fha"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal highway authority", "fha"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal highway commission", "fha"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal works and highways administration", "fha"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal housing finance agency", "fhfa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "fhfa;", "fhfa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal railroad administration", "fra"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal security agency", "fsa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal subsistance board", "fsb"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal trade commission", "ftc"),
  )
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal transit administration", "fta"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "fedreral transit administration", "fta"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal transit authority", "fta"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "urban mass transportation administration", "fta"),
  ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. fish and wildlife service", "fws"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "usfws", "fws"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "usfwl", "fws"),
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
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "us fws", "fws")
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
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. fs", "fs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us fs", "fs"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal home loan mortgage corporation", "fhlmc"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "fhlmc;", "fhlmc"),
    )%>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states general service administration", "gsa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. general service administration", "gsa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us general service administration", "gsa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "general service administration", "gsa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. general services administration", "gsa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "general services administration", "gsa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us geological survey", "usgs"), 
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "government accountibility office", "gao"), 
  ) %>%
  mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. minerals management service", "mms"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "minerals management service", "mms"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national aeronautics and space administration", "nasa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national marine fisheries servie", "nmfs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national marine fisheries service", "nmfs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "marine fisheries service", "nmfs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national fisheries service", "nmfs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "noaa fisheries", "nmfs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national marine fishieres service", "nmfs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national marine and fisheries service", "nmfs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national marine fisheries", "nmfs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "nmfs's", "nmfs"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "nmfsa", "nmfs"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national nuclear security administration", "nnsa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national oceanic and atmospheric administration", "noaa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national oceanic and atmospheric association", "noaa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national ocean and atmospheric association", "noaa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national oceanic atmospheric administration", "noaa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national oceanographic and atomspheric administration", "noaa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. national park service", "nps"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national park service", "nps"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national parks association", "nps"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national wildlife federation", "nps"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states park service", "nps"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. nuclear regulatory commission", "nrc"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states nuclear regulatory commission", "nrc"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "nuclear regulatory commission", "nrc"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "regulatory nuclear commission", "nrc"),
    ) %>% 
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "excutive office of budget and management", "omb"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states office of management and budget", "omb"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us office of management and budget", "omb"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. office of management and budget", "omb"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "the office of management and budget", "omb"),
    ) %>% 
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. office of surface mining reclamation and enforcement", "osmre"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "office of surface mining reclamation and enforcement", "osmre"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "office of surface mining, reclamation and enforcement", "osmre"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "office of surface mining", "osmre"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "pipeline and hazardous materials safety administration", "phmsa"), 
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "interstate commerce commision", "stb"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "interstate commerce commission", "stb"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "soil conservation service", "nrcs"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "rural utilities service", "usdard"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "redevelopment land agency", "usdard"),
    ) %>%
  #other
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "farm service agency", "fmsa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal crop insurance corp", "fcic"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "risk management agency", "rma"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "equal employment opportunity commission", "eeoc"),
  )
    ### full dylan agency code ------
  data_clean <- data_clean %>% 
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
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united state blm", "blm"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "office of legacy management", "blm"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "lands and mineral management", "blm"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of indian affairs", "bif"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of indian affairs", "bif"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of indian affairs", "bif"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "indian affairs", "bif"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of reclamation", "bor"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of reclamation", "bor"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "us bureau of reclamation", "bor"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of reclamation", "bor"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of reclaimation", "bor"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bor", "bor"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "water resources council", "bor"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. department of reclamation", "bor"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of ocean energy management, regulation, and enforcement", "boem"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of ocean energy management", "boem"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of ocean energy management", "boem"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of ocean energy management", "boem"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of ocean energy enforcement", "boem"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of safety and environmental enforcement", "bsee"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of safety and environmental enforcement", "bsee"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of safety and environmental enforcement", "bsee"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states environmental protection agency", "epa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. environmental protection agency", "epa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "us environmental protection agency", "epa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. epa", "epa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "us epa", "epa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "usepa", "epa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "untied states environmental protection agency", "epa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "environmental protection agency", "epa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "enivormental protection agency", "epa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "environmental crimes section", "epa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "environmental defense section", "epa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "environmental protection agenct", "epa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "nepa", "epa"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "economic development administration", "eda"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "economic development agency", "eda"), 
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "equal employment opportunity commission", "eeoc"),
    ) %>% #### Oddities
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "farm service agency", "fmsa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal crop insurance corp", "fcic"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "risk management agency", "rma"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal railroad administration", "fra"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal security agency", "fsa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal subsistance board", "fsb"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal trade commission", "ftc"),
  ) %>%
    mutate( 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "us federal food and drug administration", "fda"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. federal food and drug administration", "fda"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal food and drug administration", "fda"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "food and drug administration", "fda"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "us fda", "fda"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. fda", "fda"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal and drug administration", "fda"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal drug administration", "fda"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "food & drug administration", "fda"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. fmsa", "fda"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of drug abuse control", "fda"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal aviation administration", "faa"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal emergency management agency", "fema"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states federal highway administration", "fha"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. federal highway administration", "fha"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal highway administration", "fha"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal highway administratio", "fha"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal highway administrative", "fha"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal highway authority", "fha"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal highway commission", "fha"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal works and highways administration", "fha"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal housing finance agency", "fhfa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal home loan mortgage corporation", "fhlmc"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal transit administration", "fta"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "fedreral transit administration", "fta"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal transit authority", "fta"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "urban mass transportation administration", "fta"),
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
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. fs", "fs"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "us fs", "fs"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. fish and wildlife service", "fws"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "usfws", "fws"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "usfwl", "fws"),
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
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "us fws", "fws")
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states general service administration", "gsa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. general service administration", "gsa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "us general service administration", "gsa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "general service administration", "gsa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. general services administration", "gsa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "general services administration", "gsa"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national aeronautics and space administration", "nasa"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national marine fisheries servie", "nmfs"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national marine fisheries service", "nmfs"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "marine fisheries service", "nmfs"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national fisheries service", "nmfs"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "noaa fisheries", "nmfs"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national marine fishieres service", "nmfs"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national marine and fisheries service", "nmfs"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national marine fisheries", "nmfs"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "nmfs's", "nmfs"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "nmfsa", "nmfs"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national oceanic and atmospheric administration", "noaa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national oceanic and atmospheric association", "noaa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national ocean and atmospheric association", "noaa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national oceanic atmospheric administration", "noaa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national oceanographic and atomspheric administration", "noaa"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. national park service", "nps"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national park service", "nps"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national parks association", "nps"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national wildlife federation", "nps"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states park service", "nps"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. department of the army", "usa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states department of the army", "usa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of the army", "usa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of army", "usa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states army", "usa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "army", "usa"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. department of the air force", "usaf"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states department of the air force", "usaf"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of the air force", "usaf"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of air force", "usaf"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "air force", "usaf")
    ) %>% 
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. department of the navy", "usn"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states department of the navy", "usn"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of the navy", "usn"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of navy", "usn"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states navy", "usn"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "navy", "usn"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "naval facilities engineering command", "usn"),
    ) %>% 
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. department of the coast guard", "uscg"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states department of the coast gaurd", "uscg"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of the coast guard", "uscg"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of coast guard", "uscg"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states coast guard", "uscg"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. coast guard", "uscg"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "us coast guard", "uscg"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "coast guard", "uscg"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "coast gaurd", "uscg"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states uscg", "uscg"),
    ) %>% 
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. department of the marine corps", "usmc"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states department of the marine corps", "usmc"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of the marines", "usmc"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of marines", "usmc"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states marine corps", "usmc"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states marine corp", "usmc"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. marine corps", "usmc"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "marine corps", "usmc"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national guard", "usarng"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "advisory council on historic preservation", "achp"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. minerals management service", "mms"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "minerals management service", "mms"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. nuclear regulatory commission", "nrc"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states nuclear regulatory commission", "nrc"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "nuclear regulatory commission", "nrc"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "regulatory nuclear commission", "nrc"),
    ) %>% 
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. office of surface mining reclamation and enforcement", "osmre"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "office of surface mining reclamation and enforcement", "osmre"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "office of surface mining, reclamation and enforcement", "osmre"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "office of surface mining", "osmre"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "excutive office of budget and management", "omb"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states office of management and budget", "omb"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "us office of management and budget", "omb"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. office of management and budget", "omb"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "the office of management and budget", "omb"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "board of immigration appeals", "doj"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "us bureau of prisons", "doj"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of prisons", "doj"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of prisons", "doj"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal energy administration", "doe"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of energy", "doe"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "office of clean energy systems", "doe"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "western area power administration of the us doe", "doe"), 
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal energy regulatory commission", "ferc"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal power commission", "ferc"), 
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "interstate land sales", "hud"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "housing and urban development", "hud"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of hud", "hud"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. customs and border patrol", "uscbp"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. immigration and custom enforcement", "uscbp"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. immigration and customs enforcement", "uscbp"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states defense energy support center", "dod"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "defense logistics agency", "dod"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "defense", "dod"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states housing authority", "hud"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states defense energy support center", "dod"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "advidory council on historic preservation", "achp"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "interstate commerce commision", "stb"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "interstate commerce commission", "stb"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "medicaid agency", "cms"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national nuclear security administration", "nnsa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national security agency", "nsa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "national capital planning commission", "ncpc"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "interior board of land appeals", "ibla"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "npne", "none"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "occupational safety and health administration", "osha"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "pipeline and hazardous materials safety administration", "phmsa"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "the united states postal service", "usps"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states postal service", "usps"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. postal service", "usps"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "us postal service", "usps"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "postal service", "usps"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "government accountibility office", "gao"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "rural utilities service", "usdard"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "redevelopment land agency", "usdard"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "soil conservation service", "usdanrcs"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states small business administration", "sba"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "small business administration", "sba"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "council on environmental quality", "eop"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "tennessee valley authority", "tva"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "securities and exchange commission", "sec"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "veterans administration center", "va"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states marshal", "usms"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "urban mass transportation administration", "dot"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. department of commerce", "doc"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "unknown", "none"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "agency for international develoment", "aid"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "commission of fine arts", "cfa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of agriculture", "usda"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of health, education and welfare", "hhs"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of homeland security", "dhs"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of labor", "dol"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of transportation", "dot"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "us dot", "dot"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "american battle monuments commission", "abmc"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "delaware river basin", "nps"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states department of the interior", "doi"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. department of the interior", "doi"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "us department of the interior", "doi"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of the interior", "doi"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "department of interior", "doi"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "interior", "doi"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "dept of interios", "doi"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states deparement of doi", "doi"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. doi", "doi"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "doj\\?", "doj"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "fws, et al.", "fws"), 
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "international boundary and water comission of the united states", "dod"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal national mortgage association", "none"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "fhfa;", "fhfa"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "fhlmc;", "fhlmc"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "fmsa", "fda"),
    ) %>%
    mutate(
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "% ", "%"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, " %", "%"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, " &", "%"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "& ", "%"),
      `Federal Agencies` = str_replace_all(`Federal Agencies`, "&", "%"),
    )
  ### statutes to data_clean -----------
  data_clean <- data_clean %>% 
  mutate(
  ### solid waste disposal act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "soild waste disposal act", "solid waste disposal act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "solid waste and disposal act", "solid waste disposal act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "solid waste disposal", "solid waste disposal act"),
  ### federal water pollution control act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal water pollution control actmarine protection, research, and sanctuaries act", "fwpca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal water pollution control actmarine protection, research, and sanctuaries act", "fwpca$mprsa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal water pollution controal act", "fwpca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal water pollution act", "fwpca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal water pollution, prevention and control act", "fwpca"),
  ### clean water act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "clean water act", "cwa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "clean water act", "cwa"),
  ### clean air act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "clean air act", "caa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "clean air act of texas", "caa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "clean act act", "caa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "clean act act", "caa"),
  ### cercla/superfund (all cleaned to cercla)
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response, compensation, and liability act of 1980", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response, compensation, and liability act", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response, compensation and liability act", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response compensation and liability act", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response and liability act", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response act", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response, compensation and liability act", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response act", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response, compensation and liability act", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "superfundcercla", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "cerlca", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "cerlca", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "cerla", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "cerclar", "cercla"),
  #account for superfund and cercla in the same cell (if cell contains superfund and cercla delete superfund)
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "cercla & superfund", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "cercla$cwa", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "superfund act", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "superfund", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "superfun", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "cercla%cercla", "cercla"),
  ### national environmental policy act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "nepa&national forest management act", "nepa$nfma"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "esa&nepa", "nepa$esa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "national environmental policy act", "nepa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "environmental policy act", "nepa"),
  ### endangered species act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "endangered species act", "esa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "endangered species", "esa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "endangered  species act", "esa"),
  ### federal food, drug, and cosmetic act/federal food, drug and cosmetic act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal, food, drug, and cosmetic act", "ffdca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal food, drug, and cosmetic act,", "ffdca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal food, drug, and cosmetic act", "ffdca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal food, drug, and cosmetic ac", "ffdca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal food, drug, and cosmestic act", "ffdca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal food, drug and cosmetic act", "ffdca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "food, drug, and cosmetic act", "ffdca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "food, drug and cosmetic act", "ffdca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "food drug and cosmetic act", "ffdca"),
  ### resource conservation and recovery act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "resources conservation and recovery act", "rcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "resource, conservation and recovery act", "rcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "resource conservation and recovery act", "rcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "resource conservation and reclamation act", "rcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "resource recovery and conservation act", "rcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "resource conservation recovery act", "rcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "resouce conservation and recovery act", "rcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "resource conservation and recovert act", "rcra"),
  ### national forest management act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "national forest management act", "nfma"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "national forest and management act", "nfma"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "national forest maangement act", "nfma"),
  ### federal insectide, fungicide, and rodenticide act/federal insectide, fungicide and rodenticide act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal insecticide, fungicide, and rodenticide act", "fifra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal insecticide, fungicide, and rodentcide act", "fifra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal insecticide, fungicide and rodenticide act", "fifra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal insecticide, fugicide, and rodenticide act", "fifra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal insecticide fungicide and rodenticide act", "fifra"),
  ### atomic energy act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "atomic energy act", "aea"),
  ### reclamation Act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "the reclamation act", "reclamation act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "reclamation act none", "reclamation act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal reclamation act", "reclamation act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "reclaimation act", "reclamation act"),
  ### rivers and harbors act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "rivers and harbor act", "rivers and harbors act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "river and harbors act", "rivers and harbors act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal rivers and harbors act", "rivers and harbors act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, " the rivers and harbors act", "rivers and harbors act"),
  ### toxic substances control act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "toxic substance control act", "tsca"),
  ### safe water drinking act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "safe water drinking program", "swda"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "safe drinking water act", "swda"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "safe drinking water", "swda"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "safe water drinking act", "swda"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "safe drinking act", "swda"),
  ### federal land policy and management act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy and management act of 1976", "flpma"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy management act", "flpma"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy and management act", "flpma"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy and management act's", "flpma"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy & management act", "flpma"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "the federal land policy and management act", "flpma"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy and forest management act", "flpma"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy and exchange management act", "flpma"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy and managment act", "flpma"),
  ### oil pollution act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "oil polution act", "opa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "oil pollution act", "opa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "oil pollution control act", "opa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal oil pollution act", "opa"),
  ### surface mining control and reclamation act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "surface mining control and reclamation act", "smcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "surface mining control and reclamation act of 1977", "smcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "surface mining control and reclaimation act", "smcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "surface mining conservation and reclamation act", "smcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "surface mining and reclamation act", "smcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "surface coal mining land conservation and reclamation act", "smcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "surface mining and control reclamation act", "smcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "mining control and reclaimation act", "smcra"),
  ### wilderness act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "national wilderness act", "wilderness act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "wilderness", "wilderness act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "the wilderness act", "wilderness act"),
  ### migratory bird treaty act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "migrtory bird treaty act", "mbta"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "migratory bird treaty", "mbta"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "migratory bird treaty act", "mbta"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "migratory bird act", "mbta"),
  ### marine mammal protection act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "marine mammal protection act", "mmpa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "marine mammal protection act of 1972", "mmpa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "marine mamal protection act", "mmpa"),
  ### coastal zone management act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "coastal zone management act", "czma"),
  ### energy policy act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "energy policy act", "energy policy act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "energy poilcy act", "energy policy act"),
  ### magnuson-stevens act/fishery conservation and management act/magnuson-stevens fishery conservation and management act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnuson fishery conservation and management act", "msa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnuson-stevens fishery conservation and management act", "msa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnuson-stevens fishery and conservation act", "msa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnunson-stevens fishery conservation and management act", "msa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "fishery conservation and management act", "msa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnuson stevens fishery conservation and management act", "msa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnison stevens fishery conservation and management act", "msa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnuson stevens fisheries management and conservation act", "msa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnuson_stevens fishery conservation and management act", "msa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "fishery conservatino and management act", "msa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnuson-stevens fishery conservation act and management act", "msa"),
  ### emergency planning and community right-to-know act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "emergency planning and community right to know act", "epcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "emergency planning and community right-to-know act", "epcra"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "emergency planning and community right-to-knoe act", "epcra"),
  ### marine protection, research, and sanctuaries act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "marine protection,research, and sanctuaries act", "mprsa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "marine protection, research, and sanctuaries act", "mprsa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "marine protection, research and sanctuaries act", "mprsa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "marine protection research and sanctuaries act", "mprsa"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, " marine protection, research, and sanctuaries act", "mprsa"),
  ### ocean dumping act (no corrections needed)
  
  ### the multiple use-sustained-yield act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "multiple use-sustained yield act", "the multiple use sustained yield act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "multiple-use sustained yield act", "the multiple use sustained yield act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "multiple-use sustained-yield act", "the multiple use sustained yield act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "multiple use sustained yield act", "the multiple use sustained yield act"),
  ### noise control act/noise pollution and abatement act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "noise control act", "nca"),
  ### fish and wildlife coordination act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "fish and wildlife coordination act", "fwca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal wildlife coordination act", "fwca"),
  ### nuclear waste policy act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "nuclear waste policy act", "nwpa"),
  ### national park service organic act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "national park service organic act", "national park service organic act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "national park services organic act", "national park service organic act"),
  ### energy independence and security act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "energy independence and security act", "eisa"),
  ### fish and wildlife act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "fish and wildlife act", "fish and wildlife act"),
  ### pollution prevention act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "pollution prevention act", "ppa"),
  ### antiquities act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "antiquities act", "antiquities act"),
  ### federal power act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal power act", "fpa"),
  ### lacey act (no corrections needed)
  
  ###mineral leasing act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "mineral rights act", "mla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "mineral leasing act", "mla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "mineral lands leasing act", "mla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "mineral land leasing act", "mla"),
  ### wild and scenic rivers act
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "wild and scenie rivers act", "wild and scenic rivers act"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "national wild and scenic rivers act", "wild and scenic rivers act"),
  ### the organic act (no corrections needed)
  
  ### none
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "\\bnon\\b", "none"),
) 
  ### states to data_clean ----------
  data_clean <- data_clean %>% 
  mutate(
  `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "dc", "district of columbia"),
  `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "d.c.", "district of columbia"),
  `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "washington dc", "district of columbia"),
  `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "washington, d.c.", "district of columbia"),
  `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "washington d.c.", "district of columbia"),
  `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "washington district of columbia", "district of columbia"),
  `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "columbia", "district of columbia"),
  `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "district of district of columbia", "district of columbia"),
  `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "washington, district of columbia", "district of columbia")
) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "eastern district of tennessee", "tennessee"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "northern district of ohio", "ohio"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "western district of kentucky", "kentucky"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "western district of texas", "texas")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "state of illinois", "illinois")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "west", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "west virginia virginia", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "west virginia virigina", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, ", bluefield", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "west virginiaern", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "west virginiawest virginia", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "american west virginia", "west virginia"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "west virginia great lakes region", "west virginia")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "pacific northwest virginia", "virginia")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "califonia", "california"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "californa", "california"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "califronia", "california"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "california, san francisco", "california")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "district of guam", "guam"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "district of virgin islands", "virgin islands"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "district of washington", "washington")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "gulf of mexico", "louisiana")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "conneticut", "connecticut"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "connnecticut", "connecticut")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "newjersey", "new jersey")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "masachusetts", "massachusetts"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "massachuetts", "massachusetts"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "massachussetts", "massachusetts"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "massachuttes", "massachusetts")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "deleware", "delaware"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "delware", "delaware")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "hawai'i", "hawaii")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "illionois", "illinois")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "louisianna", "louisiana"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "lousiana", "louisiana")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "michagan", "michigan")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "missouris", "missouri")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "new meico", "new mexico")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "new york%south africa", "new york")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "okahoma", "oklahoma")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "pennslyvania", "pennsylvania")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "puerto rice", "puerto rico")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "rhoade island", "rhode island")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "south caroline", "south carolina")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "texas%mexico", "texas")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "nationwide", "united states")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "unknown", "oregon")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "viriginia", "virginia")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "alaksa", "alaska")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "antartica", "antarctica")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "u.s. fish and wildlife service", "alaska"),
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "clean water act", "west virginia")
  ) %>%
  mutate(
    `Location (state) of conflict` = str_replace_all(`Location (state) of conflict`, "pennsylvania%several", "several")
  ) %>%
  mutate(
    `Location (state) of conflict` = ifelse(ID == "2015-0139-Angler-001" & `Location (state) of conflict` == "northeast", "new england states", `Location (state) of conflict`),
    `Location (state) of conflict` = ifelse(ID == "2009-0632-Gonzal-001" & `Location (state) of conflict` == "south", "texas%gulf of mexico", `Location (state) of conflict`),
    `Location (state) of conflict` = ifelse(ID == "2017-0257-N.D.eX-001" & `Location (state) of conflict` == "dakota", "north dakota", `Location (state) of conflict`),
    `Location (state) of conflict` = ifelse(ID == "1976-0415-Maryla-001" & `Location (state) of conflict` == "northeast", "new jersey%pennsylvania", `Location (state) of conflict`),
    `Location (state) of conflict` = ifelse(ID == "1998-1998-Americ-003" & `Location (state) of conflict` == "carolina", "north carolina", `Location (state) of conflict`)
  )
  ### species to data_clean ---------
  data_clean <- data_clean %>% 
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
  ) %>%
  mutate(
    Species = str_replace_all(Species, "% ", "%")
  ) %>%
  mutate(
    Species = str_replace_all(Species, " %", "%")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "beldings savannah sparrow.", "beldings savannah sparrow")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "puget sound steelhead trout", "steelhead trout"),
    Species = str_replace_all(Species, "cv steelhead trout", "steelhead trout"),
    Species = str_replace_all(Species, "lower columbia river steelhead trout", "steelhead trout"),
    Species = str_replace_all(Species, "snake river steelhead trout", "steelhead trout"),
    Species = str_replace_all(Species, "central valley steelhead trout", "steelhead trout"),
    Species = str_replace_all(Species, "central california coast steelhead trout", "steelhead trout"),
    Species = str_replace_all(Species, "california central valley steelhead trout", "steelhead trout"),
    Species = str_replace_all(Species, "pacific steelhead trout", "steelhead trout"),
    Species = str_replace_all(Species, "steelhead trout fish", "steelhead trout"),
    Species = str_replace_all(Species, "chinook salmon steelhead trout", "steelhead trout"),
    Species = str_replace_all(Species, "california steelhead trout", "steelhead trout"),
    Species = str_replace_all(Species, "coast cutthorat trout", "coastal cutthroat trout"),
    Species = str_replace_all(Species, "columbia river bull trout", "bull trout")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "unknown", "unspecified"),
    Species = str_replace_all(Species, "none mentioned", "none")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "the california brown pelican", "brown pelican")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "northern goshowak", "northern goshawk")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "norther long-eared bat", "northern long-eared bat")
  ) %>%
  mutate(
    Species = str_replace_all(Species, "migritory birds%", "migratory birds")
  ) %>%
  mutate(
    Species = str_replace_all(Species, ",%", "%")
  ) %>%
  mutate(
    Species = ifelse(ID == "2018-0354-BlackX-001" & Species == "alabama moccasinshell%orangenacre mucket%ovate clubshell%upland combshell%triangular kidneyshell%dark pigtoe%plicate rocksnail%black warrior waterdog%flattened musk turtle%rush darter%cahaba shiner%indiana bat%northern long-eared bat", 
                     "freshwater mussel%dark pigtoe%plicate rocksnail%black warrior waterdog%flattened musk turtle%rush darter%cahaba shiner%indiana bat%northern long-eared bat", Species),
    Species = ifelse(ID == "2008-2008-Ctr.fX-013" & Species == "cauca guan%cantabrian capercaillie%gorgeted wood-quail%takahe%chatham island oystercatcher%marquesan imperial-pigeon%orange-fronted parakeet%uvea parakeet%southeastern rufous-vented ground cuckoo%chilean woodstar%margaretta's hermit%okinawa woodpecker%black-hooded antwren%fringe-backed fire-eye%st. lucia forest thrush%eiao polynesian warbler%codfish island fernbird%gizo white-eye%cherry-throated tanager%lord howe currawong%junin flightless grebe%greater adjutant stork%andean flamingo%brazilian merganser%southern helmeted curassow%blue-billed curassow%bogota rail%junin rail%jerdon's courser%slender-billed curlew%salmon-crested cockatoo%blue-throated macaw%black-breasted puffleg%esmeraldas woodstar%yellow-browed toucanet%helmeted woodpecker%royal cinclodes%white-browed tit-spinetail%brown-banded antpitta%brasilia tapaculo%kaempfer's tody-tyrant%ash-breasted tit-tyrant%peruvian plantcutter%medium tree-finch%black-backed tanager%harris' mimic swallowtail%jamaican kite swallowtail%fluminese swallowtail%hahnel's amazonian swallowtail%kaiser-i-hind swallowtail%fiji petrel%chatham petrel%cook's petrel%galapagos petrel%magenta petrel%heinroth's shearwater", 
                     "butterflies%birds%seabirds%neotropical birds%flamingo%wading birds%grebe%grouse%parakeets%curassows%quail%flightless birds%pigeon%warbler%passerine birds", Species),
    Species = ifelse(ID == "2014-010F-Massac-001" & Species == "georges bank cod%gulf of maine cod%georges bank haddock%gulf of maine haddock%georges bank yellowtail flounder%southern new england/mid-atlantic yellowtail flounder%cape cod/gulf of maine yellowtail flounder%american plaice%witch flounder%georges bank winter flounder%gulf of maine winter flounder%southern new england/mid-atlantic winter flounder%redfish%white hake%pollock%northern windowpane flounder%southern windowpane flounder%ocean pout%atlantic halibut%atlantic wolffish",
                     "cod%haddock%yellowtail flounder%american plaice%witch flounder%winter flounder%redfish%white hake%pollock%windowpane flounder%ocean pout%atlantic halibut%atlantic wolffish", Species),
    Species = ifelse(ID == "2011-0808-InDefX-001" & Species == "no", "none", Species),
    Species = ifelse(ID == "1999-1999-FERCvX-001" & Species == "no", "none", Species),
    Species = ifelse(ID == "1989-1989-Whiten-001" & Species == "no", "none", Species),
    Species = ifelse(ID == "2009-0643-SouthX-001" & Species == "no", "none", Species),
    Species = ifelse(ID == "1999-044F-DelWeX-001" & Species == "no", "none", Species),
    Species = ifelse(ID == "2019-2019-Standi-001" & Species == "no", "none", Species)
  )
  ### non-environmental ton to data_clean -------------
  data_clean <- data_clean %>% 
    mutate(
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-envrionmental","non-environmental"),
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-environemntal","non-environmental"),
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-environemental\\?","non-environmental"),
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-environemental","non-environmental"),
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-envirnomental","non-environmental"),
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-enviornmental","non-environmental"),
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-enviornemental","non-environmental"),
    `Type of Nature` = str_replace_all(`Type of Nature`,"non-enironmental","non-environmental")
  )
  ### deference to data_clean ----
data_clean <- data_clean %>%  
  mutate(
      deference = str_replace_all(deference,"scientific expertise","yes"),
      deference = str_replace_all(deference,"Yes","yes"),
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
### add columns for analysis with 0 and 1 --------
  ### plaintiff and defendant type with 0 and 1 -----------

#split plaintiff types into columns by type, 1 if type is contained, 0 if not

data_clean$pt_fed <- ifelse(grepl("fed",data_clean$`Plaintiff Types`, ignore.case = T),"1","0")
data_clean$pt_state <- ifelse(grepl("state",data_clean$`Plaintiff Types`, ignore.case = T),"1","0")
data_clean$pt_local <- ifelse(grepl("local",data_clean$`Plaintiff Types`, ignore.case = T),"1","0")
data_clean$pt_military <- ifelse(grepl("military",data_clean$`Plaintiff Types`, ignore.case = T),"1","0")
data_clean$pt_civic_assn <- ifelse(grepl("civic_assn",data_clean$`Plaintiff Types`, ignore.case = T),"1","0")
data_clean$pt_ngo <- ifelse(grepl("ngo",data_clean$`Plaintiff Types`, ignore.case = T),"1","0")
data_clean$pt_union <- ifelse(grepl("union",data_clean$`Plaintiff Types`, ignore.case = T),"1","0")
data_clean$pt_individual <- ifelse(grepl("individual",data_clean$`Plaintiff Types`, ignore.case = T),"1","0")
data_clean$pt_industry <- ifelse(grepl("industry",data_clean$`Plaintiff Types`, ignore.case = T),"1","0")
data_clean$pt_trade_assn <- ifelse(grepl("trade_assn",data_clean$`Plaintiff Types`, ignore.case = T),"1","0")
data_clean$pt_tribe <- ifelse(grepl("tribe",data_clean$`Plaintiff Types`, ignore.case = T),"1","0")
data_clean$pt_public_org <- ifelse(grepl("public_org",data_clean$`Plaintiff Types`, ignore.case = T),"1","0")
data_clean$pt_religious_org <- ifelse(grepl("religious_org",data_clean$`Plaintiff Types`, ignore.case = T),"1","0")
data_clean$pt_other <- ifelse(grepl("other",data_clean$`Plaintiff Types`, ignore.case = T),"1","0")

# make pt sum column
data_clean$pt_fed <- as.numeric(as.character(data_clean$pt_fed))
data_clean$pt_state <- as.numeric(as.character(data_clean$pt_state))
data_clean$pt_local <- as.numeric(as.character(data_clean$pt_local))
data_clean$pt_military <- as.numeric(as.character(data_clean$pt_military))
data_clean$pt_civic_assn <- as.numeric(as.character(data_clean$pt_civic_assn))
data_clean$pt_ngo <- as.numeric(as.character(data_clean$pt_ngo))
data_clean$pt_union <- as.numeric(as.character(data_clean$pt_union))
data_clean$pt_individual <- as.numeric(as.character(data_clean$pt_individual))
data_clean$pt_industry <- as.numeric(as.character(data_clean$pt_industry))
data_clean$pt_trade_assn <- as.numeric(as.character(data_clean$pt_trade_assn))
data_clean$pt_tribe <- as.numeric(as.character(data_clean$pt_tribe))
data_clean$pt_public_org <- as.numeric(as.character(data_clean$pt_public_org))
data_clean$pt_religious_org <- as.numeric(as.character(data_clean$pt_religious_org))
data_clean$pt_other <- as.numeric(as.character(data_clean$pt_other))

data_clean$pt_count <- data_clean$pt_fed + data_clean$pt_state + data_clean$pt_local + data_clean$pt_military + 
  data_clean$pt_civic_assn + data_clean$pt_ngo + data_clean$pt_union + data_clean$pt_individual + 
  data_clean$pt_industry + data_clean$pt_trade_assn + data_clean$pt_tribe + data_clean$pt_public_org + 
  data_clean$pt_religious_org + data_clean$pt_other


#split defendant types into columns by type, 1 if type is contained, 0 if not

data_clean$dt_fed <- ifelse(grepl("fed",data_clean$`Defendant Types`, ignore.case = T),"1","0")
data_clean$dt_state <- ifelse(grepl("state",data_clean$`Defendant Types`, ignore.case = T),"1","0")
data_clean$dt_local <- ifelse(grepl("local",data_clean$`Defendant Types`, ignore.case = T),"1","0")
data_clean$dt_military <- ifelse(grepl("military",data_clean$`Defendant Types`, ignore.case = T),"1","0")
data_clean$dt_civic_assn <- ifelse(grepl("civic_assn",data_clean$`Defendant Types`, ignore.case = T),"1","0")
data_clean$dt_ngo <- ifelse(grepl("ngo",data_clean$`Defendant Types`, ignore.case = T),"1","0")
data_clean$dt_union <- ifelse(grepl("union",data_clean$`Defendant Types`, ignore.case = T),"1","0")
data_clean$dt_individual <- ifelse(grepl("individual",data_clean$`Defendant Types`, ignore.case = T),"1","0")
data_clean$dt_industry <- ifelse(grepl("industry",data_clean$`Defendant Types`, ignore.case = T),"1","0")
data_clean$dt_trade_assn <- ifelse(grepl("trade_assn",data_clean$`Defendant Types`, ignore.case = T),"1","0")
data_clean$dt_tribe <- ifelse(grepl("tribe",data_clean$`Defendant Types`, ignore.case = T),"1","0")
data_clean$dt_public_org <- ifelse(grepl("public_org",data_clean$`Defendant Types`, ignore.case = T),"1","0")
data_clean$dt_religious_org <- ifelse(grepl("religious_org",data_clean$`Defendant Types`, ignore.case = T),"1","0")
data_clean$dt_other <- ifelse(grepl("other",data_clean$`Defendant Types`, ignore.case = T),"1","0")

# make dt sum column
data_clean$dt_fed <- as.numeric(as.character(data_clean$dt_fed))
data_clean$dt_state <- as.numeric(as.character(data_clean$dt_state))
data_clean$dt_local <- as.numeric(as.character(data_clean$dt_local))
data_clean$dt_military <- as.numeric(as.character(data_clean$dt_military))
data_clean$dt_civic_assn <- as.numeric(as.character(data_clean$dt_civic_assn))
data_clean$dt_ngo <- as.numeric(as.character(data_clean$dt_ngo))
data_clean$dt_union <- as.numeric(as.character(data_clean$dt_union))
data_clean$dt_individual <- as.numeric(as.character(data_clean$dt_individual))
data_clean$dt_industry <- as.numeric(as.character(data_clean$dt_industry))
data_clean$dt_trade_assn <- as.numeric(as.character(data_clean$dt_trade_assn))
data_clean$dt_tribe <- as.numeric(as.character(data_clean$dt_tribe))
data_clean$dt_public_org <- as.numeric(as.character(data_clean$dt_public_org))
data_clean$dt_religious_org <- as.numeric(as.character(data_clean$dt_religious_org))
data_clean$dt_other <- as.numeric(as.character(data_clean$dt_other))

data_clean$dt_count <- data_clean$dt_fed + data_clean$dt_state + data_clean$dt_local + data_clean$dt_military + 
  data_clean$dt_civic_assn + data_clean$dt_ngo + data_clean$dt_union + data_clean$dt_individual + 
  data_clean$dt_industry + data_clean$dt_trade_assn + data_clean$dt_tribe + data_clean$dt_public_org + 
  data_clean$dt_religious_org + data_clean$dt_other

  ### agency with 0 and 1 --------
data_clean$fa_aphis <- ifelse(grepl("aphis",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_arc <- ifelse(grepl("arc",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_aec <- ifelse(grepl("aec",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_acoe <- ifelse(grepl("acoe",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_aec <- ifelse(grepl("aec",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_bif <- ifelse(grepl("bif",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_ibla <- ifelse(grepl("ibla",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_blm <- ifelse(grepl("blm",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_boem <- ifelse(grepl("boem",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_bor <- ifelse(grepl("bor",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_bsee <- ifelse(grepl("bsee",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_eda <- ifelse(grepl("eda",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_epa <- ifelse(grepl("epa",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_faa <- ifelse(grepl("faa",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_ferc <- ifelse(grepl("ferc",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fema <- ifelse(grepl("fema",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fda <- ifelse(grepl("fda",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fha <- ifelse(grepl("fha",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fhfa <- ifelse(grepl("fhfa",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fra <- ifelse(grepl("fra",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fsa <- ifelse(grepl("fsa",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fsb <- ifelse(grepl("fsb",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_ftc <- ifelse(grepl("ftc",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fta <- ifelse(grepl("fta",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fws <- ifelse(grepl("fws",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fs <- ifelse(grepl("fs",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fhlmc <- ifelse(grepl("fhlmc",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_gsa <- ifelse(grepl("gsa",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_usgs <- ifelse(grepl("usgs",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_gao <- ifelse(grepl("gao",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_mms <- ifelse(grepl("mms",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_nasa <- ifelse(grepl("nasa",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_nmfs <- ifelse(grepl("nmfs",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_nnsa <- ifelse(grepl("nnsa",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_noaa <- ifelse(grepl("noaa",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_nps <- ifelse(grepl("nps",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_nrc <- ifelse(grepl("nrc",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_omb <- ifelse(grepl("omb",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_osmre <- ifelse(grepl("osmre",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_phmsa <- ifelse(grepl("phmsa",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_stb <- ifelse(grepl("stb",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_nrcs <- ifelse(grepl("usdanrcs",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_usdard <- ifelse(grepl("usdard",data_clean$`Federal Agencies`, ignore.case = T),"1","0")

# create fa_other column
data_clean$fa_other <- ifelse(grepl('achp|aid|abmc|cms|eeoc|eop|bop|tva|cfa|uscbp|fmsa|fcic|ncpc|nsa|rma|sec|sba',data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_military <- ifelse(grepl('usaf|usa|uscg|usarng|usn|usms',data_clean$`Federal Agencies`, ignore.case = T),"1","0")

  ### statues with 0 and 1 ------
data_clean$statutes_solidwaste <- ifelse(grepl("solid waste disposal act",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_fwpca <- ifelse(grepl("fwpca",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_cwa <- ifelse(grepl("cwa",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_caa <- ifelse(grepl("caa",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_cercla <- ifelse(grepl("cercla",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_nepa <- ifelse(grepl("nepa",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_esa <- ifelse(grepl("esa",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_ffdca <- ifelse(grepl("ffdca",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_rcra <- ifelse(grepl("rcra",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_nfma <- ifelse(grepl("nfma",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_fifra <- ifelse(grepl("fifra",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_aea <- ifelse(grepl("aea",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_reclamationact <- ifelse(grepl("reclamation act",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_riversandharbors <- ifelse(grepl("rivers and harbors act",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_tsca <- ifelse(grepl("tsca",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_swda <- ifelse(grepl("swda",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_flpma <- ifelse(grepl("flpma",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_opa <- ifelse(grepl("opa",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_smcra <- ifelse(grepl("smcra",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_wildernessact <- ifelse(grepl("wilderness act",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_mbta <- ifelse(grepl("mbta",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_mmpa <- ifelse(grepl("mmpa",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_czma <- ifelse(grepl("czma",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_energypolicyact <- ifelse(grepl("energy policy act",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_msa <- ifelse(grepl("msa",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_epcra <- ifelse(grepl("epcra",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_mprsa <- ifelse(grepl("mprsa",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_ocean <- ifelse(grepl("ocean dumping act",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_multipleuse <- ifelse(grepl("the multiple use sustained yield act",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_nca <- ifelse(grepl("nca",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_fwca <- ifelse(grepl("fwca",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_nwpa <- ifelse(grepl("nwpa",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_nationalpsoa <- ifelse(grepl("national park service organic act",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_eisa <- ifelse(grepl("eisa",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_fishandwildlifeact <- ifelse(grepl("fish and wildlife act",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_ppa <- ifelse(grepl("ppa",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_antiquitiesact <- ifelse(grepl("antiquities act",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_fpa <- ifelse(grepl("fpa",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_lacey <- ifelse(grepl("lacey act",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_mla <- ifelse(grepl("mla",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_wildandscenic <- ifelse(grepl("wild and scenic rivers act",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_organic <- ifelse(grepl("the organic act",data_clean$`Federal Statutes`, ignore.case = T),"1","0")
data_clean$statutes_none <- ifelse(grepl("none",data_clean$`Federal Statutes`, ignore.case = T),"1","0")

  ### outcomes with 0 and 1 -----
data_clean$outcome_defendant <- ifelse(grepl("defendant",data_clean$`Outcome`, ignore.case = T),"1","0")
data_clean$outcome_plaintiff <- ifelse(grepl("plaintiff",data_clean$`Outcome`, ignore.case = T),"1","0")
data_clean$outcome_mixed <- ifelse(grepl("mixed",data_clean$`Outcome`, ignore.case = T),"1","0")
  ### deference with 0 and 1 ----
data_clean$deference_yes <- ifelse(grepl("yes",data_clean$`deference`, ignore.case = T),"1","0")
  ### district courts to circuits ------
    ### make lists
first_circuit <- c("Maine|Massachusetts|New Hampshire|Puerto Rico|Rhode Island")
second_circuit <- c("Connecticut|New York|Vermont")
third_circuit <- c("Delaware|New Jersey|Pennsylvania")
fourth_circuit <- c("Maryland|North Carolina|South Carolina|Virginia|West Virginia|Virgin Islands")
fifth_circuit <- c("Louisiana|Mississippi|Texas")
sixth_circuit <- c("Kentucky|Michigan|Ohio|Tennessee")
seventh_circuit <- c("Illinois|Indiana|Wisconsin")
eighth_circuit <- c("Arkansas|Iowa|Minnesota|Missouri|Nebraska|North Dakota|South Dakota")
ninth_circuit <- c("Alaska|Arizona|California|Hawaii|Idaho|Montana|Nevada|Oregon|Washington|Guam|Northern Mariana Islands")
tenth_circuit <- c("Colorado|Kansas|Mexico|Oklahoma|Utah|Wyoming")
eleventh_circuit <- c("Alabama|Florida|Georgia")
dc_circuit <- c("District of Columbia")

data_clean <- data_clean %>%
  mutate(
    circuit = case_when(
      str_detect(court,first_circuit) ~ "1"
      ,str_detect(court,second_circuit) ~ "2"
      ,str_detect(court,third_circuit) ~ "3"
      ,str_detect(court,fourth_circuit) ~ "4"
      ,str_detect(court,fifth_circuit) ~ "5"
      ,str_detect(court,sixth_circuit) ~ "6"
      ,str_detect(court,seventh_circuit) ~ "7"
      ,str_detect(court,eighth_circuit) ~ "8"
      ,str_detect(court,ninth_circuit) ~ "9"
      ,str_detect(court,tenth_circuit) ~ "10"
      ,str_detect(court,eleventh_circuit) ~ "11"
      ,str_detect(court,dc_circuit) ~ "12"
    )
  )

  ### ngo_env and ngo_big10 with 0 and 1 ---
data_clean$ngo_env_pro <- ifelse(grepl("pro|pro//!",data_clean$`ngo_env`, ignore.case = T),"1","0")
data_clean$ngo_env_neutral <- ifelse(grepl("neutral",data_clean$`ngo_env`, ignore.case = T),"1","0")
data_clean$ngo_env_anti <- ifelse(grepl("anti",data_clean$`ngo_env`, ignore.case = T),"1","0")

data_clean <- data_clean %>%
  mutate(
    ngo_big10 = str_replace_all(ngo_big10,"none","0")
    )
### delete non-environmental cases from data --------

data_clean_no_non <- subset(data_clean,`Type of Nature`!="non-environmental" & `Type of Nature`!="none")

### reorder df -----
data_clean_no_non <- data_clean_no_non %>%
  select(assigned,group,ID,case_name,url,cite,summary,jurisdiction,court,case_date,year,Plaintiffs,`Plaintiff Types`,Defendants,`Defendant Types`,Aim,`Type of Nature`,`Object of Contention`,Outcome,`Outcome Notes`,Procedural,`Federal Agencies`,`Location (state) of conflict`,Species,`Federal Statutes`,Science,Experts,Flag,`Further notes`,cites,EJ_num,EJ_keywords,dr_phd,deference,row_num,rand_num,pt_fed,pt_state,pt_local,pt_military,pt_civic_assn,pt_ngo,pt_union,pt_individual,pt_industry,pt_trade_assn,pt_tribe,pt_public_org,pt_religious_org,pt_other,pt_count,dt_fed,dt_state,dt_local,dt_military,dt_civic_assn,dt_ngo,dt_union,dt_individual,dt_industry,dt_trade_assn,dt_tribe,dt_public_org,dt_religious_org,dt_other,dt_count,fa_aphis,fa_arc,fa_aec,fa_acoe,fa_bif,fa_ibla,fa_blm,fa_boem,fa_bor,fa_bsee,`fa_eda`,`fa_epa`,`fa_faa`,`fa_ferc`,`fa_fema`,`fa_fda`,`fa_fha`,`fa_fhfa`,`fa_fra`,`fa_fsa`,`fa_fsb`,`fa_ftc`,`fa_fta`,`fa_fws`,`fa_fs`,`fa_fhlmc`,`fa_gsa`,`fa_usgs`,`fa_gao`,`fa_mms`,`fa_nasa`,`fa_nmfs`,`fa_nnsa`,`fa_noaa`,`fa_nps`,`fa_nrc`,`fa_omb`,`fa_osmre`,`fa_phmsa`,`fa_stb`,`fa_nrcs`,`fa_usdard`,`fa_other`,`fa_military`,`statutes_solidwaste`,`statutes_fwpca`,`statutes_cwa`,`statutes_caa`,`statutes_cercla`,`statutes_nepa`,`statutes_esa`,`statutes_ffdca`,`statutes_rcra`,`statutes_nfma`,`statutes_fifra`,`statutes_aea`,`statutes_reclamationact`,`statutes_riversandharbors`,`statutes_tsca`,`statutes_swda`,`statutes_flpma`,`statutes_opa`,`statutes_smcra`,`statutes_wildernessact`,`statutes_mbta`,`statutes_mmpa`,`statutes_czma`,`statutes_energypolicyact`,`statutes_msa`,`statutes_epcra`,`statutes_mprsa`,`statutes_ocean`,`statutes_multipleuse`,`statutes_nca`,`statutes_fwca`,`statutes_nwpa`,`statutes_nationalpsoa`,`statutes_eisa`,`statutes_fishandwildlifeact`,statutes_ppa,statutes_antiquitiesact,statutes_fpa,statutes_lacey,statutes_mla,statutes_wildandscenic,statutes_organic,statutes_none,outcome_defendant,outcome_plaintiff,outcome_mixed,ngo_env,ngo_env_pro,ngo_env_neutral,ngo_env_anti,ngo_big10,deference_yes,circuit)


### export data --------

#write.csv(data_clean_no_non,"/Users/caseyr/Documents/School/OSU/Rea Environmental Law/Capstone/data_2_8.csv", row.names = FALSE)
write.csv(data_clean_no_non, "cases_coded_casey_script_cleaned.csv")

# next, pass to to Add_Metacategories.R


# the end