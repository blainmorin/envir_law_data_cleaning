install.packages("tidyverse")
library(tidyverse)

data <- read_csv("cases_coded_prelim_clean1.csv")

data$Aim
data$`Federal Agencies`


data_fed_agency <- data %>%
  select(
    `Federal Agencies`
  ) %>%
  group_by(
    `Federal Agencies`
  ) %>%
  filter(
    row_number()== 1
  ) %>%
  ungroup() %>%
  arrange( 
    desc(`Federal Agencies`)
  )



## 10/16/21

data_fed_agency <- data %>%
  select(
    `Federal Agencies`
  ) %>%
  group_by(
    `Federal Agencies`
  ) %>%
  filter(
    row_number()== 1
  ) %>%
  ungroup() %>%
  arrange( 
    desc(`Federal Agencies`)
  ) %>%
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
  ) %>% #### Oddities ####
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
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "% ", "%"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, " %", "%"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, " &", "%"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "& ", "%"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "&", "%"),
  ) 








