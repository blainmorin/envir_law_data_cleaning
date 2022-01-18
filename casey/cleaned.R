### installing packages, create data_clean -----

#packages

library(tidyverse)
library(stringr)

#load data

data <- read_csv("cases_coded_prelim_clean1.csv")

#create new df

df_to_clean <- data

#make blanks none

df_to_clean[is.na(df_to_clean)] <- "none"  

### clean data for data_clean -------

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
  ) %>%
  ### plaintiff type and defendant type to data_clean ---------
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
  ) %>%
  ### agencies to data_clean ---------
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
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of indian affairs", "bif"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of indian affairs", "bif"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of indian affairs", "bif"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of reclamation", "bor"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us bureau of reclamation", "bor"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of reclamation", "bor"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bor", "bor"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of reclaimation", "bor"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of ocean energy management", "boem"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of ocean energy management", "boem"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of ocean energy management", "boem"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. bureau of safety and environmental enforcement", "bsee"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "united states bureau of safety and environmental enforcement", "bsee"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "bureau of safety and environmental enforcement", "bsee"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "environmental protection agency", "epa"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. epa", "epa"),
  ) %>%
  mutate( 
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal food and drug administration", "fda"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "food and drug administration", "fda"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal aviation administration", "faa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal highway adminstration", "fha"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal housing finance agency", "fhfa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "federal transit administration", "fta"),
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
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "u.s. fish and wildlife service", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "usfws", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "usfwl", "fws"),
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "us fws", "fws"),
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
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "general service administration", "gsa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national aeronautics and space administration", "nasa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national marine fisheries service", "nmfs"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national oceanic and atmospheric administration", "noaa"),
  ) %>%
  mutate(
    `Federal Agencies` = str_replace_all(`Federal Agencies`, "national park service", "nps"),
  ) %>%
  ### statutes to data_clean -----------
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
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response, compensation, and liability act", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response, compensation, and liability act of 1980", "cercla"),
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
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "cercla & superfund", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "cercla$cwa", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "superfund act", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "superfund", "cercla"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "superfun", "cercla"),
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
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "food, drug, and cosmetic act", "ffdca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "food, drug and cosmetic act", "ffdca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal, food, drug, and cosmetic act", "ffdca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal food, drug, and cosmetic act,", "ffdca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal food, drug, and cosmetic act", "ffdca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal food, drug, and cosmetic ac", "ffdca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal food, drug, and cosmestic act", "ffdca"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal food, drug and cosmetic act", "ffdca"),
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
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy management act", "flpma"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy and management act", "flpma"),
  `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy and management act of 1976", "flpma"),
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
) %>%
  ### states to data_clean ----------
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
  ) %>%
  ### species to data_clean ---------
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
  ) %>%
  ### non-environmental ton to data_clean -------------
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


  ### agency with 0 and 1 --------
data_clean$fa_aphis <- ifelse(grepl("aphis",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_arc <- ifelse(grepl("arc",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_aec <- ifelse(grepl("aec",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_acoe <- ifelse(grepl("acoe",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_blm <- ifelse(grepl("blm",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_bif <- ifelse(grepl("bif",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_bor <- ifelse(grepl("bor",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_boem <- ifelse(grepl("boem",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_bsee <- ifelse(grepl("bsee",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_epa <- ifelse(grepl("epa",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fda <- ifelse(grepl("fda",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_faa <- ifelse(grepl("faa",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fha <- ifelse(grepl("fha",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fhfa <- ifelse(grepl("fhfa",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fta <- ifelse(grepl("fta",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fs <- ifelse(grepl("fs",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_fws <- ifelse(grepl("fws",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_gsa <- ifelse(grepl("gsa",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_nasa <- ifelse(grepl("nasa",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_nmfs <- ifelse(grepl("nmfs",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_noaa <- ifelse(grepl("noaa",data_clean$`Federal Agencies`, ignore.case = T),"1","0")
data_clean$fa_nps <- ifelse(grepl("nps",data_clean$`Federal Agencies`, ignore.case = T),"1","0")

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


### delete non-environmental cases from data --------

data_clean_no_non <- subset(data_clean,`Type of Nature`!="non-environmental" & `Type of Nature`!="none")

### export data --------

write.csv(data_clean_no_non,"/Users/caseyr/Documents/School/OSU/Rea Environmental Law/Capstone/data_clean_no_non_w_analysis.csv", row.names = FALSE)
