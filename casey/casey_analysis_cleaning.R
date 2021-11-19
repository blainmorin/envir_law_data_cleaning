#11/17 playing with data to try and make new columns

#packages

library(tidyverse)
library(stringr)

#load data

data <- read_csv("cases_coded_prelim_clean1.csv")

#create new df

data_pt_dt <- data

#make blanks none
  
data_pt_dt[is.na(data_pt_dt)] <- "none"  

#clean plaintiff type

clean_pt_dt <- data_pt_dt %>%
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

#split into new columns

pt_dt_by_type <- clean_pt_dt

#split pt into columns by type, 1 if type is contained, 0 if not

pt_dt_by_type$pt_fed <- ifelse(grepl("fed",pt_dt_by_type$`Plaintiff Types`, ignore.case = T),"1","0")
pt_dt_by_type$pt_state <- ifelse(grepl("state",pt_dt_by_type$`Plaintiff Types`, ignore.case = T),"1","0")
pt_dt_by_type$pt_local <- ifelse(grepl("local",pt_dt_by_type$`Plaintiff Types`, ignore.case = T),"1","0")
pt_dt_by_type$pt_military <- ifelse(grepl("military",pt_dt_by_type$`Plaintiff Types`, ignore.case = T),"1","0")
pt_dt_by_type$pt_civic_assn <- ifelse(grepl("civic_assn",pt_dt_by_type$`Plaintiff Types`, ignore.case = T),"1","0")
pt_dt_by_type$pt_ngo <- ifelse(grepl("ngo",pt_dt_by_type$`Plaintiff Types`, ignore.case = T),"1","0")
pt_dt_by_type$pt_union <- ifelse(grepl("union",pt_dt_by_type$`Plaintiff Types`, ignore.case = T),"1","0")
pt_dt_by_type$pt_individual <- ifelse(grepl("individual",pt_dt_by_type$`Plaintiff Types`, ignore.case = T),"1","0")
pt_dt_by_type$pt_industry <- ifelse(grepl("industry",pt_dt_by_type$`Plaintiff Types`, ignore.case = T),"1","0")
pt_dt_by_type$pt_trade_assn <- ifelse(grepl("trade_assn",pt_dt_by_type$`Plaintiff Types`, ignore.case = T),"1","0")
pt_dt_by_type$pt_tribe <- ifelse(grepl("tribe",pt_dt_by_type$`Plaintiff Types`, ignore.case = T),"1","0")
pt_dt_by_type$pt_public_org <- ifelse(grepl("public_org",pt_dt_by_type$`Plaintiff Types`, ignore.case = T),"1","0")
pt_dt_by_type$pt_religious_org <- ifelse(grepl("religious_org",pt_dt_by_type$`Plaintiff Types`, ignore.case = T),"1","0")
pt_dt_by_type$pt_other <- ifelse(grepl("other",pt_dt_by_type$`Plaintiff Types`, ignore.case = T),"1","0")

#split dt into columns by type, 1 if type is contained, 0 if not

pt_dt_by_type$dt_fed <- ifelse(grepl("fed",pt_dt_by_type$`Defendant Types`, ignore.case = T),"1","0")
pt_dt_by_type$dt_state <- ifelse(grepl("state",pt_dt_by_type$`Defendant Types`, ignore.case = T),"1","0")
pt_dt_by_type$dt_local <- ifelse(grepl("local",pt_dt_by_type$`Defendant Types`, ignore.case = T),"1","0")
pt_dt_by_type$dt_military <- ifelse(grepl("military",pt_dt_by_type$`Defendant Types`, ignore.case = T),"1","0")
pt_dt_by_type$dt_civic_assn <- ifelse(grepl("civic_assn",pt_dt_by_type$`Defendant Types`, ignore.case = T),"1","0")
pt_dt_by_type$dt_ngo <- ifelse(grepl("ngo",pt_dt_by_type$`Defendant Types`, ignore.case = T),"1","0")
pt_dt_by_type$dt_union <- ifelse(grepl("union",pt_dt_by_type$`Defendant Types`, ignore.case = T),"1","0")
pt_dt_by_type$dt_individual <- ifelse(grepl("individual",pt_dt_by_type$`Defendant Types`, ignore.case = T),"1","0")
pt_dt_by_type$dt_industry <- ifelse(grepl("industry",pt_dt_by_type$`Defendant Types`, ignore.case = T),"1","0")
pt_dt_by_type$dt_trade_assn <- ifelse(grepl("trade_assn",pt_dt_by_type$`Defendant Types`, ignore.case = T),"1","0")
pt_dt_by_type$dt_tribe <- ifelse(grepl("tribe",pt_dt_by_type$`Defendant Types`, ignore.case = T),"1","0")
pt_dt_by_type$dt_public_org <- ifelse(grepl("public_org",pt_dt_by_type$`Defendant Types`, ignore.case = T),"1","0")
pt_dt_by_type$dt_religious_org <- ifelse(grepl("religious_org",pt_dt_by_type$`Defendant Types`, ignore.case = T),"1","0")
pt_dt_by_type$dt_other <- ifelse(grepl("other",pt_dt_by_type$`Defendant Types`, ignore.case = T),"1","0")




