
#install packages

install.packages("tidyverse")
library(tidyverse)
library(stringr)

#load data

data <- read_csv("cases_coded_prelim_clean1.csv")

#create plaintiff type dataframe

data_plaintiff_type <- data %>%

#make blanks none

data_plaintiff_type[is.na(data_plaintiff_type)] <- "none"
  
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
  
  
# plaintiff_type_df should be fully cleaned now! 
  
  