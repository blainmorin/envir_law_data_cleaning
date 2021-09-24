### This is the master script that will
### call individual scripts.


### Start by calling data from the drive
# require(googlesheets4) == T || install.packages("googlesheets4")
# library(googlesheets4)
# law.df = read_sheet("https://docs.google.com/spreadsheets/d/1Zz50a6TH-Y56Sv7h1JVlmoGnd8llkCv4qDNX9Qs4YI0/edit#gid=0")


require(readr) == T || install.packages("readr")
library(readr)
law.df = read_csv("cases_coded_prelim_clean1.csv")


### Put other packages here
require(tidyverse) == T || install.packages("tidyverse")
library(tidyverse)

### Trim white spaces (extra spaces before or after character strings)
source("~/envir_law_data_cleaning/blain/trim_white_space.R")

