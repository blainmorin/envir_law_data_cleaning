### Blain example

### Trim Extra White Space
### This script removes extra spaces before or after a string
### Stolen from Chris' file

# library(googlesheets4)
# library(tidyverse)
# law.df = read_sheet("https://docs.google.com/spreadsheets/d/1Zz50a6TH-Y56Sv7h1JVlmoGnd8llkCv4qDNX9Qs4YI0/edit#gid=0")

law.df = law.df %>%
  mutate(
    across(
      where(
        is.character
      ),
      str_trim
    )
  )
