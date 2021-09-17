# Data Cleaning

We can decide if this system works for everyone, but my thought is to have separate scripts for each data cleaning task. Contribute your work to your folder, then we can call them from the master_clean.R file. 

Instructions for setting up rstudio with github are here: https://happygitwithr.com/rstudio-git-github.html
* Remember to pull before you push!



# Master Task List

We can also add and dispatch tasks here in lieu of a google doc. 

| Task  | Assigned to | Complete? |
| ------------- | ------------- | ------------- |
| Trim white space from strings  | Blain Morin  | Yes|
| Another task  | Another Person  | No|


# Example Structure

## Master File

The master_clean.R file loads the data from google drive and loads the required packages. The goal is this master script will run without error on any machine. For example, note the syntax on the package calls. Before the library call, the code checks that the package is available. If not, the package is installed before R tries to load it.  

```
### Start by calling data from the drive

require(googlesheets4) == T || install.packages("googlesheets4")
library(googlesheets4)

law.df = read_sheet("https://docs.google.com/spreadsheets/d/1Zz50a6TH-Y56Sv7h1JVlmoGnd8llkCv4qDNX9Qs4YI0/edit#gid=0")


### Put other packages here

require(tidyverse) == T || install.packages("tidyverse")
library(tidyverse)

```


When you write your script, you can assume that this master preamble will me run before your work is called. 

---

## Add your work

In the blain folder, I've added an example script called trim_white_space.R:

```
### Trim Extra White Space
### This script removes extra spaces before or after a string
### Stolen from Chris' file

law.df = law.df %>%
  mutate(
    across(
      where(
        is.character
      ),
      str_trim
    )
  )


```

Try to name the file so that it is clear what it does. Add comments to the top describing it as well. 

## Call your work from master_clean.R

Here is the template for calling your code in the master_clean.R file:

```

### Trim white spaces (extra spaces before or after character strings)
source("~/envir_law_data_cleaning/blain/trim_white_space.R")

```

No such thing as too many comments! Add a short description above the source call. 

If we are pulling and pushing as an Rproj, we should not run into working directory problems. 
