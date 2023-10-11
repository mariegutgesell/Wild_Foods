##Project: Wild Foods - SE Alaska
##Script: Reading in an cleaning harvest data to create workable dataframe
##Creator: Marie Gutgesell
##Date started: October 10, 2023

##load packages
library(tidyverse)
library(readxl)
library(data.table)


##read in harvest datafiles
setwd("~/Desktop/Wild Foods Repo/data/harvest_data/")
file.list <- list.files(pattern='*.xls')
df.list <- sapply(file.list, read_excel, simplify = FALSE)
df <- rbindlist(df.list) %>%
  unite(Site_Year_Code, c(Community_Name, Study_Year), sep = "_", remove = FALSE) 

##read in comprehensive survey demographics
setwd("~/Desktop/Wild Foods Repo/data/")
survey_demographics <- read_excel("CSIS_SurveyData_Demographics.xlsx", sheet = 2) %>%
  unite(Site_Year_Code, c(Community, Year), sep = "_", remove = FALSE)


##filter/cleaning dataframe
##reduce dataframe to only focus on comprehensive surveys
df <- df %>%
  filter(Site_Year_Code %in% survey_demographics$Site_Year_Code) %>% ##selects only years where a comprehensive survey was done
  filter(!grepl("Marine Mammals", Project_Name)) ##this removes data from targeted marine mammal surveys done in same year as comprehensive survey

##generate list of all species in df 
sp_list <- df %>%
  distinct(Resource_Code, Resource_Name, .keep_all = TRUE) 

projects <- df %>%
  distinct(Project_Name)

deer_targeted <- df %>%
  filter(Project_Name == "Prince of Wales Deer 1999")

  
  unique(df["Resource_Code", "Resource_Name"])


##playing around with one community, one year to just get more familiar w/ data
angoon_1984 <- df %>%
  filter(Community_Name == "Angoon") %>%
  filter(Study_Year == "1984" )

##Fish -- all 9-digit resource codes start with 1