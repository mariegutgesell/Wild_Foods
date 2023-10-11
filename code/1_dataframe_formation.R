##Project: Wild Foods - SE Alaska
##Script: Reading in and cleaning harvest data to create workable dataframe
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
df_test <- df %>%
  filter(Site_Year_Code %in% survey_demographics$Site_Year_Code) %>% ##selects only years where a comprehensive survey was done
  filter(!grepl("Marine Mammals", Project_Name)) #%>% ##this removes data from targeted marine mammal surveys done in same year as comprehensive survey
  #select(Project_Name, Site_Year_Code, Study_Year, Community_Name, Resource_Code, Resource_Name, Percent_Using, Percent_Attempting_to_Harvest, Percent_Harvesting, Percent_Receiving, Percent_Giving, Percent_Of_Total_Harvest, Estimated_Total_Pounds_Harvested, Percapita_Pounds_Harvested) #%>%##reducing to most likely columns of interest
#  filter(!str_detect(Resource_Name, "\\[.*?\\]")) ##Remove breakdown of each species into gear type -- can add this back in later if interested, but for now have removed
  
##or could you change orientation so gear type goes long? or make a separate dataframe that has gear type as column
##or since this is all nested, maybe go to lowest/most detailed level and then create summary ones.. need to think about most streamlined/efficient way of doing this grouping and so don't have duplicate data
  
##reorganize dataframe to make categories, levels, usable/no repeat rows of data

##generate list of all species in df 
sp_list <- df_test %>%
  distinct(Resource_Code, Resource_Name)

projects <- df %>%
  distinct(Project_Name)


##Some preliminary playing around with data to visualize gross patterns
all_resource_plot <- df %>%
  filter(Resource_Name == "All Resources") %>%
  ggplot(aes(x=Study_Year, y = Estimated_Total_Pounds_Harvested, group = Community_Name)) +
  geom_boxplot(aes(group = Community_Name, colour = Community_Name)) +
  theme_classic() 
all_resource_plot
  


##playing around to make sure understand structure of data
salmon_test <- df %>%
  filter(grepl("Salmon", Resource_Name))
  
  
  filter(Resource_Code %in% c("100000000", "200000000", "300000000", "400000000", "500000000", "600000000"))


##playing around with one community, one year to just get more familiar w/ data
angoon_1984 <- df %>%
  filter(Community_Name == "Angoon") %>%
  filter(Study_Year == "1984" ) 

salmon_test <- angoon_1984 %>%
  filter(grepl("Salmon", Resource_Name)) %>%
  select(Site_Year_Code, Resource_Code, Resource_Name, Estimated_Total_Pounds_Harvested)

##Fish -- all 9-digit resource codes start with 1