##Testing out harvest calculations 

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
df_comp <- df %>%
  filter(Site_Year_Code %in% survey_demographics$Site_Year_Code) %>% ##selects only years where a comprehensive survey was done
  filter(!grepl("Marine Mammals", Project_Name)) %>% ##this removes data from targeted marine mammal surveys done in same year as comprehensive survey
  left_join(survey_demographics, by = "Site_Year_Code")


##testing calculations using 1 community/year
angoon_2012 <- df_comp %>%
  filter(Site_Year_Code == "Angoon_2012")

#looking at conversion units
test_1 <- angoon_2012 %>%
  mutate(calc_rep_lb = Number_Of_Resource_Harvested * Conversion_Units_To_Pounds) %>%
  select(Resource_Code, Resource_Name, calc_rep_lb, Reported_Pounds_Harvested)

##so yes, the reported lbs harvested is calculated by converting the number of resource harvested by the conversion unit

##I am not able to determine, but I think the number of resource harvested is the sum across all households surveyed 
##surveys are done at the household level

##calculating mean lbs per household = reported pounds harvested/# of households surveyed
test_2 <- angoon_2012 %>%
  mutate(calc_mean_per_household = Reported_Pounds_Harvested/Sampled_households) %>%
  select(Resource_Code, Resource_Name, calc_mean_per_household, Mean_Pounds_Per_Household, Sampled_households) %>%
  left_join(test_1, by = c("Resource_Code", "Resource_Name"))
##close, but some differences... damn I really want to go and calculate this all myself.. 
##why are these different?
##maybe they calculated the estimated total harvest first and then calculated mean... 

test_3 <- angoon_2012 %>%
  mutate(calc_mean_per_household = Estimated_Total_Pounds_Harvested/Est_Num_Community_Households) %>%
  select(Resource_Code, Resource_Name, calc_mean_per_household, Mean_Pounds_Per_Household)

test_4 <- angoon_2012 %>%
  mutate(calc_est_total_harv = Reported_Pounds_Harvested * (Est_Num_Community_Households/Sampled_households)) %>%
  select(Resource_Code, Resource_Name, calc_est_total_harv, Estimated_Total_Pounds_Harvested)

test_5 <- angoon_2012 %>%
  mutate(calc_est_total_harv = (Number_Of_Resource_Harvested * (Est_Num_Community_Households/Sampled_households)) * Conversion_Units_To_Pounds) %>%
  select(Resource_Code, Resource_Name, calc_est_total_harv, Estimated_Total_Pounds_Harvested)
