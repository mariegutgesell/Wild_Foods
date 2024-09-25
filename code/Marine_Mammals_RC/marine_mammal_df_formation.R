##Marine Mammal Data in Chugach -- temporal patterns of harvest

##load packages
library(tidyverse)
library(readxl)
library(data.table)

###COMPREHENSIVE SURVEY DATA -----------------
##read in harvest datafiles
setwd("~/Desktop/Wild Foods Repo/data/harvest_data/Chugach/")
file.list <- list.files(pattern='*.xls')
df.list <- sapply(file.list, read_excel, simplify = FALSE)
df <- rbindlist(df.list) %>%
  unite(Site_Year_Code, c(Community_Name, Study_Year), sep = "_", remove = FALSE) 

##List of communities Raven wants data for
site_list <- c("Chenega", "Cordova", "Nanwalek", "Port Graham", "Seward", "Tatitlek", "Valdez")

df <-  df %>%
  dplyr::select(Site_Year_Code, Community_Name, Study_Year, Project_Name, Resource_Code, Resource_Name, Percent_Using:Estimated_Amount_Harvested, Percent_Of_Total_Harvest, Mean_Grams_Per_Capita_Use:Mean_Grams_Percapita_Harvest) %>%
  filter(Community_Name %in% site_list)


#3) MARINE MAMMALS 
marine_mammal_code <- "3"

mm <- df %>% 
  filter(str_detect(Resource_Code, '^3')) 

mm$Resource_Code  <- format(mm$Resource_Code, scientific = FALSE)
mm$Resource_Code <- as.character(mm$Resource_Code) 
str(mm)

mm <- mm %>%
  mutate(Habitat = "Marine") %>%
  mutate(Sex = case_when(
    grepl("Unknown Sex", Resource_Name) ~ "Unknown",
    grepl("Male", Resource_Name) ~ "Male",
    grepl("Female", Resource_Name) ~ "Female",
  )) %>%
  mutate(Taxa_lvl1 = "Marine Mammals") %>%
  mutate(Taxa_lvl2 = case_when( ##don't have a category level 2, so leaving it as marine mammals.. 
    startsWith(Resource_Code, "30") ~ "Marine Mammals",
  )) %>%
  mutate(Taxa_lvl3 = case_when(
    startsWith(Resource_Code, "3006") ~ "Porpoise", 
    startsWith(Resource_Code, "3008") ~ "Seal", 
    startsWith(Resource_Code, "3010") ~ "Sea Otter",
    startsWith(Resource_Code, "3012") ~ "Stellar Sea Lion",
    startsWith(Resource_Code, "3016") ~ "Whale",
    startsWith(Resource_Code, "3099") ~ "Unknown Marine Mammals",
    startsWith(Resource_Code, "3014") ~ "Walrus",
  )) %>%
  mutate(Taxa_lvl4 = case_when( ##keeping this as "genus" level, just to match to other categories 
    startsWith(Resource_Code, "3006") ~ "Porpoise", 
    startsWith(Resource_Code, "3008") ~ "Seal", 
    startsWith(Resource_Code, "3010") ~ "Sea Otter",
    startsWith(Resource_Code, "3012") ~ "Stellar Sea Lion",
    startsWith(Resource_Code, "3016") ~ "Whale",
    startsWith(Resource_Code, "3099") ~ "Unknown Marine Mammals",
    startsWith(Resource_Code, "3014") ~ "Walrus",
  )) %>%
  mutate(Taxa_lvl5 = case_when(
    startsWith(Resource_Code, "300602") ~ "Dall Porpoise", 
    startsWith(Resource_Code, "300802") ~ "Bearded Seal", 
    startsWith(Resource_Code, "30080600") ~ "Harbor Seal",
    startsWith(Resource_Code, "30080604") ~ "Harbour Seal (saltwater)", ##is this a different habitat? can it be captured that way? as not really different species
    startsWith(Resource_Code, "300899") ~ "Unknown Seal",
    #    startsWith(Resource_Code, "300888") ~ "Unknown Seal Oil", ##not in chugach df
    startsWith(Resource_Code, "301602") ~ "Belukha",
    startsWith(Resource_Code, "301606") ~ "Bowhead",
    startsWith(Resource_Code, "301699") ~ "Unknown Whale",
    startsWith(Resource_Code, "3014") ~ "Walrus",
    startsWith(Resource_Code, "3010") ~ "Sea Otter",
    startsWith(Resource_Code, "3012") ~ "Stellar Sea Lion",
    startsWith(Resource_Code, "3099") ~ "Unknown Marine Mammals",
  ))

#mm$Conversion_Units_To_Pounds <- as.character(mm$Conversion_Units_To_Pounds)
#mm$Est_Comm_Population <- as.character(mm$Est_Comm_Population)

##select rows where the family level is not broken down further in a certain year/community
mm1 <- mm %>%
  filter(is.na(Sex)) %>%
  filter(!is.na(Taxa_lvl3)) %>%
  group_by(Site_Year_Code, Taxa_lvl3) %>%
  filter(!Taxa_lvl3 %in% Taxa_lvl3[!is.na(Taxa_lvl4)]) ##filters out familys where genus is not NA -- so i think these are only families where further genus level does not exist (i.e., family is the lowest level ID for that year/community)

##select rows where the genus level is not broken down further for a certain year/community
mm2 <- mm %>%
  filter(is.na(Sex)) %>%
  filter(!is.na(Taxa_lvl4)) %>%
  group_by(Site_Year_Code, Taxa_lvl3, Taxa_lvl4) %>%
  filter(!Taxa_lvl4 %in% Taxa_lvl4[!is.na(Taxa_lvl5)])

##select rows where species level is not broken down further for a certain year/community 
mm3 <- mm %>%
  filter(is.na(Sex)) %>%
  filter(!is.na(Taxa_lvl5))

mm_final <- rbind(mm1, mm2, mm3) %>%
  dplyr::select(Project_Name, Community_Name, Study_Year, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Resource_Code, Resource_Name, Percent_Using:Mean_Grams_Percapita_Harvest) %>%
  mutate(Taxa_lvl4 = coalesce(Taxa_lvl4, Taxa_lvl3)) %>%
  mutate(Taxa_lvl5 = coalesce(Taxa_lvl5, Taxa_lvl4)) %>%
  filter(Taxa_lvl5 != "Harbour Seal (saltwater)") %>% ##removed harbor seal (saltwater) and fur seal (other) as always a replicate when it does come up
  filter(Taxa_lvl5 != "Fur Seal (other)")

##clean up environment
rm(mm, mm1, mm2, mm3)

str(mm_final)
##Plot time series of % harvesting, % using, harvest (total take)


##Organize and clean and fix all comprehensive survey data
mm_comp <- mm_final %>%
  filter(!grepl("Marine Mammals", Project_Name)) %>%
  select(Project_Name, Community_Name, Study_Year, Taxa_lvl1:Taxa_lvl5, Percapita_Pounds_Harvested, Percent_Using:Percent_Giving, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Conversion_Units_To_Pounds, Resource_Harvest_Units)

##the 2003 seals estimated amount are in lbs not #, so need to divide by estimated size of other seals ~56 to get number taken
##fix 2003 harbor seal estimated # taken
h_seal <- mm_comp %>%
  filter(Taxa_lvl5 == "Harbor Seal")

##conversion unit to pounds in 2000 and 2014 is 56.0 lbs/individual -- so will use that to convert the lbs in 2003 to # taken
h_seal_2003 <- h_seal %>%
  filter(Study_Year == 2003) %>%
  mutate(Estimated_Amount_Harvested = Estimated_Amount_Harvested/56.0) %>%
  mutate(Number_Of_Resource_Harvested = Number_Of_Resource_Harvested/56.0) %>%
  mutate(Conversion_Units_To_Pounds = 56.0) %>%
  mutate(Resource_Harvest_Units = "Ind.")

mm_comp1 <- mm_comp %>%
  filter(!(Study_Year == 2003 & Taxa_lvl5 == "Harbor Seal"))

##Final comprehensive marine mammal data 
mm_comp_final <- rbind(h_seal_2003, mm_comp1) %>%
#  filter(!if_all(Percapita_Pounds_Harvested:Estimated_Amount_Harvested, ~ .x == 0)) %>%
  mutate(Project_Type = "Comprehensive Survey") %>%
  ungroup() %>%
  select(Project_Type, Community_Name, Study_Year, Taxa_lvl4, Taxa_lvl5, Percapita_Pounds_Harvested:Estimated_Amount_Harvested)

#rm(mm_comp, h_seal, h_seal_2003, mm_comp1)

####MARINE MAMMAL SURVEY HARVEST DATA ---------------
setwd("~/Desktop/Wild Foods Repo/data/harvest_data/Marine_Mammal_Surveys/")
mm_chenega_hs <- read.csv("chenega_harbor_seal_harvest.csv") %>%
  mutate(Community_Name = "Chenega", Taxa_lvl4 = "Seal", Taxa_lvl5 = "Harbor Seal", Project_Type = "Marine Mammal Surveys") %>%
  rename(Study_Year = "Year", Percapita_Number_Harvested = "Harbor.seal.harvest.per.capita", Percent_Harvesting = "Native.households.harvesting.harbor.seal", Percent_Using = "Native.households.using.harbor.seal.Harvest", Number_of_Resource_Harvested = "Struck.and.lost", Estimated_Amount_Harvested = "Total.take") %>%
  select(Project_Type, Community_Name, Study_Year, Taxa_lvl4, Taxa_lvl5, Percapita_Number_Harvested:Estimated_Amount_Harvested)

mm_chenega_sl <- read.csv("chenega_sea_lion_harvest.csv") %>%
  mutate(Community_Name = "Chenega", Taxa_lvl4 = "Stellar Sea Lion", Taxa_lvl5 = "Stellar Sea Lion", Project_Type = "Marine Mammal Surveys") %>%
  rename(Study_Year = "Year", Percapita_Number_Harvested = "Sea.lion.harvest.per.capita", Percent_Harvesting = "Native.households.harvesting.sea.lion", Percent_Using = "Native.households.using.sea.lion.Harvest", Number_of_Resource_Harvested = "Struck.and.lost", Estimated_Amount_Harvested = "Total.take") %>%
  select(Project_Type, Community_Name, Study_Year, Taxa_lvl4, Taxa_lvl5, Percapita_Number_Harvested:Estimated_Amount_Harvested)
##note: this has one row wrong for harvest, where in 2003 the 2.4 was struck and lost, not harvested, but because focusing on total take did not worry about

mm_cordova_hs <- read.csv("cordova_harbor_seal_harvest.csv") %>%
  mutate(Community_Name = "Cordova", Taxa_lvl4 = "Seal", Taxa_lvl5 = "Harbor Seal", Project_Type = "Marine Mammal Surveys") %>%
  rename(Study_Year = "Year", Percapita_Number_Harvested = "Harbor.seal.harvest.per.capita", Percent_Harvesting = "Native.households.harvesting.harbor.seal", Percent_Using = "Native.households.using.harbor.seal.Harvest", Number_of_Resource_Harvested = "Struck.and.lost", Estimated_Amount_Harvested = "Total.take") %>%
  select(Project_Type, Community_Name, Study_Year, Taxa_lvl4, Taxa_lvl5, Percapita_Number_Harvested:Estimated_Amount_Harvested)

mm_cordova_sl <- read.csv("cordova_sea_lion_harvest.csv") %>%
  mutate(Community_Name = "Cordova", Taxa_lvl4 = "Stellar Sea Lion", Taxa_lvl5 = "Stellar Sea Lion", Project_Type = "Marine Mammal Surveys") %>%
  rename(Study_Year = "Year", Percapita_Number_Harvested = "Sea.lion.harvest.per.capita", Percent_Harvesting = "Native.households.harvesting.sea.lion", Percent_Using = "Native.households.using.sea.lion.Harvest", Number_of_Resource_Harvested = "Struck.and.lost", Estimated_Amount_Harvested = "Total.take") %>%
  select(Project_Type, Community_Name, Study_Year, Taxa_lvl4, Taxa_lvl5, Percapita_Number_Harvested:Estimated_Amount_Harvested)

mm_nanwalek_hs <- read.csv("nanwalek_harbor_seal_harvest.csv") %>%
  mutate(Community_Name = "Nanwalek", Taxa_lvl4 = "Seal", Taxa_lvl5 = "Harbor Seal", Project_Type = "Marine Mammal Surveys") %>%
  rename(Study_Year = "Year", Percapita_Number_Harvested = "Harbor.seal.harvest.per.capita", Percent_Harvesting = "Native.households.harvesting.harbor.seal", Percent_Using = "Native.households.using.harbor.seal.Harvest", Number_of_Resource_Harvested = "Struck.and.lost", Estimated_Amount_Harvested = "Total.take") %>%
  select(Project_Type, Community_Name, Study_Year, Taxa_lvl4, Taxa_lvl5, Percapita_Number_Harvested:Estimated_Amount_Harvested)

mm_nanwalek_sl <- read.csv("nanwalek_sea_lion_harvest.csv") %>%
  mutate(Community_Name = "Nanwalek", Taxa_lvl4 = "Stellar Sea Lion", Taxa_lvl5 = "Stellar Sea Lion", Project_Type = "Marine Mammal Surveys") %>%
  rename(Study_Year = "Year", Percapita_Number_Harvested = "Sea.Lion.Harvest.per.capita", Percent_Harvesting = "Native.Households.Harvesting.Sea.Lion", Percent_Using = "Native.Households.Using.Sea.Lion", Number_of_Resource_Harvested = "Harvest", Estimated_Amount_Harvested = "Total.Take") %>%
  select(Project_Type, Community_Name, Study_Year, Taxa_lvl4, Taxa_lvl5, Percapita_Number_Harvested:Number_of_Resource_Harvested, Estimated_Amount_Harvested)
mm_nanwalek_sl$Percent_Harvesting <- as.numeric(gsub("%", "", mm_nanwalek_sl$Percent_Harvesting))
mm_nanwalek_sl$Percent_Using <- as.numeric(gsub("%", "", mm_nanwalek_sl$Percent_Using))


mm_portgraham_hs <- read.csv("port_graham_harbor_seal_harvest.csv") %>%
  mutate(Community_Name = "Port Graham", Taxa_lvl4 = "Seal", Taxa_lvl5 = "Harbor Seal", Project_Type = "Marine Mammal Surveys") %>%
  rename(Study_Year = "Year", Percapita_Number_Harvested = "Harbor.Seal.Harvest.per.capita", Percent_Harvesting = "Native.Households.Harvesting.Harbor.Seal", Percent_Using = "Native.Households.Using.Harbor.Seal", Number_of_Resource_Harvested = "Harvest", Estimated_Amount_Harvested = "Total.Take") %>%
  select(Project_Type, Community_Name, Study_Year, Taxa_lvl4, Taxa_lvl5, Percapita_Number_Harvested:Number_of_Resource_Harvested, Estimated_Amount_Harvested)
mm_portgraham_hs$Percent_Harvesting <- as.numeric(gsub("%", "", mm_portgraham_hs$Percent_Harvesting))
mm_portgraham_hs$Percent_Using <- as.numeric(gsub("%", "", mm_portgraham_hs$Percent_Using))

mm_portgraham_sl <- read.csv("port_graham_sea_lion_harvest.csv") %>%
  mutate(Community_Name = "Port Graham", Taxa_lvl4 = "Stellar Sea Lion", Taxa_lvl5 = "Stellar Sea Lion", Project_Type = "Marine Mammal Surveys") %>%
  rename(Study_Year = "Year", Percapita_Number_Harvested = "Sea.Lion.Harvest.per.capita", Percent_Harvesting = "Native.Households.Harvesting.Sea.Lion", Percent_Using = "Native.Households.Using.Sea.Lion", Number_of_Resource_Harvested = "Harvest", Estimated_Amount_Harvested = "Total.Take") %>%
  select(Project_Type, Community_Name, Study_Year, Taxa_lvl4, Taxa_lvl5, Percapita_Number_Harvested:Number_of_Resource_Harvested, Estimated_Amount_Harvested)
mm_portgraham_sl$Percent_Harvesting <- as.numeric(gsub("%", "", mm_portgraham_sl$Percent_Harvesting))
mm_portgraham_sl$Percent_Using <- as.numeric(gsub("%", "", mm_portgraham_sl$Percent_Using))

mm_seward_hs <- read.csv("seward_harbor_seal_harvest.csv") %>%
  mutate(Community_Name = "Seward", Taxa_lvl4 = "Seal", Taxa_lvl5 = "Harbor Seal", Project_Type = "Marine Mammal Surveys") %>%
  rename(Study_Year = "Year", Percapita_Number_Harvested = "Harbor.Seal.Harvest.per.capita", Percent_Harvesting = "Native.Households.Harvesting.Harbor.Seal", Percent_Using = "Native.Households.Using.Harbor.Seal", Number_of_Resource_Harvested = "Harvest", Estimated_Amount_Harvested = "Total.Take") %>%
  select(Project_Type, Community_Name, Study_Year, Taxa_lvl4, Taxa_lvl5, Percapita_Number_Harvested:Number_of_Resource_Harvested, Estimated_Amount_Harvested)
mm_seward_hs$Percent_Harvesting <- as.numeric(gsub("%", "", mm_seward_hs$Percent_Harvesting))
mm_seward_hs$Percent_Using <- as.numeric(gsub("%", "", mm_seward_hs$Percent_Using))

mm_seward_sl <- read.csv("seward_sea_lion_harvest.csv") %>%
  mutate(Community_Name = "Seward", Taxa_lvl4 = "Stellar Sea Lion", Taxa_lvl5 = "Stellar Sea Lion", Project_Type = "Marine Mammal Surveys") %>%
  rename(Study_Year = "Year", Percapita_Number_Harvested = "Sea.Lion.Harvest.per.capita", Percent_Harvesting = "Native.Households.Harvesting.Sea.Lion", Percent_Using = "Native.Households.Using.Sea.Lion", Number_of_Resource_Harvested = "Harvest", Estimated_Amount_Harvested = "Total.Take") %>%
  select(Project_Type, Community_Name, Study_Year, Taxa_lvl4, Taxa_lvl5, Percapita_Number_Harvested:Number_of_Resource_Harvested, Estimated_Amount_Harvested)
mm_seward_sl$Percent_Harvesting <- as.numeric(gsub("%", "", mm_seward_sl$Percent_Harvesting))
mm_seward_sl$Percent_Using <- as.numeric(gsub("%", "", mm_seward_sl$Percent_Using))

mm_tatitlek_hs <- read.csv("tatitlek_harbor_seal_harvest.csv") %>%
  mutate(Community_Name = "Tatitlek", Taxa_lvl4 = "Seal", Taxa_lvl5 = "Harbor Seal", Project_Type = "Marine Mammal Surveys") %>%
  rename(Study_Year = "Year", Percapita_Number_Harvested = "Harbor.Seal.Harvest.per.capita", Percent_Harvesting = "Native.Households.Harvesting.Harbor.Seal", Percent_Using = "Native.Households.Using.Harbor.Seal", Number_of_Resource_Harvested = "Harvest", Estimated_Amount_Harvested = "Total.Take") %>%
  select(Project_Type, Community_Name, Study_Year, Taxa_lvl4, Taxa_lvl5, Percapita_Number_Harvested:Number_of_Resource_Harvested, Estimated_Amount_Harvested)
mm_tatitlek_hs$Percent_Harvesting <- as.numeric(gsub("%", "", mm_tatitlek_hs$Percent_Harvesting))
mm_tatitlek_hs$Percent_Using <- as.numeric(gsub("%", "", mm_tatitlek_hs$Percent_Using))

mm_tatitlek_sl <- read.csv("tatitlek_sea_lion_harvest.csv") %>%
  mutate(Community_Name = "Tatitlek", Taxa_lvl4 = "Stellar Sea Lion", Taxa_lvl5 = "Stellar Sea Lion", Project_Type = "Marine Mammal Surveys") %>%
  rename(Study_Year = "Year", Percapita_Number_Harvested = "Sea.Lion.Harvest.per.capita", Percent_Harvesting = "Native.Households.Harvesting.Sea.Lion", Percent_Using = "Native.Households.Using.Sea.Lion", Number_of_Resource_Harvested = "Harvest", Estimated_Amount_Harvested = "Total.Take") %>%
  select(Project_Type, Community_Name, Study_Year, Taxa_lvl4, Taxa_lvl5, Percapita_Number_Harvested:Number_of_Resource_Harvested, Estimated_Amount_Harvested)
mm_tatitlek_sl$Percent_Harvesting <- as.numeric(gsub("%", "", mm_tatitlek_sl$Percent_Harvesting))
mm_tatitlek_sl$Percent_Using <- as.numeric(gsub("%", "", mm_tatitlek_sl$Percent_Using))

mm_valdez_hs <- read.csv("valdez_harbor_seal_harvest.csv") %>%
  mutate(Community_Name = "Valdez", Taxa_lvl4 = "Seal", Taxa_lvl5 = "Harbor Seal", Project_Type = "Marine Mammal Surveys") %>%
  rename(Study_Year = "Year", Percapita_Number_Harvested = "Harbor.Seal.Harvest.per.capita", Percent_Harvesting = "Native.Households.Harvesting.Harbor.Seal", Percent_Using = "Native.Households.Using.Harbor.Seal", Number_of_Resource_Harvested = "Harvest", Estimated_Amount_Harvested = "Total.Take") %>%
  select(Project_Type, Community_Name, Study_Year, Taxa_lvl4, Taxa_lvl5, Percapita_Number_Harvested:Number_of_Resource_Harvested, Estimated_Amount_Harvested)
mm_valdez_hs$Percent_Harvesting <- as.numeric(gsub("%", "", mm_valdez_hs$Percent_Harvesting))
mm_valdez_hs$Percent_Using <- as.numeric(gsub("%", "", mm_valdez_hs$Percent_Using))

mm_valdez_sl <- read.csv("valdez_sea_lion_harvest.csv") %>%
  mutate(Community_Name = "Valdez", Taxa_lvl4 = "Stellar Sea Lion", Taxa_lvl5 = "Stellar Sea Lion", Project_Type = "Marine Mammal Surveys") %>%
  rename(Study_Year = "Year", Percapita_Number_Harvested = "Sea.Lion.Harvest.per.capita", Percent_Harvesting = "Native.Households.Harvesting.Sea.Lion", Percent_Using = "Native.Households.Using.Sea.Lion", Number_of_Resource_Harvested = "Harvest", Estimated_Amount_Harvested = "Total.Take") %>%
  select(Project_Type, Community_Name, Study_Year, Taxa_lvl4, Taxa_lvl5, Percapita_Number_Harvested:Number_of_Resource_Harvested, Estimated_Amount_Harvested)
mm_valdez_sl$Percent_Harvesting <- as.numeric(gsub("%", "", mm_valdez_sl$Percent_Harvesting))
mm_valdez_sl$Percent_Using <- as.numeric(gsub("%", "", mm_valdez_sl$Percent_Using))

mm_survey_all <- rbind(mm_chenega_hs, mm_chenega_sl, mm_cordova_hs, mm_cordova_sl, mm_nanwalek_hs, mm_nanwalek_sl, mm_portgraham_hs, mm_portgraham_sl, mm_seward_hs, mm_seward_sl, mm_tatitlek_hs, mm_tatitlek_sl, mm_valdez_hs, mm_valdez_sl) %>%
  rename(Number_Of_Resource_Harvested = "Number_of_Resource_Harvested") %>%
  mutate(Percent_Using = Percent_Using/100,
         Percent_Harvesting = Percent_Harvesting/100)



####JOIN COMPREHENSIVE AND MARINE MAMMAL SURVEY TOGETHER ---------
rm(list = ls()[!ls() %in% c("mm_comp_final", "mm_survey_all")])

mm_all <- full_join(mm_comp_final, mm_survey_all, by = c("Project_Type", "Community_Name", "Study_Year", "Taxa_lvl4", "Taxa_lvl5", "Percent_Using", "Percent_Harvesting", "Number_Of_Resource_Harvested", "Estimated_Amount_Harvested")) %>%
  select(Project_Type:Taxa_lvl5, Percent_Using:Percapita_Number_Harvested, Percapita_Pounds_Harvested)


setwd("~/Desktop/Wild Foods Repo/")
write.csv(mm_all, "code/Marine_Mammals_RC/marine_mammal_harvest_data_all.csv")


