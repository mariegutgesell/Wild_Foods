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

##start with fish
fish_code <- "1"

fish <- df_test %>% 
  filter(str_detect(Resource_Code, '^1')) 
 
fish$Resource_Code  <- format(fish$Resource_Code, scientific = FALSE)
 
fish$Resource_Code <- as.character(fish$Resource_Code) 
str(fish)

fish <- fish %>%
  mutate(Fishing_Gear_Type = case_when(
    endsWith(Resource_Code, "0") ~ "NA",
    endsWith(Resource_Code, "1") ~ "CF_Retention",
    endsWith(Resource_Code, "2") ~ "Rod_Reel",
    endsWith(Resource_Code, "3") ~ "Other_Gear",
  )) %>%
  mutate(General_Category = case_when(
    startsWith(Resource_Code, "1") ~ "Fish",
  )) %>%
  mutate(General_Category_lvl2 = case_when(
    startsWith(Resource_Code, "11") ~ "Salmon",
    startsWith(Resource_Code, "12") ~ "Non-Salmon Fish",
  )) %>%
  mutate(Family = case_when(
    startsWith(Resource_Code, "11") ~ "Oncorhynchus",
    startsWith(Resource_Code, "1202") ~ "Clupeidae",
    startsWith(Resource_Code, "1203") ~ "Clupeidae",
    startsWith(Resource_Code, "1204") ~ "Osmeridae",
    startsWith(Resource_Code, "1206") ~ "Bass", ##only groups are sea bass and unknown bass, but they could belong to different families? 
    startsWith(Resource_Code, "1208") ~ "Blenny", 
    startsWith(Resource_Code, "1210") ~ "Gadidae", ##what species sum up the cods?
    startsWith(Resource_Code, "1212") ~ "Eel",
    startsWith(Resource_Code, "1214") ~ "Flounder",
    startsWith(Resource_Code, "1216") ~ "Hexagrammidae",
    startsWith(Resource_Code, "1218") ~ "Halibut",
    startsWith(Resource_Code, "1220") ~ "Lamprey",
    startsWith(Resource_Code, "1222") ~ "Perch", 
    startsWith(Resource_Code, "1226") ~ "Rockfish",
    startsWith(Resource_Code, "1228") ~ "Sablefish",
    startsWith(Resource_Code, "1230") ~ "Sculpin",
    startsWith(Resource_Code, "1232") ~ "Shark",
    startsWith(Resource_Code, "1234") ~ "Skates",
    startsWith(Resource_Code, "1236") ~ "Sole",
    startsWith(Resource_Code, "1240") ~ "Tuna_Mackerel",
    startsWith(Resource_Code, "1250") ~ "Char",
    startsWith(Resource_Code, "1252") ~ "Grayling",
    startsWith(Resource_Code, "1254") ~ "Pike",
    startsWith(Resource_Code, "1258") ~ "Sturgeon",
    startsWith(Resource_Code, "1262") ~ "Trout",
    startsWith(Resource_Code, "1264") ~ "Whitefish",
    startsWith(Resource_Code, "129") ~ "Unknown Non-Salmon Fish",
    startsWith(Resource_Code, "1248") ~ "Burbot",
    startsWith(Resource_Code, "1256") ~ "Sheefish",
  )) %>%
  mutate(Species = case_when(
    startsWith(Resource_Code, "111") ~ "Chum Salmon",
    startsWith(Resource_Code, "112") ~ "Coho Salmon",
    startsWith(Resource_Code, "113") ~ "Chinook Salmon",
    startsWith(Resource_Code, "114") ~ "Pink Salmon",
    startsWith(Resource_Code, "115") ~ "Sockeye Salmon",
    startsWith(Resource_Code, "116") ~ "Landlocked Salmon", ##what does this mean?
    startsWith(Resource_Code, "118") ~ "Salmon Roe",
    startsWith(Resource_Code, "119") ~ "Unknown Salmon",
    startsWith(Resource_Code, "1202") ~ "Herring",
    startsWith(Resource_Code, "1203") ~ "Herring Roe", ##note: this is broken down into different types and locations of herring spawn, herring sac roe, etc. do we need to go this specified? I don't think so
    startsWith(Resource_Code, "120402") ~ "Capelin (grunion)",
    startsWith(Resource_Code, "120404") ~ "Eulachon (hooligan, candlefish",
    startsWith(Resource_Code, "120408") ~ "Surf Smelt",
    startsWith(Resource_Code, "12041") ~ "Silver Smelt",
    startsWith(Resource_Code, "12049") ~ "Unknown Smelt",
    startsWith(Resource_Code, "120602") ~ "Sea Bass", ##what is difference between sea bass and black rockfish?
    startsWith(Resource_Code, "120609") ~ "Unknown Bass",
    startsWith(Resource_Code, "1208") ~ "Blenny",
    startsWith(Resource_Code, "121004") ~ "Pacific Cod (gray)",
    startsWith(Resource_Code, "121008") ~ "Pacific Tom Cod",
    startsWith(Resource_Code, "121012") ~ "Walleye Pollock (whiting)",
    startsWith(Resource_Code, "121006") ~ "Pacific (silver) hake",
    startsWith(Resource_Code, "12109") ~ "Unknown Cod",
    startsWith(Resource_Code, "1212") ~ "Eel",
    startsWith(Resource_Code, "12140") ~ "Flounder",
    startsWith(Resource_Code, "12149") ~ "Unknown Flounder",
    startsWith(Resource_Code, "121606") ~ "Lingcod",
    startsWith(Resource_Code, "121608") ~ "Rock Greenling",
    startsWith(Resource_Code, "121604") ~ "Kelp Greenling",
    startsWith(Resource_Code, "12169") ~ "Unknown Greenling",
    startsWith(Resource_Code, "1218") ~ "Halibut",
    startsWith(Resource_Code, "1220") ~ "Lamprey",
    startsWith(Resource_Code, "122202") ~ "Sea Perch",
    startsWith(Resource_Code, "12229") ~ "Unknown Perch", 
    startsWith(Resource_Code, "122602") ~ "Black Rockfish",
    startsWith(Resource_Code, "122604") ~ "Red Rockfish",
    startsWith(Resource_Code, "122606") ~ "Yellow Eye Rockfish",
    startsWith(Resource_Code, "122612") ~ "Quillback Rockfish",
    startsWith(Resource_Code, "122614") ~ "Brown Rockfish", 
    startsWith(Resource_Code, "122634") ~ "China Rockfish",
    startsWith(Resource_Code, "122616") ~ "Dusky Rockfish",
    startsWith(Resource_Code, "122618") ~ "Copper Rockfish",
    startsWith(Resource_Code, "12269") ~ "Unknown Rockfish",
    startsWith(Resource_Code, "1228") ~ "Sablefish (black cod)", 
    startsWith(Resource_Code, "123002") ~ "Buffalo Sculpin", 
    startsWith(Resource_Code, "123004") ~ "Bullhead Sculpin",
    startsWith(Resource_Code, "123006") ~ "Irish Lord",
    startsWith(Resource_Code, "12300602") ~ "Red Irish Lord",
    startsWith(Resource_Code, "12309") ~ "Unknown Sculpin",
    startsWith(Resource_Code, "123202") ~ "Dogfish",
    startsWith(Resource_Code, "123204") ~ "Salmon Shark",
    startsWith(Resource_Code, "12329") ~ "Unknown Shark",
    startsWith(Resource_Code, "1234") ~ "Skates",
    startsWith(Resource_Code, "12360") ~ "Sole",
    startsWith(Resource_Code, "12369") ~ "Unknown Sole",
    startsWith(Resource_Code, "124002") ~ "Blue Fin", 
    startsWith(Resource_Code, "124004") ~ "Mackerel", 
    startsWith(Resource_Code, "12409") ~ "Unknown Tuna/Mackerel",
    startsWith(Resource_Code, "125002") ~ "Arctic Char",
    startsWith(Resource_Code, "125004") ~ "Brook Trout", 
    startsWith(Resource_Code, "125006") ~ "Dolly Varden",
    startsWith(Resource_Code, "1250069") ~ "Dolly Varden - unknown", ##what does this mean?
    startsWith(Resource_Code, "1252") ~ "Grayling",
    startsWith(Resource_Code, "1254") ~ "Pike",
    startsWith(Resource_Code, "125802") ~ "Green Sturgeon", 
    startsWith(Resource_Code, "125804") ~ "White Sturgeon", 
    startsWith(Resource_Code, "12589") ~ "Unknown Sturgeon",
    startsWith(Resource_Code, "126202") ~ "Cutthroat Trout",
    startsWith(Resource_Code, "126204") ~ "Rainbow Trout", 
    startsWith(Resource_Code, "126206") ~ "Steelhead", 
    startsWith(Resource_Code, "12629") ~ "Unknown Trout", 
    startsWith(Resource_Code, "12640") ~ "Whitefish", 
    startsWith(Resource_Code, "12649") ~ "Unknown Whitefish",
    startsWith(Resource_Code, "1299") ~ "Unknown Non-Salmon Fish", 
    startsWith(Resource_Code, "1248") ~ "Burbot",
    startsWith(Resource_Code, "1256") ~ "Sheefish",
  ))
 
##need to see if i should put in species name for family level sum, have only put now if family not further broken down

##want to write a loop function that will go through and check if the categories sum to each other.. each site/year

fish_sp_list <- fish %>%
  distinct(Resource_Code, Resource_Name)


  
fish_test <- fish %>%
  group_by(Site_Year_Code) %>%
  count()
  
##have 65 separate food webs to construct :D 


##also want to check that all the values for the different categories add up 


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