##Generating cleaned dataframe for food web analysis
##Wild Foods Project

##Creator: Marie Gutgesell
##Data started: November 21, 2023

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

##reduce dataframe to only focus on comprehensive surveys
df_comp <- df %>%
  filter(Site_Year_Code %in% survey_demographics$Site_Year_Code) %>% ##selects only years where a comprehensive survey was done
  filter(!grepl("Marine Mammals", Project_Name)) %>% ##this removes data from targeted marine mammal surveys done in same year as comprehensive survey
  select(Site_Year_Code, Project_Name, Resource_Code, Resource_Name, Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest, Conversion_Units_To_Pounds,Resource_Harvest_Units ) %>%
  left_join(survey_demographics %>% select(Site_Year_Code, Est_Comm_Population), by = "Site_Year_Code")

##reorganize dataframe to make categories, levels

##start with fish
##start with fish
#1) FISH ------------------
fish_code <- "1"
fish <- df_comp %>% 
  filter(str_detect(Resource_Code, '^1')) 
fish$Resource_Code  <- format(fish$Resource_Code, scientific = FALSE)
fish$Resource_Code <- as.character(fish$Resource_Code) 
str(fish)

fish <- fish %>%
  mutate(Fishing_Gear_Type = case_when(
    endsWith(Resource_Code, "0") ~ "NA", ##fishing NA
    endsWith(Resource_Code, "1") ~ "CF_Retention", ##CF_retention
    endsWith(Resource_Code, "2") ~ "Rod_Reel", ##Rod and Real
    endsWith(Resource_Code, "3") ~ "Other_Gear", ##Other Gear
  )) %>%
  mutate(Taxa_lvl1 = case_when(
    startsWith(Resource_Code, "1") ~ "Fish",
  )) %>%
  mutate(Taxa_lvl2 = case_when(
    startsWith(Resource_Code, "11") ~ "Salmon",
    startsWith(Resource_Code, "12") ~ "Non-Salmon Fish",
  )) %>%
  mutate(Taxa_lvl3 = case_when(
    startsWith(Resource_Code, "11") ~ "Oncorhynchus",
    startsWith(Resource_Code, "1202") ~ "Clupeidae",
    startsWith(Resource_Code, "1203") ~ "Clupeidae",
    startsWith(Resource_Code, "1204") ~ "Osmeridae",
    startsWith(Resource_Code, "1206") ~ "Bass", ##only groups are sea bass and unknown bass, but they could belong to different families? 
    startsWith(Resource_Code, "1208") ~ "Blenny", 
    startsWith(Resource_Code, "1210") ~ "Gadiformes", ##what species sum up the cods?
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
  mutate(Taxa_lvl4 = case_when(
    startsWith(Resource_Code, "11") ~ "Oncorhynchus",
    startsWith(Resource_Code, "1202") ~ "Clupeidae",
    startsWith(Resource_Code, "1203") ~ "Clupeidae",
    startsWith(Resource_Code, "1204") ~ "Osmeridae",
    startsWith(Resource_Code, "1206") ~ "Bass", ##only groups are sea bass and unknown bass, but they could belong to different families? 
    startsWith(Resource_Code, "1208") ~ "Blenny", 
    startsWith(Resource_Code, "1210") ~ "Gadiformes", ##what species sum up the cods?
    startsWith(Resource_Code, "1212") ~ "Eel",
    startsWith(Resource_Code, "1214") ~ "Flounder",
    startsWith(Resource_Code, "1216") ~ "Hexagrammidae",
    startsWith(Resource_Code, "1218") ~ "Halibut",
    startsWith(Resource_Code, "1220") ~ "Lamprey",
    startsWith(Resource_Code, "1222") ~ "Perch", 
    startsWith(Resource_Code, "1226") ~ "Rockfish",
    startsWith(Resource_Code, "1228") ~ "Sablefish",
    startsWith(Resource_Code, "123006") ~ "Irish Lord",
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
  mutate(Taxa_lvl5 = case_when(
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
    startsWith(Resource_Code, "120699") ~ "Unknown Bass",
    startsWith(Resource_Code, "1208") ~ "Blenny",
    startsWith(Resource_Code, "121004") ~ "Pacific Cod (gray)",
    startsWith(Resource_Code, "121008") ~ "Pacific Tom Cod",
    startsWith(Resource_Code, "121012") ~ "Walleye Pollock (whiting)",
    startsWith(Resource_Code, "121006") ~ "Pacific (silver) hake",
    startsWith(Resource_Code, "12109") ~ "Unknown Gadiformes",
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
    #startsWith(Resource_Code, "12300600") ~ "Irish Lord",
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
    startsWith(Resource_Code, "1250060") ~ "Dolly Varden",
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
  )) %>%
  mutate(Habitat = case_when(
    startsWith(Taxa_lvl2, "Salmon") ~ "Freshwater_Anadromous",
    startsWith(Taxa_lvl3, "Osmeridae") ~ "Freshwater_Anadromous",
    startsWith(Taxa_lvl3, "Char") ~ "Freshwater_Anadromous",
    startsWith(Taxa_lvl3, "Trout") ~ "Freshwater_Anadromous",
    startsWith(Taxa_lvl3, "Whitefish") ~ "Freshwater_Anadromous",
    startsWith(Taxa_lvl5, "Herring Roe") ~ "Nearshore",
    startsWith(Taxa_lvl2, "Non-Salmon") ~ "Marine",
  )) %>%
  mutate(Roe_Collection_Type = case_when(
    startsWith(Resource_Code, "120302") ~ "Unspecified",
    startsWith(Resource_Code, "120304") ~ "Sac Roe",
    startsWith(Resource_Code, "120306") ~ "Spawn on Kelp",
    startsWith(Resource_Code, "120308") ~ "Roe on Hair Seaweed",
    startsWith(Resource_Code, "120310") ~ "Roe in Hemlock Branches",
  ))
fish_str <- fish %>%
  distinct(Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Resource_Code, Resource_Name, Fishing_Gear_Type, Roe_Collection_Type)
str(fish)
#change these to character so dont get added 
fish$Conversion_Units_To_Pounds <- as.character(fish$Conversion_Units_To_Pounds)
fish$Est_Comm_Population <- as.character(fish$Est_Comm_Population)
str(fish)
##select rows where the family level is not broken down further in a certain year/community
fish1 <- fish %>%
  filter(Fishing_Gear_Type == "NA") %>%
  filter(!is.na(Taxa_lvl3)) %>%
  group_by(Site_Year_Code, Taxa_lvl3) %>%
  filter(!Taxa_lvl3 %in% Taxa_lvl3[!is.na(Taxa_lvl4)]) ##filters out familys where genus is not NA -- so i think these are only families where further genus level does not exist (i.e., family is the lowest level ID for that year/community)

##select rows where the genus level is not broken down further for a certain year/community
fish2 <- fish %>%
  filter(Fishing_Gear_Type == "NA") %>%
  filter(!is.na(Taxa_lvl4)) %>%
  group_by(Site_Year_Code, Taxa_lvl3, Taxa_lvl4) %>%
  filter(!Taxa_lvl4 %in% Taxa_lvl4[!is.na(Taxa_lvl5)])

##select rows where species level is not broken down further for a certain year/community 
fish3 <- fish %>%
  filter(Fishing_Gear_Type == "NA") %>%
  filter(!is.na(Taxa_lvl5))

fish4 <- rbind(fish1, fish2, fish3)


##Resolve the Dolly Varden Issue
##Yakutat 2015 and Klukwan 2014 are the only ones with this dolly varden unknown species, and it is a duplicate of the dolly varden row in these communities/years
##    - For both of these places, the dolly varden-unknown looks like the right data (at least for these 7 variables..), these are the only two sites with this dv unknown, so my feeling is select the dv unknown for these two sites.. 
##select the two year-communities that have the dolly varden unknown, and then are removing the two rows that are just dolly varden name, keep dolly-varden unknown and then change name to dolly varden (makes sure we don't have duplicated data)
fish5 <- fish4 %>%
  filter(Site_Year_Code %in% c("Klukwan_2014", "Yakutat_2015")) %>%
  filter(Taxa_lvl5 != "Dolly Varden") 
fish5$Taxa_lvl5[fish5$Taxa_lvl5 == "Dolly Varden - unknown"] <- "Dolly Varden"  

##select all other sites and then will bind this back together 
fish6 <- fish4 %>%
  filter(Site_Year_Code != "Klukwan_2014") %>%
  filter(Site_Year_Code != "Yakutat_2015")

fish7 <- rbind(fish5, fish6) %>%
  select(Project_Name, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest, Conversion_Units_To_Pounds, Resource_Harvest_Units, Est_Comm_Population) %>%
  mutate(Taxa_lvl4 = coalesce(Taxa_lvl4, Taxa_lvl3)) %>%
  mutate(Taxa_lvl5 = coalesce(Taxa_lvl5, Taxa_lvl4))
  
##sum rows where is same species in community/year (e.g., multiple types of herring roe)
fish_final <- fish7 %>%
  group_by(Project_Name, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Conversion_Units_To_Pounds, Resource_Harvest_Units, Est_Comm_Population) %>%
  summarise(across(where(is.numeric), sum))
  

##Note: I may go back and try and calculate these values myself from the beginning, as I am wary, but am moving forward this way for now. 

#clean up environment
rm(fish1, fish2, fish3, fish4, fish5, fish6, fish7)


#2) LAND MAMMALS ------------------
land_mammal_code <- "2"

lm <- df_comp %>% 
  filter(str_detect(Resource_Code, '^2')) 

lm$Resource_Code  <- format(lm$Resource_Code, scientific = FALSE)
lm$Resource_Code <- as.character(lm$Resource_Code) 
str(lm)

lm <- lm %>%
  mutate(Habitat = "Terrestrial") %>%
  mutate(Sex = case_when(
    endsWith(Resource_Code, "1") ~ "Male", 
    endsWith(Resource_Code, "2") ~ "Female", 
    endsWith(Resource_Code, "9") ~ "Unknown",
  )) %>%
  mutate(Taxa_lvl1 = "Land Mammals") %>%
  mutate(Taxa_lvl2 = case_when(
    startsWith(Resource_Code, "21") ~ "Large Land Mammals",
    startsWith(Resource_Code, "22") ~ "Small Land Mammals",
  )) %>%
  mutate(Taxa_lvl3 = case_when( ##only small land mammals are broken down further 
    startsWith(Resource_Code, "2106") ~ "Black Bear",
    startsWith(Resource_Code, "2108") ~ "Brown Bear",
    startsWith(Resource_Code, "2110") ~ "Caribou",
    startsWith(Resource_Code, "2112") ~ "Deer",
    startsWith(Resource_Code, "2114") ~ "Elk", 
    startsWith(Resource_Code, "2116") ~ "Goat", 
    startsWith(Resource_Code, "2118") ~ "Moose",
    startsWith(Resource_Code, "2122") ~ "Dall Sheep",
    startsWith(Resource_Code, "2202") ~ "Beaver",
    startsWith(Resource_Code, "2204") ~ "Coyote",
    startsWith(Resource_Code, "2208") ~ "Fox", 
    startsWith(Resource_Code, "2210") ~ "Hare",
    startsWith(Resource_Code, "2212") ~ "Land Otter",
    startsWith(Resource_Code, "2216") ~ "Lynx",
    startsWith(Resource_Code, "2218") ~ "Marmot",
    startsWith(Resource_Code, "2220") ~ "Marten",
    startsWith(Resource_Code, "2222") ~ "Mink",
    startsWith(Resource_Code, "2224") ~ "Muskrat",
    startsWith(Resource_Code, "2226") ~ "Porcupine", 
    startsWith(Resource_Code, "2228") ~ "Squirrel", 
    startsWith(Resource_Code, "2230") ~ "Weasel",
    startsWith(Resource_Code, "2232") ~ "Wolf",
    startsWith(Resource_Code, "2234" ) ~ "Wolverine",
    startsWith(Resource_Code, "2120") ~ "Muskox",
    startsWith(Resource_Code, "2104") ~ "Bison", 
    startsWith(Resource_Code, "2199") ~ "Unknown Large Land Mammal",
    startsWith(Resource_Code, "2299") ~ "Unknown Small Land Mammals/Furbearers"
  )) %>%
  mutate(Taxa_lvl4 = case_when( ##no "genus" level for land mammals, so keeping this the same as the family, but doing this so matches with other large categories as want to retain that info
    startsWith(Resource_Code, "2106") ~ "Black Bear",
    startsWith(Resource_Code, "2108") ~ "Brown Bear",
    startsWith(Resource_Code, "2110") ~ "Caribou",
    startsWith(Resource_Code, "2112") ~ "Deer",
    startsWith(Resource_Code, "2114") ~ "Elk", 
    startsWith(Resource_Code, "2116") ~ "Goat", 
    startsWith(Resource_Code, "2118") ~ "Moose",
    startsWith(Resource_Code, "2122") ~ "Dall Sheep",
    startsWith(Resource_Code, "2202") ~ "Beaver",
    startsWith(Resource_Code, "2204") ~ "Coyote",
    startsWith(Resource_Code, "2208") ~ "Fox", 
    startsWith(Resource_Code, "2210") ~ "Hare",
    startsWith(Resource_Code, "2212") ~ "Land Otter",
    startsWith(Resource_Code, "2216") ~ "Lynx",
    startsWith(Resource_Code, "2218") ~ "Marmot",
    startsWith(Resource_Code, "2220") ~ "Marten",
    startsWith(Resource_Code, "2222") ~ "Mink",
    startsWith(Resource_Code, "2224") ~ "Muskrat",
    startsWith(Resource_Code, "2226") ~ "Porcupine", 
    startsWith(Resource_Code, "2228") ~ "Squirrel", 
    startsWith(Resource_Code, "2230") ~ "Weasel",
    startsWith(Resource_Code, "2232") ~ "Wolf",
    startsWith(Resource_Code, "2234" ) ~ "Wolverine",
    startsWith(Resource_Code, "2120") ~ "Muskox",
    startsWith(Resource_Code, "2104") ~ "Bison", 
    startsWith(Resource_Code, "2199") ~ "Unknown Large Land Mammal",
    startsWith(Resource_Code, "2299") ~ "Unknown Small Land Mammals/Furbearers"
  )) %>%
  mutate(Taxa_lvl5 = case_when( ##only small mammals broken down further
    startsWith(Resource_Code, "2106") ~ "Black Bear",
    startsWith(Resource_Code, "2108") ~ "Brown Bear",
    startsWith(Resource_Code, "2110") ~ "Caribou",
    startsWith(Resource_Code, "2112") ~ "Deer",
    startsWith(Resource_Code, "2114") ~ "Elk", 
    startsWith(Resource_Code, "2116") ~ "Goat", 
    startsWith(Resource_Code, "2118") ~ "Moose",
    startsWith(Resource_Code, "2122") ~ "Dall Sheep",
    startsWith(Resource_Code, "2202") ~ "Beaver",
    startsWith(Resource_Code, "2204") ~ "Coyote",
    startsWith(Resource_Code, "220804") ~ "Red Fox", 
    startsWith(Resource_Code, "221004") ~ "Snowshoe Hare",
    startsWith(Resource_Code, "221099") ~ "Unknown Hare",
    startsWith(Resource_Code, "2212") ~ "Land Otter",
    startsWith(Resource_Code, "2216") ~ "Lynx",
    startsWith(Resource_Code, "2218") ~ "Marmot",
    startsWith(Resource_Code, "2220") ~ "Marten",
    startsWith(Resource_Code, "2222") ~ "Mink",
    startsWith(Resource_Code, "2224") ~ "Muskrat",
    startsWith(Resource_Code, "2226") ~ "Porcupine", 
    startsWith(Resource_Code, "222804") ~ "Tree Squirrel",
    startsWith(Resource_Code, "222899") ~ "Unknown Squirrel", 
    startsWith(Resource_Code, "222806") ~ "Flying Squirrel",
    startsWith(Resource_Code, "2230") ~ "Weasel",
    startsWith(Resource_Code, "2232") ~ "Wolf",
    startsWith(Resource_Code, "2234" ) ~ "Wolverine",
    startsWith(Resource_Code, "2120") ~ "Muskox",
    startsWith(Resource_Code, "2104") ~ "Bison", 
    startsWith(Resource_Code, "2199") ~ "Unknown Large Land Mammal",
    startsWith(Resource_Code, "2299") ~ "Unknown Small Land Mammals/Furbearers"
  )) 

lm$Conversion_Units_To_Pounds <- as.character(lm$Conversion_Units_To_Pounds)
lm$Est_Comm_Population <- as.character(lm$Est_Comm_Population)

##select rows where the family level is not broken down further in a certain year/community
lm1 <- lm %>%
  filter(is.na(Sex)) %>%
  filter(!is.na(Taxa_lvl3)) %>%
  group_by(Site_Year_Code, Taxa_lvl3) %>%
  filter(!Taxa_lvl3 %in% Taxa_lvl3[!is.na(Taxa_lvl4)]) ##filters out familys where genus is not NA -- so i think these are only families where further genus level does not exist (i.e., family is the lowest level ID for that year/community)

##select rows where the genus level is not broken down further for a certain year/community
lm2 <- lm %>%
  filter(is.na(Sex)) %>%
  filter(!is.na(Taxa_lvl4)) %>%
  group_by(Site_Year_Code, Taxa_lvl3, Taxa_lvl4) %>%
  filter(!Taxa_lvl4 %in% Taxa_lvl4[!is.na(Taxa_lvl5)])

##select rows where species level is not broken down further for a certain year/community 
lm3 <- lm %>%
  filter(is.na(Sex)) %>%
  filter(!is.na(Taxa_lvl5))

lm4 <- rbind(lm1, lm2, lm3) %>%
  select(Project_Name, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest, Conversion_Units_To_Pounds, Resource_Harvest_Units, Est_Comm_Population) %>%
  mutate(Taxa_lvl4 = coalesce(Taxa_lvl4, Taxa_lvl3)) %>%
  mutate(Taxa_lvl5 = coalesce(Taxa_lvl5, Taxa_lvl4))

lm_final <- lm4 %>%
  group_by(Project_Name, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Conversion_Units_To_Pounds, Resource_Harvest_Units, Est_Comm_Population) %>%
  summarise(across(where(is.numeric), sum))

#clean up environment
rm(lm1, lm2, lm3, lm4)
##Note: there are some small land mammals where there are # of resources harvested, but no estimated weight, so will need to decide if we want to go and determine conversion units and then calculate estimated weight
##I feel like an intermediate solution oculd be to determine estimated dressed weight for these species where this has occurred, so not needing to re-do all convserions, but for these particular cases to have some  estimate of weight that is comparable to the other estimates  
##This doesn't solve the issue that some of the conversion estimates are likely mistakes in the database (e.g., brown bear conversion used for black bear conversion but this is an option)
##Or clean up and fix conversion factors by project/species, add in ones where it is 0 based on literature, and then recalculate estimated weights based on # reported
##However, can not fix the rows where the # reported is 0, however these all have an estimated amount harvested, so can calculate that way (not sure where these values came from but they are there)

#lm_final_na_resharvested <- lm_final %>%
#  filter(is.na(Number_Of_Resource_Harvested))

#3) MARINE MAMMALS ------------------
marine_mammal_code <- "3"

mm <- df_comp %>% 
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
    startsWith(Resource_Code, "3008") ~ "Seal", 
    startsWith(Resource_Code, "3010") ~ "Sea Otter",
    startsWith(Resource_Code, "3012") ~ "Stellar Sea Lion",
    startsWith(Resource_Code, "3016") ~ "Whale",
    startsWith(Resource_Code, "3099") ~ "Unknown Marine Mammals",
    startsWith(Resource_Code, "3014") ~ "Walrus",
  )) %>%
  mutate(Taxa_lvl4 = case_when( ##keeping this as "genus" level, just to match to other categories 
    startsWith(Resource_Code, "3008") ~ "Seal", 
    startsWith(Resource_Code, "3010") ~ "Sea Otter",
    startsWith(Resource_Code, "3012") ~ "Stellar Sea Lion",
    startsWith(Resource_Code, "3016") ~ "Whale",
    startsWith(Resource_Code, "3099") ~ "Unknown Marine Mammals",
    startsWith(Resource_Code, "3014") ~ "Walrus",
  )) %>%
  mutate(Taxa_lvl5 = case_when(
    startsWith(Resource_Code, "30080400") ~ "Fur Seal", 
    startsWith(Resource_Code, "30080404") ~ "Fur Seal (other)", ##what is a fur seal other?? 
    startsWith(Resource_Code, "30080600") ~ "Harbor Seal",
    startsWith(Resource_Code, "30080604") ~ "Harbour Seal (saltwater)", ##is this a different habitat? can it be captured that way? as not really different species
    startsWith(Resource_Code, "300899") ~ "Unknown Seal",
    startsWith(Resource_Code, "300888") ~ "Unknown Seal Oil",
    startsWith(Resource_Code, "301602") ~ "Belukha",
    startsWith(Resource_Code, "301606") ~ "Bowhead",
    startsWith(Resource_Code, "301699") ~ "Unknown Whale",
    startsWith(Resource_Code, "3014") ~ "Walrus",
    startsWith(Resource_Code, "3010") ~ "Sea Otter",
    startsWith(Resource_Code, "3012") ~ "Stellar Sea Lion",
    startsWith(Resource_Code, "3099") ~ "Unknown Marine Mammals",
  ))

mm$Conversion_Units_To_Pounds <- as.character(mm$Conversion_Units_To_Pounds)
mm$Est_Comm_Population <- as.character(mm$Est_Comm_Population)

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

mm4 <- rbind(mm1, mm2, mm3) %>%
  select(Project_Name, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest, Conversion_Units_To_Pounds, Resource_Harvest_Units, Est_Comm_Population) %>%
  mutate(Taxa_lvl4 = coalesce(Taxa_lvl4, Taxa_lvl3)) %>%
  mutate(Taxa_lvl5 = coalesce(Taxa_lvl5, Taxa_lvl4)) %>%
  filter(Taxa_lvl5 != "Harbour Seal (saltwater)") %>% ##removed harbor seal (saltwater) and fur seal (other) as always a replicate when it does come up
  filter(Taxa_lvl5 != "Fur Seal (other)")
  
mm_final <- mm4 %>%
  group_by(Project_Name, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Conversion_Units_To_Pounds, Resource_Harvest_Units, Est_Comm_Population) %>%
  summarise(across(where(is.numeric), sum))

rm(mm1, mm2, mm3, mm4)

#4) BIRDS AND EGGS -----------------

birds_eggs_code <- "4"

be <- df_comp %>% 
  filter(str_detect(Resource_Code, '^4')) 

be$Resource_Code  <- format(be$Resource_Code, scientific = FALSE)
be$Resource_Code <- as.character(be$Resource_Code) 
str(be)

be <- be %>%
  mutate(Habitat = "Terrestrial") %>% ##for purposes of my own research project may want to go through these and be more specific
  mutate(Season = case_when(
    grepl("Winter", Resource_Name) ~ "Winter",
    grepl("Fall", Resource_Name) ~ "Fall",
    grepl("Summer", Resource_Name) ~ "Summer",
    grepl("Spring", Resource_Name) ~ "Spring",
    grepl("Season Unknown", Resource_Name) ~ "Unknown",
  )) %>%
  mutate(Taxa_lvl1 = "Birds and Eggs") %>%
  mutate(Taxa_lvl2 = case_when(
    startsWith(Resource_Code, "41") ~ "Migratory Birds",
    startsWith(Resource_Code, "42") ~ "Other Birds", 
    startsWith(Resource_Code, "43") ~ "Bird Eggs",
  )) %>%
  mutate(Taxa_lvl3 = case_when(
    startsWith(Resource_Code, "4102") ~ "Duck",
    startsWith(Resource_Code, "4104") ~ "Geese",
    startsWith(Resource_Code, "4106") ~ "Swan",
    startsWith(Resource_Code, "4108") ~ "Crane",
    startsWith(Resource_Code, "4110") ~ "Shorebird",
    startsWith(Resource_Code, "4112") ~ "Seabirds and Loons",
    startsWith(Resource_Code, "4114") ~ "Heron",
    startsWith(Resource_Code, "4218") ~ "Upland Game Birds",
    startsWith(Resource_Code, "4288") ~ "Unknown Other Birds", ##is this. the right place to put this? 
    startsWith(Resource_Code, "4302") ~ "Duck Eggs",
    startsWith(Resource_Code, "4304") ~ "Geese Eggs",
    startsWith(Resource_Code, "4306") ~ "Swan Eggs",
    startsWith(Resource_Code, "4308") ~ "Crane Eggs",
    startsWith(Resource_Code, "4310") ~ "Shorebird Eggs",
    startsWith(Resource_Code, "4312") ~ "Seabirds and Loon Eggs",
    startsWith(Resource_Code, "4318") ~ "Upland Game Bird Eggs",
    startsWith(Resource_Code, "4399") ~ "Unknown Eggs",
  )) %>% 
  mutate(Taxa_lvl4 = case_when(
    startsWith(Resource_Code, "410202") ~ "Bufflehead",
    startsWith(Resource_Code, "410204") ~ "Canvasback",
    startsWith(Resource_Code, "410208") ~ "Gadwall",
    startsWith(Resource_Code, "410210") ~ "Goldeneye", 
    startsWith(Resource_Code, "410212") ~ "Harlequin",
    startsWith(Resource_Code, "410214") ~ "Mallard",
    startsWith(Resource_Code, "410216") ~ "Merganser", 
    startsWith(Resource_Code, "410218") ~ "Long-tailed Duck (Oldsquaw)",
    startsWith(Resource_Code, "410220") ~ "Northern Pintail",
    startsWith(Resource_Code, "410222") ~ "Redhead Duck",
    startsWith(Resource_Code, "410226") ~ "Scaup", 
    startsWith(Resource_Code, "410228") ~ "Scoter", 
    startsWith(Resource_Code, "410232") ~ "Teal", 
    startsWith(Resource_Code, "410236") ~ "Wigeon", 
    startsWith(Resource_Code, "410299") ~ "Unknown Duck",
    startsWith(Resource_Code, "410402") ~ "Brant",
    startsWith(Resource_Code, "410404") ~ "Canada Geese", 
    startsWith(Resource_Code, "410406") ~ "Emperor Geese",
    startsWith(Resource_Code, "410408") ~ "Snow Geese",
    startsWith(Resource_Code, "410410") ~ "White-fronted Geese",
    startsWith(Resource_Code, "410499") ~ "Unknown Geese",
    startsWith(Resource_Code, "410604") ~ "Tundra Swan (whistling)",
    startsWith(Resource_Code, "410699") ~ "Unknown Swan",
    startsWith(Resource_Code, "410802") ~ "Sandhill Crane",
    startsWith(Resource_Code, "411002") ~ "Wilson's Snipe",
    startsWith(Resource_Code, "411004") ~ "Black Oystercatcher",
    startsWith(Resource_Code, "411099") ~ "Unknown Shorebirds", 
    startsWith(Resource_Code, "411204") ~ "Cormorants",
    startsWith(Resource_Code, "411208") ~ "Grebe", 
    startsWith(Resource_Code, "411210") ~ "Guillemot",
    startsWith(Resource_Code, "411212") ~ "Gull",  
    startsWith(Resource_Code, "411216") ~ "Loon",
    startsWith(Resource_Code, "411222") ~ "Puffin",
    startsWith(Resource_Code, "411226") ~ "Tern",
    startsWith(Resource_Code, "411299") ~ "Unknown Seabirds",
    startsWith(Resource_Code, "4114") ~ "Great Blue Heron",
    startsWith(Resource_Code, "421802") ~ "Grouse", 
    startsWith(Resource_Code, "421804") ~ "Ptarmigan", 
    startsWith(Resource_Code, "421899") ~ "Unknown Upland Game Birds",
    startsWith(Resource_Code, "430214") ~ "Mallard Eggs", 
    startsWith(Resource_Code, "430299") ~ "Unknown Duck Eggs",
    startsWith(Resource_Code, "430404") ~ "Canada Geese Eggs",
    startsWith(Resource_Code, "430499") ~ "Unknown Geese Eggs",
    startsWith(Resource_Code, "43060") ~ "Swan Eggs",
    startsWith(Resource_Code, "43069") ~ "Unknown Swan Eggs", 
    startsWith(Resource_Code, "430802") ~ "Sandhill Crane Eggs",
    startsWith(Resource_Code, "430899") ~ "Unknown Crane",
    startsWith(Resource_Code, "431004") ~ "Black Oystercatcher Eggs",
    startsWith(Resource_Code, "43109901" ) ~ "Unknown Small Shorebird Eggs",
    startsWith(Resource_Code, "43109902") ~ "Unknown Large Shorebird Eggs",
    startsWith(Resource_Code, "431210") ~ "Guillemot Eggs",
    startsWith(Resource_Code, "43121204") ~ "Glaucous Winged Gull Eggs", ##
    startsWith(Resource_Code, "43121299") ~ "Unknown Gull Eggs",
    startsWith(Resource_Code, "4312160") ~ "Loon Eggs",
    startsWith(Resource_Code, "4312169") ~ "Unknown Loon Eggs",
    startsWith(Resource_Code, "43121802") ~ "Common Murre Eggs",  
    startsWith(Resource_Code, "4312260") ~ "Tern Eggs",
    startsWith(Resource_Code, "4312269") ~ "Unknown Tern Eggs",
    startsWith(Resource_Code, "43129") ~ "Unknown Seabird Eggs",
    startsWith(Resource_Code, "431802") ~ "Grouse Eggs",
    startsWith(Resource_Code, "4318029") ~ "Unknown Grouse Eggs",
    startsWith(Resource_Code, "4318040") ~ "Ptarmigan Eggs",
    startsWith(Resource_Code, "4318049") ~ "Unknown Ptarmigan Eggs",
  )) %>%
  mutate(Taxa_lvl5 = case_when(
    startsWith(Resource_Code, "410202") ~ "Bufflehead",
    startsWith(Resource_Code, "410204") ~ "Canvasback",
    startsWith(Resource_Code, "410208") ~ "Gadwall",
    startsWith(Resource_Code, "41021002") ~ "Barrows Goldeneye", 
    startsWith(Resource_Code, "41021004") ~ "Common Goldeneye",
    startsWith(Resource_Code, "4102109") ~ "Unknown Goldeneye",
    startsWith(Resource_Code, "410212") ~ "Harlequin",
    startsWith(Resource_Code, "410214") ~ "Mallard",
    startsWith(Resource_Code, "41021602") ~ "Common Merganser",
    startsWith(Resource_Code, "41021604") ~ "Red-Breasted Merganser",
    startsWith(Resource_Code, "41021699") ~ "Unknown Merganser",
    startsWith(Resource_Code, "410218") ~ "Long-tailed Duck (Oldsquaw)",
    startsWith(Resource_Code, "410220") ~ "Northern Pintail",
    startsWith(Resource_Code, "410222") ~ "Redhead Duck",
    startsWith(Resource_Code, "41022602") ~ "Greater Scaup",
    startsWith(Resource_Code, "41022604") ~"Lesser Scaup",
    startsWith(Resource_Code, "41022699") ~ "Unknown Scaup",
    startsWith(Resource_Code, "41022804") ~ "Surf Scoter", 
    startsWith(Resource_Code, "41022806") ~ "White-winged Scoter",
    startsWith(Resource_Code, "41022899") ~ "Unknown Scoter",
    startsWith(Resource_Code, "41023206") ~ "Green-Winged Teal", 
    startsWith(Resource_Code, "41023299") ~ "Unknown Teal",
    startsWith(Resource_Code, "41023602") ~ "American Wigeon", 
    startsWith(Resource_Code, "41023699") ~ "Unknown Wigeon",
    startsWith(Resource_Code, "410299") ~ "Unknown Duck",
    startsWith(Resource_Code, "410402") ~ "Brant",
    startsWith(Resource_Code, "41040406") ~ "Dusky Canada Geese", 
    startsWith(Resource_Code, "41040408") ~ "Lesser Canada Geese",
    startsWith(Resource_Code, "4104041") ~ "Vancouver Canada Geese",
    startsWith(Resource_Code, "41040499") ~ "Unknown Canada Geese",
    startsWith(Resource_Code, "410406") ~ "Emperor Geese",
    startsWith(Resource_Code, "410408") ~ "Snow Geese",
    startsWith(Resource_Code, "410410") ~ "White-fronted Geese",
    startsWith(Resource_Code, "410499") ~ "Unknown Geese",
    startsWith(Resource_Code, "410604") ~ "Tundra Swan (whistling)",
    startsWith(Resource_Code, "410699") ~ "Unknown Swan",
    startsWith(Resource_Code, "410802") ~ "Sandhill Crane",
    startsWith(Resource_Code, "411002") ~ "Wilson's Snipe",
    startsWith(Resource_Code, "411004") ~ "Black Oystercatcher",
    startsWith(Resource_Code, "41109901") ~ "Unknown Small Shorebirds", 
    startsWith(Resource_Code, "41109902") ~ "Unknown Large Shorebirds",
    startsWith(Resource_Code, "411204") ~ "Cormorants",
    startsWith(Resource_Code, "41120802") ~ "Horned Grebe", 
    startsWith(Resource_Code, "41120804") ~ "Red Necked Grebe",
    startsWith(Resource_Code, "41120899") ~ "Unknown Grebe",
    startsWith(Resource_Code, "411210") ~ "Guillemot",
    startsWith(Resource_Code, "411212") ~ "Gull",  
    startsWith(Resource_Code, "4112160") ~ "Loon",
    startsWith(Resource_Code, "4112169") ~ "Unknown Loon",
    startsWith(Resource_Code, "4112220") ~ "Puffin",
    startsWith(Resource_Code, "4112229") ~ "Unknown Puffin",
    startsWith(Resource_Code, "411226") ~ "Tern",
    startsWith(Resource_Code, "411299") ~ "Unknown Seabird",
    startsWith(Resource_Code, "4114") ~ "Great Blue Heron",
    startsWith(Resource_Code, "42180202") ~ "Spruce Grouse", 
    startsWith(Resource_Code, "42180206") ~ "Ruffed Grouse",
    startsWith(Resource_Code, "4218029") ~ "Unknown Grouse",
    startsWith(Resource_Code, "42180402") ~ "Rock Ptarmigan", 
    startsWith(Resource_Code, "42180404") ~ "Willow Ptarmigan",
    startsWith(Resource_Code, "4218049") ~ "Unknown Ptarmigan",
    startsWith(Resource_Code, "421899") ~ "Unknown Upland Game Birds",
    startsWith(Resource_Code, "430214") ~ "Mallard Eggs", 
    startsWith(Resource_Code, "430299") ~ "Unknown Duck Eggs",
    startsWith(Resource_Code, "43040408") ~ "Lesser Canada Geese Eggs",
    startsWith(Resource_Code, "4304049") ~ "Unknown Canada Geese Eggs",
    startsWith(Resource_Code, "430499") ~ "Unknown Geese Eggs",
    startsWith(Resource_Code, "43060") ~ "Swan Eggs",
    startsWith(Resource_Code, "43069") ~ "Unknown Swan Eggs",
    startsWith(Resource_Code, "430802") ~ "Sandhill Crane Eggs",
    startsWith(Resource_Code, "430899") ~ "Unknown Crane Eggs",
    startsWith(Resource_Code, "431004") ~ "Black Oystercatcher Eggs",
    startsWith(Resource_Code, "43109901" ) ~ "Unknown Small Shorebird Eggs",
    startsWith(Resource_Code, "43109902") ~ "Unknown Large Shorebird Eggs",
    startsWith(Resource_Code, "431210") ~ "Guillemot Eggs",
    startsWith(Resource_Code, "43121204") ~ "Glaucous Winged Gull Eggs", ##
    startsWith(Resource_Code, "43121299") ~ "Unknown Gull Eggs",
    startsWith(Resource_Code, "4312160") ~ "Loon Eggs",
    startsWith(Resource_Code, "4312169") ~ "Unknown Loon Eggs",
    startsWith(Resource_Code, "43121802") ~ "Common Murre Eggs", 
    startsWith(Resource_Code, "4312260") ~ "Tern Eggs",
    startsWith(Resource_Code, "4312269") ~ "Unknown Tern Eggs",
    startsWith(Resource_Code, "431299") ~ "Unknown Seabird Eggs",
    startsWith(Resource_Code, "431802") ~ "Grouse Eggs",
    startsWith(Resource_Code, "4318029") ~ "Unknown Grouse Eggs",
    startsWith(Resource_Code, "4318040") ~ "Ptarmigan Eggs",
    startsWith(Resource_Code, "4318049") ~ "Unknown Ptarmigan Eggs",
  )) 


##are there other groups that are only identified to a higher level like family that i am missing..???? yes...need to re do this. 
be$Conversion_Units_To_Pounds <- as.character(be$Conversion_Units_To_Pounds)
be$Est_Comm_Population <- as.character(be$Est_Comm_Population)

be1 <- be %>%
  filter(is.na(Season)) %>%
  filter(!is.na(Taxa_lvl3)) %>%
  group_by(Site_Year_Code, Taxa_lvl3) %>%
  filter(!Taxa_lvl3 %in% Taxa_lvl3[!is.na(Taxa_lvl4)]) ##filters out familys where genus is not NA -- so i think these are only families where further genus level does not exist

be2 <- be %>%
  filter(is.na(Season)) %>%
  filter(!is.na(Taxa_lvl4)) %>%
  group_by(Site_Year_Code, Taxa_lvl3, Taxa_lvl4) %>%
  filter(!Taxa_lvl4 %in% Taxa_lvl4[!is.na(Taxa_lvl5)])

be3 <- be %>%
  filter(is.na(Season)) %>%
  filter(!is.na(Taxa_lvl5))


be4 <- rbind(be1, be2, be3) %>%
  select(Project_Name, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest, Conversion_Units_To_Pounds, Resource_Harvest_Units, Est_Comm_Population) %>%
  mutate(Taxa_lvl4 = coalesce(Taxa_lvl4, Taxa_lvl3)) %>%
  mutate(Taxa_lvl5 = coalesce(Taxa_lvl5, Taxa_lvl4)) %>%
  distinct() ## for sitka_2013 for some reason the large and small shorebirds are duplicated, just removing these (only duplicate row)


be_final <- be4 %>%
  group_by(Project_Name, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Conversion_Units_To_Pounds, Resource_Harvest_Units, Est_Comm_Population) %>%
  summarise(across(where(is.numeric), sum))

rm(be1, be2, be3, be4)

#5) MARINE INVERTEBRATES -----------------
marine_inverts_code <- "5"

mi <- df_comp %>% 
  filter(str_detect(Resource_Code, '^5')) 

mi$Resource_Code  <- format(mi$Resource_Code, scientific = FALSE)
mi$Resource_Code <- as.character(mi$Resource_Code) 
str(mi)

mi <- mi %>%
  mutate(Fishing_Gear_Type = case_when(
    endsWith(Resource_Code, "1") ~ "CF_Retention", ##CF_retention
    endsWith(Resource_Code, "2") ~ "Non Commercial Gear", 
  )) %>%
  mutate(Taxa_lvl1 = "Marine Invertebrates") %>%
  mutate(Taxa_lvl2 = "Marine Invertebrates") %>%
  mutate(Taxa_lvl3 = case_when(
    startsWith(Resource_Code, "5002") ~ "Abalone",
    startsWith(Resource_Code, "5004") ~ "Chiton",
    startsWith(Resource_Code, "5006") ~ "Clam",
    startsWith(Resource_Code, "5008") ~ "Cockle",
    startsWith(Resource_Code, "5010") ~ "Crab",
    startsWith(Resource_Code, "5012") ~ "Clam",
    startsWith(Resource_Code, "5018") ~ "Limpet",
    startsWith(Resource_Code, "5020") ~ "Mussel",
    startsWith(Resource_Code, "5022") ~ "Octopus",
    startsWith(Resource_Code, "5024") ~ "Oyster",
    startsWith(Resource_Code, "5026") ~ "Scallop",
    startsWith(Resource_Code, "5030") ~ "Sea Cucumber",
    startsWith(Resource_Code, "5032") ~ "Sea Urchin",
    startsWith(Resource_Code, "5034") ~ "Shrimp",
    startsWith(Resource_Code, "5037") ~ "Starfish",
    startsWith(Resource_Code, "5038") ~ "Squid",
    startsWith(Resource_Code, "5040") ~ "Whelk",
    startsWith(Resource_Code, "5099") ~ "Unknown Marine Invertebrates",
  )) %>%
  mutate(Taxa_lvl4 = case_when(
    startsWith(Resource_Code, "5002") ~ "Abalone",
    startsWith(Resource_Code, "500404") ~ "Red (large) Chiton",
    startsWith(Resource_Code, "500408") ~ "Black (small) Chiton",
    startsWith(Resource_Code, "500499") ~ "Unknown Chiton",
    startsWith(Resource_Code, "500602") ~ "Butter Clam",
    startsWith(Resource_Code, "500606") ~ "Horse Clam",
    startsWith(Resource_Code, "500608") ~ "Pacific Littleneck Clam",
    startsWith(Resource_Code, "500610") ~ "Pinkneck Clam",
    startsWith(Resource_Code, "500612") ~ "Razor Clam",
    startsWith(Resource_Code, "50069") ~ "Unknown Clam",
    startsWith(Resource_Code, "500802") ~ "Basket Cockle",
    startsWith(Resource_Code, "500804") ~ "Heart Cockle",
    startsWith(Resource_Code, "500899") ~ "Unknown Cockle",
    startsWith(Resource_Code, "501002") ~ "Box Crab",
    startsWith(Resource_Code, "501004") ~ "Dungeness Crab",
    startsWith(Resource_Code, "501008") ~ "King Crab", ##are the king crabs always broken down? do we need another level here?
    startsWith(Resource_Code, "501012") ~ "Tanner Crab", ##tanner crab also has summary level
    startsWith(Resource_Code, "50109") ~ "Unknown Crab",
    startsWith(Resource_Code, "5012") ~ "Geoduck Clam",
    startsWith(Resource_Code, "5014") ~ "Limpet",
    startsWith(Resource_Code, "502002") ~ "Blue Mussel",
    startsWith(Resource_Code, "502099") ~ "Unknown Mussel",
    startsWith(Resource_Code, "5022") ~ "Octopus",
    startsWith(Resource_Code, "50240") ~ "Oyster",
    startsWith(Resource_Code, "50249") ~ "Unknown Oyster",
    startsWith(Resource_Code, "502602") ~ "Weathervane Scallop",
    startsWith(Resource_Code, "502604") ~ "Rock Scallops",
    startsWith(Resource_Code, "502699") ~ "Unknown Scallop",
    startsWith(Resource_Code, "503004") ~ "Yein Sea Cucumber",
    startsWith(Resource_Code, "503099") ~ "Unknown Sea Cucumber",
    startsWith(Resource_Code, "503202") ~ "Green Sea Urchin",
    startsWith(Resource_Code, "503204") ~ "Red Sea Urchin",
    startsWith(Resource_Code, "504306") ~ "Purple Sea Urchin",
    startsWith(Resource_Code, "50329") ~ "Unknown Sea Urchin",
    startsWith(Resource_Code, "5034") ~ "Shrimp",
    startsWith(Resource_Code, "5037") ~ "Starfish",
    startsWith(Resource_Code, "5038") ~ "Squid",
    startsWith(Resource_Code, "5040") ~ "Whelk",
    startsWith(Resource_Code, "5099") ~ "Unknown Marine Invertebrates",
  )) %>%
  mutate(Taxa_lvl5 = case_when(
    startsWith(Resource_Code, "5002") ~ "Abalone",
    startsWith(Resource_Code, "500404") ~ "Red (large) Chiton",
    startsWith(Resource_Code, "500408") ~ "Black (small) Chiton",
    startsWith(Resource_Code, "500499") ~ "Unknown Chiton",
    startsWith(Resource_Code, "500602") ~ "Butter Clam",
    startsWith(Resource_Code, "500606") ~ "Horse Clam",
    startsWith(Resource_Code, "500608") ~ "Pacific Littleneck Clam",
    startsWith(Resource_Code, "500610") ~ "Pinkneck Clam",
    startsWith(Resource_Code, "500612") ~ "Razor Clam",
    startsWith(Resource_Code, "50069") ~ "Unknown Clam",
    startsWith(Resource_Code, "500802") ~ "Basket Cockle",
    startsWith(Resource_Code, "500804") ~ "Heart Cockle",
    startsWith(Resource_Code, "500899") ~ "Unknown Cockle",
    startsWith(Resource_Code, "501002") ~ "Box Crab",
    startsWith(Resource_Code, "501004") ~ "Dungeness Crab",
    startsWith(Resource_Code, "50100802") ~ "Blue King Crab", ##are the king crabs always broken down? do we need another level here?
    startsWith(Resource_Code, "50100804") ~ "Brown King Crab",
    startsWith(Resource_Code, "50100808") ~ "Red King Crab",
    startsWith(Resource_Code, "50100899") ~ "Unknown King Crab",
    startsWith(Resource_Code, "50101202") ~ "Tanner Crab, Bairdi", ##tanner crab also has summary level
    startsWith(Resource_Code, "5010129") ~ "Unknown Tanner Crab",
    startsWith(Resource_Code, "50109") ~ "Unknown Crab",
    startsWith(Resource_Code, "5012") ~ "Geoduck Clam",
    startsWith(Resource_Code, "5014") ~ "Limpet",
    startsWith(Resource_Code, "502002") ~ "Blue Mussel",
    startsWith(Resource_Code, "502099") ~ "Unknown Mussel",
    startsWith(Resource_Code, "5022") ~ "Octopus",
    startsWith(Resource_Code, "50240") ~ "Oyster",
    startsWith(Resource_Code, "50249") ~ "Unknown Oyster",
    startsWith(Resource_Code, "502602") ~ "Weathervane Scallop",
    startsWith(Resource_Code, "502604") ~ "Rock Scallops",
    startsWith(Resource_Code, "502699") ~ "Unknown Scallop",
    startsWith(Resource_Code, "503004") ~ "Yein Sea Cucumber",
    startsWith(Resource_Code, "503099") ~ "Unknown Sea Cucumber",
    startsWith(Resource_Code, "503202") ~ "Green Sea Urchin",
    startsWith(Resource_Code, "503204") ~ "Red Sea Urchin",
    startsWith(Resource_Code, "504306") ~ "Purple Sea Urchin",
    startsWith(Resource_Code, "50329") ~ "Unknown Sea Urchin",
    startsWith(Resource_Code, "5034") ~ "Shrimp",
    startsWith(Resource_Code, "5037") ~ "Starfish",
    startsWith(Resource_Code, "5038") ~ "Squid",
    startsWith(Resource_Code, "5040") ~ "Whelk",
    startsWith(Resource_Code, "5099") ~ "Unknown Marine Invertebrates",
  )) %>%
  mutate(Habitat = case_when(
    grepl("King Crab", Taxa_lvl4) ~ "Marine",
    startsWith(Taxa_lvl3, "Octopus") ~ "Marine",
    startsWith(Taxa_lvl3, "Scallop") ~ "Marine", 
    startsWith(Taxa_lvl3, "Shrimp") ~ "Marine",
    grepl("Tanner Crab", Taxa_lvl5) ~ "Marine",
    startsWith(Taxa_lvl3, "Abalone") ~ "Nearshore",
    startsWith(Taxa_lvl3, "Chiton") ~ "Nearshore",
    startsWith(Taxa_lvl3, "Clam") ~ "Nearshore",
    startsWith(Taxa_lvl3, "Cockle") ~ "Nearshore",
    startsWith(Taxa_lvl5, "Dungeness") ~ "Nearshore",
    startsWith(Taxa_lvl3, "Geoduck") ~ "Nearshore",
    startsWith(Taxa_lvl3, "Limpet") ~ "Nearshore",
    startsWith(Taxa_lvl3, "Mussel") ~ "Nearshore",
    startsWith(Taxa_lvl3, "Sea Cucumber") ~ "Nearshore",
    startsWith(Taxa_lvl3, "Sea Urchin") ~ "Nearshore",
    startsWith(Taxa_lvl3, "Squid") ~ "Marine", 
    startsWith(Taxa_lvl5, "Box Crab") ~ "Marine",
    startsWith(Taxa_lvl5, "Unknown Crab") ~ "Marine",
    startsWith(Taxa_lvl3, "Oyster") ~ "Marine",
    startsWith(Taxa_lvl3, "Starfish") ~ "Nearshore",
    startsWith(Taxa_lvl3, "Unknown Marine Invert") ~ "Marine",
    startsWith(Taxa_lvl3, "Whelk") ~ "Nearshore",
  ))

mi$Conversion_Units_To_Pounds <- as.character(mi$Conversion_Units_To_Pounds)
mi$Est_Comm_Population <- as.character(mi$Est_Comm_Population)
str(mi)
##select rows where the family level is not broken down further in a certain year/community
mi1 <- mi %>%
  filter(is.na(Fishing_Gear_Type)) %>%
  filter(!is.na(Taxa_lvl3)) %>%
  group_by(Site_Year_Code, Taxa_lvl3) %>%
  filter(!Taxa_lvl3 %in% Taxa_lvl3[!is.na(Taxa_lvl4)]) ##filters out familys where genus is not NA -- so i think these are only families where further genus level does not exist (i.e., family is the lowest level ID for that year/community)

##select rows where the genus level is not broken down further for a certain year/community
mi2 <- mi %>%
  filter(is.na(Fishing_Gear_Type)) %>%
  filter(!is.na(Taxa_lvl4)) %>%
  group_by(Site_Year_Code, Taxa_lvl3, Taxa_lvl4) %>%
  filter(!Taxa_lvl4 %in% Taxa_lvl4[!is.na(Taxa_lvl5)])

##select rows where species level is not broken down further for a certain year/community 
mi3 <- mi %>%
  filter(is.na(Fishing_Gear_Type)) %>%
  filter(!is.na(Taxa_lvl5))

mi4 <- rbind(mi1, mi2, mi3) %>%
  select(Project_Name, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest, Conversion_Units_To_Pounds, Resource_Harvest_Units, Est_Comm_Population) %>%
  mutate(Taxa_lvl4 = coalesce(Taxa_lvl4, Taxa_lvl3)) %>%
  mutate(Taxa_lvl5 = coalesce(Taxa_lvl5, Taxa_lvl4)) 

mi_final <- mi4 %>%
  group_by(Project_Name, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Conversion_Units_To_Pounds, Resource_Harvest_Units, Est_Comm_Population) %>%
  summarise(across(where(is.numeric), sum))

rm(mi1, mi2, mi3, mi4)
#6) VEGETATION --------------------

veg_code <- "6"

veg <- df_comp %>% 
  filter(str_detect(Resource_Code, '^6')) 

veg$Resource_Code  <- format(veg$Resource_Code, scientific = FALSE)
veg$Resource_Code <- as.character(veg$Resource_Code) 
str(veg)
sp_veg <- veg %>%
  distinct(Resource_Code, Resource_Name)

veg <- veg %>%
  mutate(Taxa_lvl1 = "Vegetation") %>%
  mutate(Taxa_lvl2 = "Vegetation") %>% ##don't think has another general cat
  mutate(Taxa_lvl3 = case_when(
    startsWith(Resource_Code, "6010") ~ "Berries",
    startsWith(Resource_Code, "6020400") ~ "Mushrooms", 
    startsWith(Resource_Code, "602046") ~ "Mushrooms",
    startsWith(Resource_Code, "6020") ~ "Plants/Greens", 
 ##want to break this down and separate this more... or maybe just add other functional traits but this is very broad
    startsWith(Resource_Code, "6030") ~ "Seaweed/Kelp",
    startsWith(Resource_Code, "6040") ~ "Wood", ##wood will have to be removed for diversity calculations
    startsWith(Resource_Code, "6050") ~ "Coal", ##remove for any food web work
    startsWith(Resource_Code, "6053") ~ "Seaweed/Kelp", ##remove for diversity calc
  )) %>% 
  mutate(Taxa_lvl4 = case_when(   ##may want to break this down and separate this more... or maybe just add other functional traits but this is very broad, putting this level in to match 
    startsWith(Resource_Code, "6010") ~ "Berries",
    startsWith(Resource_Code, "6020400") ~ "Mushrooms", 
    startsWith(Resource_Code, "602046") ~ "Mushrooms",
    startsWith(Resource_Code, "6020") ~ "Plants/Greens", 
    startsWith(Resource_Code, "6030") ~ "Seaweed/Kelp",
    startsWith(Resource_Code, "6040") ~ "Wood", ##wood will have to be removed for diversity calculations
    startsWith(Resource_Code, "6050") ~ "Coal", ##remove for diversity calc
    startsWith(Resource_Code, "6053") ~ "Seaweed/Kelp", ##remove for diversity calc
  )) %>%
  mutate(Taxa_lvl5 = case_when(
    startsWith(Resource_Code, "601002") ~ "Blueberry",
    startsWith(Resource_Code, "601004") ~ "Low Bush Cranberry",
    startsWith(Resource_Code, "601006") ~ "High Bush Cranberry",
    startsWith(Resource_Code, "601008") ~ "Elderberry",
    startsWith(Resource_Code, "601010") ~ "Gooseberry",
    startsWith(Resource_Code, "601012") ~ "Currants",
    startsWith(Resource_Code, "601014") ~ "Huckleberry",
    startsWith(Resource_Code, "601016") ~ "Cloud Berry",
    startsWith(Resource_Code, "601018") ~ "Nagoonberry",
    startsWith(Resource_Code, "601020") ~ "Raspberry",
    startsWith(Resource_Code, "601022") ~ "Salmonberry",
    startsWith(Resource_Code, "601024") ~ "Soapberry",
    startsWith(Resource_Code, "601026") ~ "Strawberry",
    startsWith(Resource_Code, "601028") ~ "Thimbleberry",
    startsWith(Resource_Code, "601032") ~ "Twisted Stalk Berry (Watermelon Berry)",
    startsWith(Resource_Code, "602002") ~ "Beach Asparagus",
    startsWith(Resource_Code, "602004") ~ "Goose Tongue",
    startsWith(Resource_Code, "602006") ~ "Wild Rhubarb",
    startsWith(Resource_Code, "602009") ~ "Eskimo Potato",
    startsWith(Resource_Code, "602012") ~ "Devils Club",
    startsWith(Resource_Code, "602014") ~ "Fiddlehead Ferns",
    startsWith(Resource_Code, "602016") ~ "Nettle",
    startsWith(Resource_Code, "602018") ~ "Hudson Bay Tea",
    startsWith(Resource_Code, "602020") ~ "Indian Rice",
    startsWith(Resource_Code, "602022") ~ "Mint",
    startsWith(Resource_Code, "602024") ~ "Salmonberry Shoots", ##when thinking about diversity, is this really another species?
    startsWith(Resource_Code, "602026") ~ "Skunk Cabbage",
    startsWith(Resource_Code, "602028") ~ "Sourdock",
    startsWith(Resource_Code, "602030") ~ "Spruce Tips",
    startsWith(Resource_Code, "602032") ~ "Wild Celery",
    startsWith(Resource_Code, "602034") ~ "Wild Parsley",
    startsWith(Resource_Code, "602036") ~ "Wild Rose Hips",
    startsWith(Resource_Code, "602038") ~ "Other Wild Greens", #would we include this in diversity estimates? may need. to also filter out other
    startsWith(Resource_Code, "602040") ~ "Unknown Mushrooms", ##would we include this in diversity estimates?
    startsWith(Resource_Code, "603002") ~ "Black Seaweed",
    startsWith(Resource_Code, "603004") ~ "Bull Kelp",
    startsWith(Resource_Code, "603006") ~ "Red Seaweed",
    startsWith(Resource_Code, "603008") ~ "Sea Ribbons",
    startsWith(Resource_Code, "603010") ~ "Giant Kelp",
    startsWith(Resource_Code, "603012") ~ "Alaria",
    startsWith(Resource_Code, "603090") ~ "Unknown Seaweed", ##made the seaweed/kelp for fertilizer just unknown seaweed, as don't know which seaweed species but is not a new species 
    startsWith(Resource_Code, "603099") ~ "Unknown Seaweed",
    startsWith(Resource_Code, "604011") ~ "Alder",
    startsWith(Resource_Code, "604099") ~ "Other Wood",
    startsWith(Resource_Code, "601007") ~ "Crowberry",
    startsWith(Resource_Code, "602010") ~ "Other Beach Greens",
    startsWith(Resource_Code, "602027") ~ "Dandelion Greens",
    startsWith(Resource_Code, "602037") ~ "Yarrow",
    startsWith(Resource_Code, "602041") ~ "Sorrel",
    startsWith(Resource_Code, "602042") ~ "Fireweed",
    startsWith(Resource_Code, "602043") ~ "Plantain",
    startsWith(Resource_Code, "603014") ~ "Red Laver (Dulse)",
    startsWith(Resource_Code, "603016") ~ "Bladder Wrack",
    startsWith(Resource_Code, "604005") ~ "Spruce Pitch",
    startsWith(Resource_Code, "601030") ~ "Blackberry",
    startsWith(Resource_Code, "601099") ~ "Other Wild Berry",
    startsWith(Resource_Code, "602025") ~ "Lambs Quarter",
    startsWith(Resource_Code, "604008") ~ "Spruce",
    startsWith(Resource_Code, "604010") ~ "Cottonwood",
    startsWith(Resource_Code, "601034") ~ "Serviceberry",
    startsWith(Resource_Code, "602008") ~ "Wild Sweet Potato",
    startsWith(Resource_Code, "604002") ~"Bark",
    startsWith(Resource_Code, "604004") ~ "Roots",
    startsWith(Resource_Code, "602044") ~ "Stinkweed",
    startsWith(Resource_Code, "602048") ~ "Unknown Greens from Land",
    startsWith(Resource_Code, "60204604") ~ "Chaga", ##one higher level is fungus, is chaga always fungus or is sometimes fingus reported and not chaga? - 
    startsWith(Resource_Code, "602052") ~ "Wild Chives",
    startsWith(Resource_Code, "604013") ~ "Willow",
    startsWith(Resource_Code, "6050") ~ "Coal",
    startsWith(Resource_Code, "6053") ~ "Unknown Kelp", ##Made kelp for fertilizer unknown kelp, as it is not a different species, have retained fertilizer in the use category
  )) %>%
  mutate(Harvest_Type = case_when(
    endsWith(Resource_Code, "2") ~ "Non Commercial",
    endsWith(Resource_Code, "1") ~ "Commercial",
  )) %>% 
  mutate(Habitat = case_when(
    grepl("Kelp", Taxa_lvl3) ~ "Nearshore",
    !grepl("Kelp", Taxa_lvl3) ~ "Terrestrial",
  )) %>%
mutate(Use = case_when(
  grepl("Fertilizer", Resource_Name) ~ "Fertilizer",
))


veg$Conversion_Units_To_Pounds <- as.character(veg$Conversion_Units_To_Pounds)
veg$Est_Comm_Population <- as.character(veg$Est_Comm_Population)

##select rows where the family level is not broken down further in a certain year/community
veg1 <- veg %>%
  filter(is.na(Harvest_Type)) %>%
  filter(!is.na(Taxa_lvl3)) %>%
  group_by(Site_Year_Code, Taxa_lvl3) %>%
  filter(!Taxa_lvl3 %in% Taxa_lvl3[!is.na(Taxa_lvl4)]) ##filters out familys where genus is not NA -- so i think these are only families where further genus level does not exist (i.e., family is the lowest level ID for that year/community)

##select rows where the genus level is not broken down further for a certain year/community
veg2 <- veg %>%
  filter(is.na(Harvest_Type)) %>%
  filter(!is.na(Taxa_lvl4)) %>%
  group_by(Site_Year_Code, Taxa_lvl3, Taxa_lvl4) %>%
  filter(!Taxa_lvl4 %in% Taxa_lvl4[!is.na(Taxa_lvl5)])

##select rows where species level is not broken down further for a certain year/community 
veg3 <- veg %>%
  filter(is.na(Harvest_Type)) %>%
  filter(!is.na(Taxa_lvl5))

veg4 <- rbind(veg1, veg2, veg3) %>%
  select(Project_Name, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest, Conversion_Units_To_Pounds, Resource_Harvest_Units, Est_Comm_Population) %>%
  mutate(Taxa_lvl4 = coalesce(Taxa_lvl4, Taxa_lvl3)) %>%
  mutate(Taxa_lvl5 = coalesce(Taxa_lvl5, Taxa_lvl4)) %>%
  distinct()

veg_final <- veg4 %>%
  group_by(Project_Name, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Conversion_Units_To_Pounds, Resource_Harvest_Units, Est_Comm_Population) %>%
  summarise(across(where(is.numeric), sum))

rm(veg1, veg2, veg3, veg4)
##Note: for now, have decided to go to the level of each species, and use the existing per capita harvets and total estimated lbs harvested to move forward with trying to establish food web approach.
##
#Join all dataframes together ----------
df_final <- rbind(fish_final, lm_final, mm_final, be_final, mi_final, veg_final) %>%
  filter(!if_all(Reported_Pounds_Harvested:Percent_Of_Total_Harvest, ~ .x == 0)) ##remove rows where all values are 0 (nothing was harvested)

setwd("~/Desktop/Wild Foods Repo/")
write.csv(df_final, "intermediate_files/harvest_data_clean.csv")

##for now keeping eggs and adults separate (same w/ roe in fish) as these have different trophic levels... can add later if want to, or remove egg/roe part of name for species richness.. need to think about how to do that still anyway
