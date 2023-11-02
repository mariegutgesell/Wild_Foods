##Species Categorizations
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
df_test <- df %>%
  filter(Site_Year_Code %in% survey_demographics$Site_Year_Code) %>% ##selects only years where a comprehensive survey was done
  filter(!grepl("Marine Mammals", Project_Name)) #%>% ##this removes data from targeted marine mammal surveys done in same year as comprehensive survey
#select(Project_Name, Site_Year_Code, Study_Year, Community_Name, Resource_Code, Resource_Name, Percent_Using, Percent_Attempting_to_Harvest, Percent_Harvesting, Percent_Receiving, Percent_Giving, Percent_Of_Total_Harvest, Estimated_Total_Pounds_Harvested, Percapita_Pounds_Harvested) #%>%##reducing to most likely columns of interest
#  filter(!str_detect(Resource_Name, "\\[.*?\\]")) ##Remove breakdown of each species into gear type -- can add this back in later if interested, but for now have removed

##or could you change orientation so gear type goes long? or make a separate dataframe that has gear type as column
##or since this is all nested, maybe go to lowest/most detailed level and then create summary ones.. need to think about most streamlined/efficient way of doing this grouping and so don't have duplicate data

##reorganize dataframe to make categories, levels, usable/no repeat rows of data

sp_all <- df_test %>%
  distinct(Resource_Code, Resource_Name)

##start with fish
###1) FISH ------------------
fish_code <- "1"
fish <- df_test %>% 
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

##add habitat in here... 

##need to see if i should put in species name for family level sum, have only put now if family not further broken down

##want to write a loop function that will go through and check if the categories sum to each other.. each site/year

fish_sp_list <- fish %>%
  distinct(General_Category, General_Category_lvl2, Family, Species, Resource_Code, Resource_Name)

####2) LAND MAMMALS -----------------
land_mammal_code <- "2"

lm <- df_test %>% 
  filter(str_detect(Resource_Code, '^2')) 

lm$Resource_Code  <- format(lm$Resource_Code, scientific = FALSE)
lm$Resource_Code <- as.character(lm$Resource_Code) 
str(lm)
sp_lm <- lm %>%
  distinct(Resource_Code, Resource_Name)

sp_lm <- sp_lm %>%
  mutate(Habitat = "Terrestrial") %>%
  mutate(Sex = case_when(
    grepl("Sex Unknown", Resource_Name) ~ "Unknown",
    grepl("Male", Resource_Name) ~ "Male",
    grepl("Female", Resource_Name) ~ "Female",
  )) %>%
  mutate(General_Category = "Land Mammals") %>%
  mutate(General_Category_lvl2 = case_when(
    startsWith(Resource_Code, "21") ~ "Large Land Mammals",
    startsWith(Resource_Code, "22") ~ "Small Land Mammals",
  )) %>%
  mutate(Family = case_when( ##since the mammals aren't broken down into families (no sums at that level, will just make family and species the same.. )
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
  mutate(Species = case_when( ##since the mammals aren't broken down into families (no sums at that level, will just make family and species the same.. )
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
    startsWith(Resource_Code, "221009") ~ "Unknown Hare",
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

##what are all of the NAs?? where have resource code but no resource name


###MARINE MAMMALS ------------------
marine_mammal_code <- "3"

mm <- df_test %>% 
  filter(str_detect(Resource_Code, '^3')) 

mm$Resource_Code  <- format(mm$Resource_Code, scientific = FALSE)
mm$Resource_Code <- as.character(mm$Resource_Code) 
str(mm)
sp_mm <- mm %>%
  distinct(Resource_Code, Resource_Name)

sp_mm <- sp_mm %>%
  mutate(Habitat = "Marine") %>%
  mutate(Sex = case_when(
    grepl("Unknown Sex", Resource_Name) ~ "Unknown",
    grepl("Male", Resource_Name) ~ "Male",
    grepl("Female", Resource_Name) ~ "Female",
  )) %>%
  mutate(General_Category = "Marine Mammals") %>%
  mutate(General_Category_lvl2 = case_when( ##don't have a category level 2, so leaving it as marine mammals.. 
    startsWith(Resource_Code, "30") ~ "Marine Mammals",
  )) %>%
mutate(Family = case_when(
  startsWith(Resource_Code, "3008") ~ "Seal", 
  startsWith(Resource_Code, "3010") ~ "Sea Otter",
  startsWith(Resource_Code, "3012") ~ "Stellar Sea Lion",
  startsWith(Resource_Code, "3016") ~ "Whale",
  startsWith(Resource_Code, "3099") ~ "Unknown Marine Mammals",
  startsWith(Resource_Code, "3014") ~ "Walrus",
  
)) %>%
  mutate(Species = case_when(
    startsWith(Resource_Code, "300804") ~ "Fur Seal", 
    startsWith(Resource_Code, "30080404") ~ "Fur Seal (other)", ##what is a fur seal other?? 
    startsWith(Resource_Code, "300806") ~ "Harbor Seal",
    startsWith(Resource_Code, "30080604") ~ "Harbour Seal (saltwater)", ##is this a different habitat? can it be captured that way? as not really different species
   startsWith(Resource_Code, "300899") ~ "Unknown Seal",
   startsWith(Resource_Code, "300888") ~ "Unknown Seal Oil",
     startsWith(Resource_Code, "301602") ~ "Belukha",
    startsWith(Resource_Code, "301606") ~ "Bowhead",
   startsWith(Resource_Code, "301699") ~ "Unknown Whale",
   startsWith(Resource_Code, "3014") ~ "Walrus",
    
  ))


##need to figure out how to properly differentiate between the ones w/ breakdowns and not... my brain is too tired to think rn but there has to be a way

###4) BIRDS AND EGGS

birds_eggs_code <- "4"

be <- df_test %>% 
  filter(str_detect(Resource_Code, '^4')) 

be$Resource_Code  <- format(be$Resource_Code, scientific = FALSE)
be$Resource_Code <- as.character(be$Resource_Code) 
str(be)
be_mm <- be %>%
  distinct(Resource_Code, Resource_Name)



