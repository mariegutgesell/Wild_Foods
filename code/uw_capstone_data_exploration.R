##UW Capstone course, creating dataframe, explore data

library(tidyverse)
library(ggplot2)
library(readxl)
library(data.table)


##read in harvest datafiles
setwd("~/Desktop/Wild Foods Repo/data/harvest_data/Tongass/")
file.list <- list.files(pattern='*.xls')
df.list <- sapply(file.list, read_excel, simplify = FALSE)
df <- rbindlist(df.list) %>%
  unite(Site_Year_Code, c(Community_Name, Study_Year), sep = "_", remove = FALSE) 

##read in comprehensive survey demographics
setwd("~/Desktop/Wild Foods Repo/data/")
survey_demographics <- read_excel("CSIS_SurveyData_Demographics.xlsx", sheet = 2) %>%
  unite(Site_Year_Code, c(Community, Year), sep = "_", remove = FALSE)


##Read in harvest data
t_df <- read.csv("intermediate_data/tongass_harvest_data_clean.csv") %>%
  separate(Site_Year_Code, into = c("Site", "Year"), sep = "_", remove = FALSE)

t_df_target <- df %>%
  filter(!Project_Name %in% t_df$Project_Name) 

sp_list <- t_df_target %>%
  select(Resource_Name, Resource_Code) %>%
  distinct()



#2) LAND MAMMALS ------------------
land_mammal_code <- "2"

lm <- t_df_target %>% 
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

    startsWith(Resource_Code, "2112") ~ "Deer",

  )) %>%
  mutate(Taxa_lvl4 = case_when( ##no "genus" level for land mammals, so keeping this the same as the family, but doing this so matches with other large categories as want to retain that info
    startsWith(Resource_Code, "2112") ~ "Deer",
  )) %>%
  mutate(Taxa_lvl5 = case_when( ##only small mammals broken down further
    startsWith(Resource_Code, "2112") ~ "Deer",
  )) 

#lm$Conversion_Units_To_Pounds <- as.character(lm$Conversion_Units_To_Pounds)
#lm$Est_Comm_Population <- as.character(lm$Est_Comm_Population)

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

lm_final <- rbind(lm1, lm2, lm3) %>%
  dplyr::select(Project_Name, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Resource_Code, Resource_Name, Percent_Using:Mean_Grams_Percapita_Harvest) %>%
  mutate(Taxa_lvl4 = coalesce(Taxa_lvl4, Taxa_lvl3)) %>%
  mutate(Taxa_lvl5 = coalesce(Taxa_lvl5, Taxa_lvl4))


#clean up environment
rm(lm1, lm2, lm3)


#3) MARINE MAMMALS ------------------
marine_mammal_code <- "3"

mm <- t_df_target %>% 
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
  dplyr::select(Project_Name, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Resource_Code, Resource_Name, Percent_Using:Mean_Grams_Percapita_Harvest) %>%
  mutate(Taxa_lvl4 = coalesce(Taxa_lvl4, Taxa_lvl3)) %>%
  mutate(Taxa_lvl5 = coalesce(Taxa_lvl5, Taxa_lvl4)) %>%
  filter(Taxa_lvl5 != "Harbour Seal (saltwater)") %>% ##removed harbor seal (saltwater) and fur seal (other) as always a replicate when it does come up
  filter(Taxa_lvl5 != "Fur Seal (other)")

rm(mm1, mm2, mm3)


#1) FISH ------------------
fish_code <- "1"
fish <- t_df_target %>% 
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
    startsWith(Resource_Code, "1202") ~ "Herring",
    startsWith(Resource_Code, "1203") ~ "Herring",
  )) %>%
  mutate(Taxa_lvl4 = case_when(
    startsWith(Resource_Code, "1202") ~ "Herring",
    startsWith(Resource_Code, "1203") ~ "Herring Roe",
     )) %>%
  mutate(Taxa_lvl5 = case_when(
    startsWith(Resource_Code, "1202") ~ "Herring",
    startsWith(Resource_Code, "1203") ~ "Herring Roe",
##note: this is broken down into different types and locations of herring spawn, herring sac roe, etc. do we need to go this specified? I don't think so
  )) %>%
  mutate(Habitat = case_when(
    startsWith(Taxa_lvl4, "Herring Roe") ~ "Nearshore",
    startsWith(Taxa_lvl2, "Herring") ~ "Marine",
  )) 
fish_str <- fish %>%
  distinct(Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Resource_Code, Resource_Name, Fishing_Gear_Type)
str(fish)
#change these to character so dont get added 
#fish$Conversion_Units_To_Pounds <- as.character(fish$Conversion_Units_To_Pounds)
#fish$Est_Comm_Population <- as.character(fish$Est_Comm_Population)
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




fish_final <- rbind(fish1, fish2, fish3)
  dplyr::select(Project_Name, Site_Year_Code, Habitat, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Resource_Code, Resource_Name, Percent_Using:Mean_Grams_Percapita_Harvest) %>%
  mutate(Taxa_lvl4 = coalesce(Taxa_lvl4, Taxa_lvl3)) %>%
  mutate(Taxa_lvl5 = coalesce(Taxa_lvl5, Taxa_lvl4))


##Note: I may go back and try and calculate these values myself from the beginning, as I am wary, but am moving forward this way for now. 

#clean up environment
rm(fish1, fish2, fish3)

##Joining all targeted surveys back together 
df_target_final <- rbind(fish_final, lm_final, mm_final)



df_target_final <- df_target_final %>%
  separate(Site_Year_Code, into = c("Site", "Year"), sep = "_", remove = FALSE) %>%
  select(Project_Name, Site_Year_Code, Site, Year, Habitat, Taxa_lvl1:Taxa_lvl5, Resource_Code, Resource_Name, Percent_Using:Estimated_Amount_Harvested, Mean_Grams_Per_Capita_Use:Mean_Grams_Percapita_Harvest)



write.csv(df_target_final, "UW_tongass_harvest_data_clean_targeted.csv")

##Looking at harvest distributions
total_harvest_rank <- t_df %>%
  group_by(Site) %>%
  mutate(harvest_rank = rank(-Percapita_Pounds_Harvested, ties.method = "min")) 

total_harvest_rank <- total_harvest_rank %>%
  group_by(Site) %>%
  arrange(harvest_rank) %>%
  mutate(harvest_rank_2 = row_number())


clrs2 = c("#FF9999","#003366","#CC9966", "#339933")

total_harvest_rank %>%
#  filter(Site == "Hoonah") %>% 
  ggplot(aes(x = harvest_rank_2, y= Percapita_Pounds_Harvested, fill = Habitat)) +
  geom_col() +
#  geom_text(aes(label = Taxa_lvl5),angle = 90, hjust = 1, vjust = 1, size = 3.5, family = "Times New Roman") +
  scale_fill_manual(values = clrs2)+
  theme_classic() +
  xlab("Harvest Rank") +
  ylab("Percapita Harvest (kg/person)") +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x =element_text(size = 14) , legend.position = "none", legend.title = element_text("Habitat"), text = element_text(family = "Times New Roman")) +
  facet_wrap(~Site, scale = "free")


##Looking at time series 
df_temp <- t_df %>%
  group_by(Site) %>%
  filter(n_distinct(Year) >= 3) %>%
  filter(Site_Year_Code != "Hoonah_2016") 

df_temp$Year <- as.numeric(df_temp$Year)

temporal_harvest_plot <- ggplot() +
  geom_line(data = df_temp, aes(x = Year, y = Percapita_Pounds_Harvested,  group = Taxa_lvl5)) +
  geom_point(data = df_temp, aes(x = Year, y = Percapita_Pounds_Harvested,  group = Taxa_lvl5)) +
  theme_classic() + 
  ylab("Percapita Harvest\n(kg/person)") +
  scale_x_continuous(breaks = seq(1983, 2015, by = 5))+
#  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none") +
#  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) 
  facet_wrap(~Site)
temporal_harvest_plot

