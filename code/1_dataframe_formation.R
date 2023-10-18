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
 
##need to see if i should put in species name for family level sum, have only put now if family not further broken down

##want to write a loop function that will go through and check if the categories sum to each other.. each site/year

fish_sp_list <- fish %>%
  distinct(General_Category, General_Category_lvl2, Family, Species, Resource_Code, Resource_Name)


  
fish_test <- fish %>%
  group_by(Site_Year_Code) %>%
  count()
  
##have 65 separate food webs to construct :D 


##also want to check that all the values for the different categories add up 


##testing to see if can build loop function
angoon_1984 <- fish %>%
  filter(Site_Year_Code == "Angoon_1984") #%>%
 # select(Site_Year_Code, General_Category:Species, Fishing_Gear_Type, Resource_Code, Resource_Name, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Estimated_Amount_Harvested, Estimated_Total_Pounds_Harvested) %>%
  #select(Site_Year_Code, General_Category:Species, Fishing_Gear_Type, Resource_Code, Resource_Name, sort(names(.)))

##select species that are broken down into gear types -- this will be within species level
gt_sp <- angoon_1984 %>%
  filter(Fishing_Gear_Type != "NA") %>%
  filter(!is.na(Species)) %>%
  group_by(Family, Species) %>%
  summarise_at(vars(Percent_Using:Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
  rename_with(~paste0(., "_gt_sum"), Percent_Using:Mean_Grams_Percapita_Harvest)

ng_sp <- angoon_1984 %>%
  filter(Fishing_Gear_Type == "NA") %>%
  group_by(Family, Species) %>%
  summarise_at(vars(Percent_Using:Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
  rename_with(~paste0(., "_db_sum"), Percent_Using:Mean_Grams_Percapita_Harvest)

test <- inner_join(gt_sp, ng_sp, by = c("Family", "Species")) %>%
  select(Family, Species, sort(names(.)))



##comparing sums at family level -- only for families that are broken down into multiple species 
fam_db <- angoon_1984 %>%
    filter(Fishing_Gear_Type == "NA") %>%
    filter(is.na(Species)) %>%
    group_by(Family) %>%
    summarise_at(vars(Percent_Using:Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_db_sum"), Percent_Using:Mean_Grams_Percapita_Harvest)

fam_calc <- angoon_1984 %>%
  filter(Fishing_Gear_Type == "NA") %>%
  filter(Species != "NA") %>%
  group_by(Family) %>%
  summarise_at(vars(Percent_Using:Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
  rename_with(~paste0(., "_fam_sum"), Percent_Using:Mean_Grams_Percapita_Harvest)

 
test_2 <- inner_join(fam_db, fam_calc, by = "Family") %>%
  select(Family, sort(names(.)))

##put into function to apply to all sites/years
##function to test whether sum of gear types within species matches with sum value already given in database
sp_gt_sum_test_func <- function(x){
  gt_sp <- x %>%
    filter(Fishing_Gear_Type != "NA") %>%
    filter(!is.na(Species)) %>%
    group_by(Site_Year_Code, Family, Species) %>%
    summarise_at(vars(Percent_Using:Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_gt_sum"), Percent_Using:Mean_Grams_Percapita_Harvest)
  ng_sp <- x %>%
    filter(Fishing_Gear_Type == "NA") %>%
    group_by(Site_Year_Code, Family, Species) %>%
    summarise_at(vars(Percent_Using:Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_db_sum"), Percent_Using:Mean_Grams_Percapita_Harvest)
  gear_sum <- inner_join(gt_sp, ng_sp, by = c("Site_Year_Code", "Family", "Species")) %>%
    select(Site_Year_Code, Family, Species, sort(names(.)))
}

##function to test if species sum within family to the family level sum already given in the database
fam_sum_test_func <- function(x){
  fam_db <- x %>%
    filter(Fishing_Gear_Type == "NA") %>%
    filter(is.na(Species)) %>%
    group_by(Site_Year_Code, Family) %>%
    summarise_at(vars(Percent_Using:Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_db_sum"), Percent_Using:Mean_Grams_Percapita_Harvest)
  fam_calc <- x %>%
    filter(Fishing_Gear_Type == "NA") %>%
    filter(Species != "NA") %>%
    group_by(Site_Year_Code, Family) %>%
    summarise_at(vars(Percent_Using:Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_fam_sum"), Percent_Using:Mean_Grams_Percapita_Harvest)
  fam_sum <- inner_join(fam_db, fam_calc, by = c("Site_Year_Code", "Family")) %>%
    select(Site_Year_Code, Family, sort(names(.)))
}  

sp_gt_sum_test <- split(fish, paste0(fish$Site_Year_Code)) %>%
  map(sp_gt_sum_test_func) %>%
  bind_rows()

fam_sum_test <- split(fish, paste0(fish$Site_Year_Code)) %>%
  map(fam_sum_test_func) %>%
  bind_rows()




##this function removes the total family sum, and the gear specific rows, so takes the already calculated sum within each species, and if there are multiple of each speices (e.g., herring roe, it takes the sum of those)
test_func <- function(x){
  df_prep <- x
  family_total <- df_prep %>%
    filter(Fishing_Gear_Type == "NA") %>%
    filter(!is.na(Species)) %>%
    group_by(Species) %>%
    mutate_if(is.numeric, sum) %>%
    mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
    distinct()
}

ang_test <- test_func(angoon_1984)
##kind of worked but not really

fish_test_2 <- split(fish, paste0(fish$Site_Year_Code)) %>%
  map(test_func) %>%
  bind_rows()

##so i think this works, have 1 value for each species/year/site but the only thing it does not have is the type of gear
##also for some variables, summing does not work -- i.e., the %'s 

##need to figure out how to check/confirm that these sums are accurate and okay -- this is the hard part!
##for each category in general_category_level 2
###if Family is.na ---> mutaate - level 1
###if species is.na --> mutate level 2
###if fishing gear is.na --> level 3
###if fishing gear is not na --> level 4

angoon_1984$Family <- angoon_1984$Family %>% replace_na("NA")
angoon_1984$Species <- angoon_1984$Species %>% replace_na("NA")

ang_test_2 <- angoon_1984 %>%
  mutate(Nest_Level = case_when(
  grepl("NA", Family) ~ "Level_1",
  grepl("NA", Species) ~ "Level_2",
  grepl("NA", Fishing_Gear_Type) ~ "Level_3",
  grepl("CF", Fishing_Gear_Type) ~ "Level_4",
  grepl("Rod", Fishing_Gear_Type) ~ "Level_4",
  grepl("Other", Fishing_Gear_Type) ~ "Level_4",
  )) 

sp_sum_func <- function(x){
  df_prep <- x
  lvl3_sum <- df_prep %>%
    filter(Fishing_Gear_Type =="CF_Retention" | Fishing_Gear_Type == "Rod_Reel" | Fishing_Gear_Type == "Other_Gear") %>%
    group_by(Family) %>%
    mutate(mean_pph_sum = sum(Mean_Pounds_Per_Household)) ##will need to figure out how to run this for multiple columns, have done this before just forget 
  lvl4_sum <- df_prep %>%
    filter(Nest_Level == "Level_4") %>%
    group_by(Family) %>%
    mutate(mean_pph_sum = sum(Mean_Pounds_Per_Household))
  lvl2_sum <- df_prep %>%
  filter(Nest_Level == "Level_2") %>%
    group_by(Family) %>%
    mutate(mean_pph_sum = sum(Mean_Pounds_Per_Household))
  lvl_sums <- rbind(lvl2_sum, lvl3_sum, lvl4_sum)
}
##not doing what i want





 
  
ang_sum_test <- ang_test_2 %>%
  filter(Fishing_Gear_Type =="CF_Retention" | Fishing_Gear_Type == "Rod_Reel" | Fishing_Gear_Type == "Other_Gear") %>%
  group_by(Family, Species) %>%
  summarise_at(vars(Mean_Pounds_Per_Household:Estimated_Total_Pounds_Harvested), sum) 

ang_sum_test_2 <- ang_sum_test %>%
  filter(Species != "NA") %>%
  group_by(Family) %>%
  summarise_at(vars(Mean_Pounds_Per_Household:Estimated_Total_Pounds_Harvested), sum) %>%
  mutate(type = "gear_type_sp_breakdown")
  
ang_sum_test_3 <- 
  ang_sum_test %>%
  filter(Species == "NA") %>%
  group_by(Family) %>%
  summarise_at(vars(Mean_Pounds_Per_Household:Estimated_Total_Pounds_Harvested), sum) %>%
  mutate(type = "gear_type_fam_breakdown") 

ang_sum_test_4 <- rbind(ang_sum_test_2, ang_sum_test_3)


  mutate(mean_pph_sum = sum(Mean_Pounds_Per_Household))


ang_sum_test <- ang_test_2 %>%
  if(all(Fishing_Gear_Type =="CF_Retention" | Fishing_Gear_Type == "Rod_Reel" | Fishing_Gear_Type == "Other_Gear")){
    group_by(Species) %>%
      mutate(mean_pph_sum = sum(Mean_Pounds_Per_Household))
  }else{
    group_by(Species) %>%
      mutate(mean_pph_sum = Mean_Pounds_Per_Household)
  }

##create numeric code vector for fishing gear type
str(angoon_1984)

angoon_1984$Fishing_Gear_Type <- as.numeric(angoon_1984$Fishing_Gear_Type)

w <- c() ##creates empty vector named 'w'
test <- angoon_1984 %>%
  if(all(Fishing_Gear_Type >= 1)){
    group_by(Family) %>%
      group_by(Species) %>%
      mutate(mean_pph_sum = sum(Mean_Pounds_Per_Household))
  }else{
    group_by(Species) %>%
      mutate(mean_pph_sum = Mean_Pounds_Per_Household)
  }
  
  for(i in 1:length(Fishing_Gear_Type)){
    if (Fishing_Gear_Type[i] >= 1){
    w[i] <- group_by(Family) %>%
      sum(Mean_Pounds_Per_Household[i])
  }
  else 
    w[i] <- Mean_Pounds_Per_Household[i]
}

w <- as.data.frame(w)

sp_sum <- aggregate(Mean_Pounds_Per_Household~Family+Species+Nest_Level, data=ang_test_2, FUN=sum, na.rm = FALSE)

test_func_2 <- function(x){
  df_prep <- x
  
}


Family, Species, Fishing_Gear_Type,Resource_Code, Resource_Name,

angoon_1984_salmon_na <- fish %>%
  filter(Site_Year_Code == "Angoon_1984") %>%
  filter(General_Category_lvl2 == "Salmon") %>%
  filter(is.na(Species)) %>%
  filter(Fishing_Gear_Type == "NA")%>%
  select(Species, Percent_Using:Percapita_Pounds_Harvested, Number_Of_Resource_Harvested:Mean_Grams_Percapita_Harvest) %>%
  mutate(geartype = "NA")

angoon_1984_salmon_gt <- fish %>%
  filter(Site_Year_Code == "Angoon_1984") %>%
  filter(General_Category_lvl2 == "Salmon") %>%
  filter(Fishing_Gear_Type != "NA") %>%
  #group_by(Species) %>%
  summarise_at(vars(Percent_Using:Percapita_Pounds_Harvested, Number_Of_Resource_Harvested:Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
  mutate(geartype = "sum_all") %>%
  mutate(Species = "allspecies") %>%
  select(Species, Percent_Using:Mean_Grams_Percapita_Harvest, geartype)

salmon_test <- rbind(angoon_1984_salmon_gt, angoon_1984_salmon_na) %>%
  select(Species, geartype, Percent_Using:Mean_Grams_Percapita_Harvest)

angoon_1984_chinook_na <- angoon_1984 %>%
  filter(Species == "Chinook Salmon")  %>%
  filter(Fishing_Gear_Type == "NA") %>%
  select(Percent_Using:Percapita_Pounds_Harvested, Number_Of_Resource_Harvested:Mean_Grams_Percapita_Harvest) %>%
  mutate(geartype = "NA")
 
angoon_1984_chinook_gt <- angoon_1984 %>%
  filter(Species == "Chinook Salmon")  %>%
  filter(Fishing_Gear_Type != "NA") %>%
  summarise_at(vars(Percent_Using:Percapita_Pounds_Harvested, Number_Of_Resource_Harvested:Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
  mutate(geartype = "sum_all")


angoon_1984_chinook_test <- rbind(angoon_1984_chinook_na, angoon_1984_chinook_gt)
##so even just looking at 1 year, 1 season, these values do not always add up... need to figure out methods and extrapolations to see if gear type is even worth retaining.. and why are the values different? 


sum(angoong_1984_chinook[which(angoong_1984_chinook != "NA")], 10)  
  ifelse(Fishing_Gear_Type == "NA", )
  if(Fishing_Gear_Type == "NA"){
    
  }


%>%
  group_nest(General_Category, General_Category_lvl2, Family)

angoon_1984
##for each species, identified by first 4 to 7 digits of resource code, do they match sum of codes with gear type.. 
##determine if nested levels sum up to furthur up level 


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