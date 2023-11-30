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
df_comp <- df %>%
  filter(Site_Year_Code %in% survey_demographics$Site_Year_Code) %>% ##selects only years where a comprehensive survey was done
  filter(!grepl("Marine Mammals", Project_Name)) #%>% ##this removes data from targeted marine mammal surveys done in same year as comprehensive survey
  #select(Project_Name, Site_Year_Code, Study_Year, Community_Name, Resource_Code, Resource_Name, Percent_Using, Percent_Attempting_to_Harvest, Percent_Harvesting, Percent_Receiving, Percent_Giving, Percent_Of_Total_Harvest, Estimated_Total_Pounds_Harvested, Percapita_Pounds_Harvested) #%>%##reducing to most likely columns of interest
#  filter(!str_detect(Resource_Name, "\\[.*?\\]")) ##Remove breakdown of each species into gear type -- can add this back in later if interested, but for now have removed
  
##Exploring Dataset 
##unique projects - all
proj <- df %>%
  group_by(Project_ID, Project_Name) %>%
  distinct(Project_ID, Project_Name)
##unique comprehensive projects
comp_proj <- df_test %>%
  group_by(Project_ID, Project_Name) %>%
  distinct(Project_ID, Project_Name)

##looking into surveys where both comprehensive and targeted years -- is the targeted data incorporated into the comprehensive data? 
ct_survey <- survey_demographics %>%
  filter(`Also_Target_Year?` == "Yes")
ct_test <- df %>%
  filter(Site_Year_Code %in% ct_survey$Site_Year_Code) %>%
  filter(str_detect(Resource_Code, '^3')) %>%
  filter(Site_Year_Code == "Angoon_1996") 
##no, separate surveys conducted. and values not identical, as survey protocols are different, and not all communities/years have the targeted marine survey, best to focus on only data from comprehensive survey

mushrooms <- df %>%
  filter(grepl("Mushroom", Resource_Name))
  

##or could you change orientation so gear type goes long? or make a separate dataframe that has gear type as column
##or since this is all nested, maybe go to lowest/most detailed level and then create summary ones.. need to think about most streamlined/efficient way of doing this grouping and so don't have duplicate data
  
##reorganize dataframe to make categories, levels, usable/no repeat rows of data

##start with fish
##start with fish
###1) FISH ------------------
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
    startsWith(Resource_Code, "120699") ~ "Unknown Bass",
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
    startsWith(Resource_Code, "12300600") ~ "Irish Lord",
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
    startsWith(Resource_Code, "1250069") ~ "Dolly Varden", ##what does this mean?
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
    startsWith(General_Category_lvl2, "Salmon") ~ "Freshwater_Anadromous",
    startsWith(Family, "Osmeridae") ~ "Freshwater_Anadromous",
    startsWith(Family, "Char") ~ "Freshwater_Anadromous",
    startsWith(Family, "Trout") ~ "Freshwater_Anadromous",
    startsWith(Family, "Whitefish") ~ "Freshwater_Anadromous",
    startsWith(Species, "Herring Roe") ~ "Nearshore",
    startsWith(General_Category_lvl2, "Non-Salmon") ~ "Marine",
  )) %>%
  mutate(Roe_Collection_Type = case_when(
    startsWith(Resource_Code, "120302") ~ "Unspecified",
    startsWith(Resource_Code, "120304") ~ "Sac Roe",
    startsWith(Resource_Code, "120306") ~ "Spawn on Kelp",
    startsWith(Resource_Code, "120308") ~ "Roe on Hair Seaweed",
    startsWith(Resource_Code, "120310") ~ "Roe in Hemlock Branches",
  )) %>%
  mutate(Species_lvl2 = case_when(
    startsWith(Resource_Code, "12300602") ~ "Red Irish Lord",
  ))


##need to see if i should put in species name for family level sum, have only put now if family not further broken down

##want to write a loop function that will go through and check if the categories sum to each other.. each site/year

fish_str <- fish %>%
  distinct(Habitat, General_Category, General_Category_lvl2, Family, Species, Roe_Collection_Type, Resource_Code, Resource_Name, Fishing_Gear_Type)

fish_sp_list <- fish %>%
  filter(!is.na(Species)) %>%
  filter(Fishing_Gear_Type == "NA") %>%
  distinct(Habitat, General_Category, General_Category_lvl2, Family, Species)
  
fish_test <- fish %>%
  group_by(Site_Year_Code) %>%
  count()
  
test2 <- fish %>%
  filter(Family == "Lamprey")
##have 65 separate food webs to construct :D 

##function to test whether sum of gear types within species matches with sum value already given in database
##this function generates sums across numeric variables for rows in data with fishing gear, and those without, only for fish broken down into gear type
sp_gt_sum_test_func <- function(x){
  gt_sp <- x %>%
    filter(Species != "Herring Roe") %>%
    filter(Fishing_Gear_Type != "NA") %>%
    filter(!is.na(Species)) %>%
    group_by(Site_Year_Code, Family, Species) %>%
    summarise_at(vars(Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_gt_sum"), Reported_Pounds_Harvested:Percent_Of_Total_Harvest)
  ##need to do herring roe separate, as is a unique case -- because has another level of separation (based on where harvested from), so the ng total is double because it includes each levels no gear sum, so need to remove those.. 
  gt_hr <- x %>%
    filter(Species == "Herring Roe") %>%
    filter(Fishing_Gear_Type != "NA") %>%
    filter(is.na(Roe_Collection_Type)) %>% 
    group_by(Site_Year_Code, Family, Species) %>%
    summarise_at(vars(Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_gt_sum"), Reported_Pounds_Harvested:Percent_Of_Total_Harvest)
  gt_sp2 <- rbind(gt_sp, gt_hr)
  ng_sp <- x %>%
    filter(Species != "Herring Roe") %>%
    filter(Fishing_Gear_Type == "NA") %>%
    group_by(Site_Year_Code, Family, Species) %>%
    summarise_at(vars(Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_db_sum"), Reported_Pounds_Harvested:Percent_Of_Total_Harvest)
    ng_hr <- x %>%
    filter(Species == "Herring Roe") %>%
    filter(Fishing_Gear_Type == "NA") %>%
    filter(is.na(Roe_Collection_Type)) %>% ##so this keeps only the overall sum of herring roe row, not each sub-type of where collected sum row, so this should remove duplication
    group_by(Site_Year_Code, Family, Species) %>%
    summarise_at(vars(Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_db_sum"), Reported_Pounds_Harvested:Percent_Of_Total_Harvest)
  ng_sp2 <- rbind(ng_sp, ng_hr)
  gear_sum <- inner_join(gt_sp2, ng_sp2, by = c("Site_Year_Code", "Family", "Species")) %>%
    select(Site_Year_Code, Family, Species, sort(names(.)))
}
##next step is to compare the sum already in the database, to the one calculated here by summing the gear type - have done below, need to think about % stuff before adding into function

##function to test if species sum within family to the family level sum already given in the database
fam_sum_test_func <- function(x){
  fam_db <- x %>%
    filter(Fishing_Gear_Type == "NA") %>%
    filter(is.na(Species)) %>%
     group_by(Site_Year_Code, Family) %>%
    summarise_at(vars(Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_db_sum"), Reported_Pounds_Harvested:Percent_Of_Total_Harvest)
  fam_calc <- x %>%
    filter(Fishing_Gear_Type == "NA") %>%
    filter(Species != "NA") %>%
    #filter(Species != "Irish Lord") %>%   ##for now am just removing this, becuase red irish lord is the only irish lord and its a genus in family sculpin, but only fish that has this additional categorization level...may be better way to deal with this in future, but for now don't worry about
    group_by(Site_Year_Code, Family) %>%
    summarise_at(vars(Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_fam_sum"), Reported_Pounds_Harvested:Percent_Of_Total_Harvest)
  fam_sum <- inner_join(fam_db, fam_calc, by = c("Site_Year_Code", "Family")) %>%
    select(Site_Year_Code, Family, sort(names(.)))
}  

##I think need to do irish lord separatly.. because it has that one extra level, so it gets messed up -- but then sculpin wouldn't add up ...ahhhh this is so dumb 

sp_gt_sum_test <- split(fish, paste0(fish$Site_Year_Code)) %>%
  map(sp_gt_sum_test_func) %>%
  bind_rows()

fam_sum_test <- split(fish, paste0(fish$Site_Year_Code)) %>%
  map(fam_sum_test_func) %>%
  bind_rows()


##testing calculating difference and then selecting those rows with a magnitude greater than 2 lbs 
##to-do next once working: expand to other variables (only that you would expect to sum!!) and then add this into the function so all of this happens at once
##variables we would expect to add up: Estimated_Total_Pounds_Harvested, Mean_Grams_Percapita_Harvest, Mean_Pounds_Per_Household, Number_of_Resource_Harvested, Percapita_Pounds_Harvested,  Reported_Pounds_Harvested, 
##variables we wouldn't expect to add up: the confidence intervals, percent using/giving etc. because individuals are not mutually exclusive.. e.g., 63% of pop could be sharing chinook, but only 30% share chum, since its the same pop asked about chinook/chum you wouldn't think that is then 93% of the population, would only add if they were different groups 
sp_gt_diff_test <- sp_gt_sum_test %>%
    group_by(Site_Year_Code, Family, Species) %>%
    mutate(est_total_lb_harv_diff = Estimated_Total_Pounds_Harvested_db_sum - Estimated_Total_Pounds_Harvested_gt_sum) %>%
    mutate(est_amount_harv_diff = Estimated_Amount_Harvested_db_sum - Estimated_Amount_Harvested_gt_sum) %>%
    mutate(mean_lb_perhousehold_diff = Mean_Pounds_Per_Household_db_sum - Mean_Pounds_Per_Household_gt_sum) %>%
    mutate(num_res_harv_diff = Number_Of_Resource_Harvested_db_sum - Number_Of_Resource_Harvested_gt_sum) %>%
    mutate(percap_lb_harv_diff = Percapita_Pounds_Harvested_db_sum - Percapita_Pounds_Harvested_gt_sum) %>%
    mutate(per_total_harv_diff = Percent_Of_Total_Harvest_db_sum - Percent_Of_Total_Harvest_gt_sum) %>%
    mutate(rep_lb_harv_diff = Reported_Pounds_Harvested_db_sum - Reported_Pounds_Harvested_gt_sum) %>%
    select(Site_Year_Code, Family, Species, est_total_lb_harv_diff, est_amount_harv_diff, mean_lb_perhousehold_diff, num_res_harv_diff, percap_lb_harv_diff, per_total_harv_diff,  rep_lb_harv_diff)

str(sp_gt_diff_test)
##determine where magnitude is > +/- 2
sp_gt_diff_2 <- sp_gt_diff_test %>%
  filter(if_any(est_total_lb_harv_diff:rep_lb_harv_diff, ~ .x > 2))

sp_gt_diff_3 <- sp_gt_diff_test %>%
  filter(if_any(est_total_lb_harv_diff:rep_lb_harv_diff, ~ .x < -2))
sp_gt_diff <- rbind(sp_gt_diff_2, sp_gt_diff_3) %>%##these are not going to be mutually exclusive... some rows could be >2 some less than >2
  distinct(Site_Year_Code, Family, Species, .keep_all = TRUE)

##okay, so when ignoring the percent data, or values derived from the % data, it is the same 73/1604 (4.5%) that have issues as only the est_total harvest, so same 3 reasons as listed below
##but the % stuff is kinda messed up, i need to understand more how those % values are input/what assumptions are made.. 



##trying to figure out why such a large magnitude of differences:
#setwd("~/Desktop/Wild Foods Repo/")
#write.csv(sp_gt_diff, "intermediate_files/gear_type_sum_large_differences.csv")  
#test <- fish %>%
#  filter(Site_Year_Code == "Hoonah_2012") %>%
#  filter(Species == "Halibut")

##Nov 15, 2023: went through each row w/ large difference and indicated reason why (see gear_type_sum_large_differences_notes excel sheet), reasons summarized below
##Reasons not adding up:
##1. Gear type breakdown mentioned/included in list, but no values reported, confusing because this is true for some of the variables, but not others -- why? for example have values for % harvesting/giving/recieving etc/ and not for the weights, this doesn't really make sense to me... 
##    - This is likely the case for any where the estimated sum across gear type is 0 (so 35/76)
##    - I think if this is the case, use the db summed row and not the gear breakdown -- can write this into filtering function I think.. 
##2. Looks like not all of what is collected is reported/broken down by gear type... so even if gear type is listed, then still not broken down fully, so likely gear type not known for all of harvest (because some will be filled, other 0)
##    - This I think would also make sense to use the pre-calculated one.. 
##3. Breakdown across all gears (or reported for some gears) but values do not add up... why? nature of how survey done? also some mention %'s but no reported harvest

##I think should talk to Lauren about what she recommends, could be nature of how data is collected/survey done that maybe that sum by species is more accurate?
##See if can find example of survey from 2012, or maybe earlier years.. have 2021 but survey may have changed


##family level
fam_diff_test <- fam_sum_test %>%
  group_by(Site_Year_Code, Family) %>%
  mutate(est_total_lb_harv_diff = Estimated_Total_Pounds_Harvested_db_sum - Estimated_Total_Pounds_Harvested_fam_sum) %>%
  mutate(est_amount_harv_diff = Estimated_Amount_Harvested_db_sum - Estimated_Amount_Harvested_fam_sum) %>%
  mutate(mean_lb_perhousehold_diff = Mean_Pounds_Per_Household_db_sum - Mean_Pounds_Per_Household_fam_sum) %>%
  mutate(num_res_harv_diff = Number_Of_Resource_Harvested_db_sum - Number_Of_Resource_Harvested_fam_sum) %>%
  mutate(percap_lb_harv_diff = Percapita_Pounds_Harvested_db_sum - Percapita_Pounds_Harvested_fam_sum) %>%
  mutate(per_total_harv_diff = Percent_Of_Total_Harvest_db_sum - Percent_Of_Total_Harvest_fam_sum) %>%
  mutate(rep_lb_harv_diff = Reported_Pounds_Harvested_db_sum - Reported_Pounds_Harvested_fam_sum) %>%
  select(Site_Year_Code, Family, est_total_lb_harv_diff, est_amount_harv_diff, mean_lb_perhousehold_diff, num_res_harv_diff, percap_lb_harv_diff, per_total_harv_diff,  rep_lb_harv_diff)


fam_diff_2 <- fam_diff_test %>%
  filter(if_any(est_total_lb_harv_diff:rep_lb_harv_diff, ~ .x > 2))

fam_diff_3 <- fam_diff_test %>%
  filter(if_any(est_total_lb_harv_diff:rep_lb_harv_diff, ~ .x < -2))
fam_diff <- rbind(fam_diff_2, fam_diff_3) %>%
  distinct(Site_Year_Code, Family, .keep_all = TRUE)

##82 of 494 families where calculated sum is greater or less than 2 of sum already in database

##trying to figure out why such a large magnitude of differences:
#setwd("~/Desktop/Wild Foods Repo/")
#write.csv(fam_diff, "intermediate_files/fish_family_sum_large_differences.csv")  
#test <- fish %>%
#  select(Site_Year_Code, Family, Species, Resource_Name, Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest ) %>%
#  filter(Site_Year_Code == "Yakutat_2015") %>%
#  filter(Family == "Char")

#test2 <- fish %>%
#  select(Site_Year_Code, Family, Species, Resource_Name, Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest ) %>%
#  filter(Site_Year_Code == "Angoon_2012")

##Nov 15, 2023 -- went through every row with large magnitudes and listed reason why (see fish_family_sum_large_differences_notes.xlsx), summary of reasons below
 
##Reasons for differences in magnitude:
##1. Number of resources harvested and/or amount harvested seem to have weight values (i.e., estimated total lb harvested and reported lb harvested)
##    - I think for these cases, take the species level data and working up from there is best (ideally gear type) - seems to be errors in summary data at family level already in db
##2. Small addition errors it looks like, for example family level sum in db is off by 1-3
##    - I think doing own sum, not using db level sum is best way to go for this, some small error but values are close enough that summing from the species level I think is more accurate, more likely errors occurred due to minor addition errors
##3. Sometimes, no clear way at all that family level sum in db comes from the species level values... not sure what to do about these ones. 
##    - Example: Klukwan_1983 Trout, Haines_1983 Trout
##4. Irish Lord problem -- one more level for just the irish lord genus within sculpin, leading to issues... how do I solve this? this can be done via coding somehow
##5. Klukwan_2014 dolly varden -- kind of a mess, need to figure out but data looks messed up  - Yakutat 2015 and Klukwan 2014 are the only ones with this dolly varden unknown species -- see notes in excel sheet
##    - For both of these places, the dolly varden-unknown looks like the right data (at least for these 7 variables..), these are the only two sites with this dv unknown, so my feeling is select the dv unknown for these two sites.. 

##FOR TOMORROW:
##Figure out how to resolve dolly varden and irish lord issue
##Need to make final decisions (or tentative final decisions) of what I think is most logical for how to resolve issues
##Is there a way to add the difference testing within the sum function? 
##Put together that final, clean datasheet for fish -- which will be the fish part of the larger df moving forward
##Start next group, large land mammals

##I think, in some ways, losing the gear type and using the species sum is the most accurate, and also easier way to make comparable to other large groups, where things like gear type don't exist ... i dont know maybe not as want to retain that info but what do you do about that 4.5% of data where it wouldn't add up? 

##sorting out irish lord issue
##for every community with irish lord data, is it always to level of red irish lord? no.....AHHHHH
il <- fish %>%
  filter(Species == "Irish Lord" | Species == "Red Irish Lord")

##communities w/ irish lord but no red irish lord breakdown: angoon 1984, haines 1983, klawock 1984 klukwan 1983, tenakee springs 1984, yakutat 1984
##so essentially for the irish lord, any community sampled before 1985 did not have breakdown into red irish lord


##conversion factors for fish
fish_cf <- fish %>%
  select(Project_Name, Family, Species, Roe_Collection_Type, Fishing_Gear_Type, Resource_Harvest_Units, Conversion_Units_To_Pounds) %>%
  distinct(Project_Name, Species, Resource_Harvest_Units, Conversion_Units_To_Pounds, .keep_all = TRUE)

test <- fish %>%
  select(Habitat, Site_Year_Code, General_Category, General_Category_lvl2, Family, Species, Roe_Collection_Type, Fishing_Gear_Type, Resource_Harvest_Units, Conversion_Units_To_Pounds, Number_Of_Resource_Harvested, Reported_Pounds_Harvested) %>%
  filter(Site_Year_Code == "Klukwan_2014")

##how many species do we not have a # of reported fish?
##but there are also a bunch with inaccurate reported number..no this is for the family sum, so if working from the species level this should be fine

  
#2) Land Mammals ------------------
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
##what are all of the NAs?? where have resource code but no resource name
##is last digit 9 always sex unknown? why for the small land mammals do they have that line and the total line ugh like what is going on here

##Lets look into the small land mammals
slm <- lm %>%
  filter(General_Category_lvl2 == "Small Land Mammals") %>%
  select(Habitat, General_Category, General_Category_lvl2, Family, Species,  Resource_Code, Resource_Name, Sex, Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Conversion_Units_To_Pounds, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest )


lm_str <- lm %>%
  distinct(Habitat, General_Category, General_Category_lvl2, Family, Species,  Resource_Code, Resource_Name, Sex)

lm_sp_list <- lm %>%
  filter(!is.na(Species)) %>%
  filter(is.na(Sex)) %>%
  distinct(Habitat, General_Category, General_Category_lvl2, Family, Species)

##small land mammals are broken down into species (sometimes) and then have unknown sex and NA (so broken down into sex unknown, seems fucking pointless but why not complicate it more)

##testing if land mammal values add up
lm_sp_sex_sum_test_func <- function(x){
  sex_sp <- x %>%
    filter(!is.na(Sex)) %>%
    filter(!is.na(Species)) %>%
    group_by(Site_Year_Code, Family, Species) %>%
    summarise_at(vars(Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_sex_sum"), Reported_Pounds_Harvested:Percent_Of_Total_Harvest)
  db_sex_sp <- x %>%
    filter(is.na(Sex)) %>%
    group_by(Site_Year_Code, Family, Species) %>%
    summarise_at(vars(Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_db_sum"), Reported_Pounds_Harvested:Percent_Of_Total_Harvest)
  sex_sum <- inner_join(sex_sp, db_sex_sp, by = c("Site_Year_Code", "Family", "Species")) %>%
    select(Site_Year_Code, Family, Species, sort(names(.)))
}

lm_sp_sex_sum_test <- split(lm, paste0(lm$Site_Year_Code)) %>%
  map(lm_sp_sex_sum_test_func) %>%
  bind_rows()


sp_sex_diff_test <- lm_sp_sex_sum_test %>%
  group_by(Site_Year_Code, Family, Species) %>%
  mutate(est_total_lb_harv_diff = Estimated_Total_Pounds_Harvested_db_sum - Estimated_Total_Pounds_Harvested_sex_sum) %>%
  mutate(est_amount_harv_diff = Estimated_Amount_Harvested_db_sum - Estimated_Amount_Harvested_sex_sum) %>%
  mutate(mean_lb_perhousehold_diff = Mean_Pounds_Per_Household_db_sum - Mean_Pounds_Per_Household_sex_sum) %>%
  mutate(num_res_harv_diff = Number_Of_Resource_Harvested_db_sum - Number_Of_Resource_Harvested_sex_sum) %>%
  mutate(percap_lb_harv_diff = Percapita_Pounds_Harvested_db_sum - Percapita_Pounds_Harvested_sex_sum) %>%
  mutate(per_total_harv_diff = Percent_Of_Total_Harvest_db_sum - Percent_Of_Total_Harvest_sex_sum) %>%
  mutate(rep_lb_harv_diff = Reported_Pounds_Harvested_db_sum - Reported_Pounds_Harvested_sex_sum) %>%
  select(Site_Year_Code, Family, Species, est_total_lb_harv_diff, est_amount_harv_diff, mean_lb_perhousehold_diff, num_res_harv_diff, percap_lb_harv_diff, per_total_harv_diff,  rep_lb_harv_diff)

str(sp_sex_diff_test)
##determine where magnitude is > +/- 2
sp_sex_diff_2 <- sp_sex_diff_test %>%
  filter(if_any(est_total_lb_harv_diff:rep_lb_harv_diff, ~ .x > 2))

sp_sex_diff_3 <- sp_sex_diff_test %>%
  filter(if_any(est_total_lb_harv_diff:rep_lb_harv_diff, ~ .x < -2))
sp_sex_diff <- rbind(sp_sex_diff_2, sp_sex_diff_3) %>%##these are not going to be mutually exclusive... some rows could be >2 some less than >2
  distinct(Site_Year_Code, Family, Species, .keep_all = TRUE)

##all of the sex breakdowns seem to add up 


##Note: sometimes conversion values are 0, but # of species returned is not 0, so estimated weight is 0... what is going on here? 
conv_0 <- lm %>%
  filter(Conversion_Units_To_Pounds == "0") %>%
  select(Site_Year_Code,Habitat, General_Category, General_Category_lvl2, Family, Species, Sex, Conversion_Units_To_Pounds, Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest) %>%
  filter(Number_Of_Resource_Harvested != "0")
##145/794 small mammal data rows .. 18.3%.. that is a lot.. 

test <- lm %>%
  select(Site_Year_Code,Habitat, General_Category, General_Category_lvl2, Family, Species, Sex, Conversion_Units_To_Pounds, Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest) %>%
  filter(Site_Year_Code == "Angoon_2012") %>%
  filter(Species == "Land Otter")
##can i do conversions myself?

lm_cf <- lm %>%
 select(Project_Name, Family, Species, Sex,  Resource_Harvest_Units, Conversion_Units_To_Pounds) %>%
  distinct(Project_Name, Family, Species, Resource_Harvest_Units, Conversion_Units_To_Pounds)

##sometimes have multiple conversion factors per species, for example the bears, this honestly looks like mistakes rather than intentional.. 

##do the species within families add up? only break down is in small land mammals
lm_fam_sum_test_func <- function(x){
  fam_db <- x %>%
    filter(is.na(Sex)) %>%
    filter(is.na(Species)) %>%
    group_by(Site_Year_Code, Family) %>%
    summarise_at(vars(Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_db_sum"), Reported_Pounds_Harvested:Percent_Of_Total_Harvest)
  fam_calc <- x %>%
    filter(is.na(Sex)) %>%
    filter(Species != "NA") %>%
       group_by(Site_Year_Code, Family) %>%
    summarise_at(vars(Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_fam_sum"), Reported_Pounds_Harvested:Percent_Of_Total_Harvest)
  fam_sum <- inner_join(fam_db, fam_calc, by = c("Site_Year_Code", "Family")) %>%
    select(Site_Year_Code, Family, sort(names(.)))
} 

lm_fam_sum_test <- split(lm, paste0(lm$Site_Year_Code)) %>%
  map(lm_fam_sum_test_func) %>%
  bind_rows()
##only fox, hare and squirrel are broken down into species (regardless of sex, other small land mammals are broken down into sex but not species)

##so it looks like, if this is working properly, that at least the summed species level adds up, i don't think worth keeping the breakdown to sex... 

##NEXT:
##Need to decide what to do with the conversion factors, the multiple conversion factors which may or may not be accurate? not sure.. 
lm_data <- lm %>%
  select(Site_Year_Code,Habitat, General_Category, General_Category_lvl2, Family, Species, Sex, Conversion_Units_To_Pounds, Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest) %>%
  filter(!is.na(Species)) %>%
  filter(is.na(Sex))

##the NAs are still weirding me out..not sure if i like this approach
lm_na <- lm_data %>%
  filter(is.na(Number_Of_Resource_Harvested)) %>%
  filter(Estimated_Total_Pounds_Harvested != "0")
##for 107 of 987 entries, there is an NA for # of resources harvested and reported lbs harvested, and 57 have an estimated total lbs harvested, how? 
##for example, angoon 1987, have no reported lbs or # of deer harvest, but have somehow a total estimate of lbs and number harvested? how? 
test <- lm %>%
  select(Site_Year_Code,Habitat, General_Category, General_Category_lvl2, Family, Species, Sex, Conversion_Units_To_Pounds, Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest) %>%
  filter(Site_Year_Code == "Angoon_1987")

###MARINE MAMMALS ------------------
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
    startsWith(Resource_Code, "3010") ~ "Sea Otter",
    startsWith(Resource_Code, "3012") ~ "Stellar Sea Lion",
    startsWith(Resource_Code, "3099") ~ "Unknown Marine Mammals",
  ))

mm_str <- mm %>%
  distinct(Habitat, General_Category, General_Category_lvl2, Family, Species,  Resource_Code, Resource_Name, Sex)

mm_sp_list <- mm %>%
  filter(!is.na(Species)) %>%
  filter(is.na(Sex)) %>%
  distinct(Habitat, General_Category, General_Category_lvl2, Family, Species)


##fur seal and harbor seal are duplicates. -- do both of them always come up? 
harbour_seal <- mm %>%
  filter(Species == "Harbor Seal") %>%
  filter(is.na(Sex))
##harbor seal saltwater is always a replicate, of harbor seal, so just remove.. 
fur_seal <- mm %>%
  filter(Species == "Fur Seal") %>%
  filter(is.na(Sex))
##fur seal (other) is always a replicate, of fur seal, so just remove.. 


###4) BIRDS AND EGGS -----------------

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
  mutate(General_Category = "Birds and Eggs") %>%
  mutate(General_Category_lvl2 = case_when(
    startsWith(Resource_Code, "41") ~ "Migratory Birds",
    startsWith(Resource_Code, "42") ~ "Other Birds", 
    startsWith(Resource_Code, "43") ~ "Bird Eggs",
  )) %>%
  mutate(Family = case_when(
    startsWith(Resource_Code, "4102") ~ "Ducks",
    startsWith(Resource_Code, "4104") ~ "Geese",
    startsWith(Resource_Code, "4106") ~ "Swan",
    startsWith(Resource_Code, "4108") ~ "Crane",
    startsWith(Resource_Code, "4110") ~ "Shorebird",
    startsWith(Resource_Code, "4112") ~ "Seabirds and Loons",
    startsWith(Resource_Code, "4114") ~ "Heron",
    startsWith(Resource_Code, "4218") ~ "Upland Game Birds",
    startsWith(Resource_Code, "4288") ~ "Unknown Other Birds", ##is this. the right place to put this? 
    startsWith(Resource_Code, "4302") ~ "Ducks",
    startsWith(Resource_Code, "4304") ~ "Geese",
    startsWith(Resource_Code, "4306") ~ "Swan",
    startsWith(Resource_Code, "4308") ~ "Crane",
    startsWith(Resource_Code, "4310") ~ "Shorebird",
    startsWith(Resource_Code, "4312") ~ "Seabirds and Loons",
    startsWith(Resource_Code, "4318") ~ "Upland Game Birds",
    startsWith(Resource_Code, "4399") ~ "Unknown Eggs",
  )) %>% 
  mutate(Genus = case_when(
    startsWith(Resource_Code, "410202") ~ "Bufflehead",
    startsWith(Resource_Code, "410204") ~ "Canvasback",
    startsWith(Resource_Code, "410208") ~ "Gadwall",
    startsWith(Resource_Code, "410210") ~ "Goldeneye", ##need to check, is goldeneye always broken down? or are there times when only goldeneye is recorded and not further broken down? 
    startsWith(Resource_Code, "410212") ~ "Harlequin",
    startsWith(Resource_Code, "410214") ~ "Mallard",
    startsWith(Resource_Code, "410216") ~ "Merganser", ##merganser always broken down?
    startsWith(Resource_Code, "410218") ~ "Long-tailed Duck (Oldsquaw)",
    startsWith(Resource_Code, "410220") ~ "Northern Pintail",
    startsWith(Resource_Code, "410222") ~ "Redhead Duck",
    startsWith(Resource_Code, "410226") ~ "Scaup", ##scaup always broken down?
    startsWith(Resource_Code, "410228") ~ "Scoter", ##scoter always broken down?
    startsWith(Resource_Code, "410232") ~ "Teal", ##teal always broken down?
    startsWith(Resource_Code, "410236") ~ "Wigeon", ##wigeon always broken down?
    startsWith(Resource_Code, "410299") ~ "Unknown Ducks",
    startsWith(Resource_Code, "410402") ~ "Brant",
    startsWith(Resource_Code, "410404") ~ "Canada Geese", ##canada geese always broken down?
    startsWith(Resource_Code, "410406") ~ "Emperor Geese",
    startsWith(Resource_Code, "410408") ~ "Snow Geese",
    startsWith(Resource_Code, "410410") ~ "White-fronted Geese",
    startsWith(Resource_Code, "410499") ~ "Unknown Geese",
    startsWith(Resource_Code, "410604") ~ "Tundra Swan (whistling)",
    startsWith(Resource_Code, "410699") ~ "Unknown Swan",
    startsWith(Resource_Code, "410802") ~ "Sandhill Crane",
    startsWith(Resource_Code, "411002") ~ "Wilson's Snipe",
    startsWith(Resource_Code, "411004") ~ "Black Oystercatcher",
    startsWith(Resource_Code, "411099") ~ "Unknown Shorebirds", ##do they always break down unknown into large/small?
    startsWith(Resource_Code, "411204") ~ "Cormorants",
    startsWith(Resource_Code, "411208") ~ "Grebe", ##grebe always broken down?
    startsWith(Resource_Code, "411210") ~ "Guillemot",
    startsWith(Resource_Code, "411212") ~ "Gull",  ##different species than the glaucous winged gull?
    startsWith(Resource_Code, "411216") ~ "Loon",
    startsWith(Resource_Code, "411222") ~ "Puffin",
    startsWith(Resource_Code, "411226") ~ "Tern",
    startsWith(Resource_Code, "411299") ~ "Unknown Seabirds",
    startsWith(Resource_Code, "4114") ~ "Great Blue Heron",
    startsWith(Resource_Code, "421802") ~ "Grouse", ##grouse always broken down?
    startsWith(Resource_Code, "421804") ~ "Ptarmigan", ##ptarmigan always broken down?
    startsWith(Resource_Code, "421899") ~ "Unknown Upland Game Birds",
    startsWith(Resource_Code, "430214") ~ "Mallard", 
    startsWith(Resource_Code, "430299") ~ "Unknown Duck",
    startsWith(Resource_Code, "430404") ~ "Canada Geese",
    startsWith(Resource_Code, "430499") ~ "Unknown Geese",
    startsWith(Resource_Code, "43060") ~ "Swan",
    startsWith(Resource_Code, "43069") ~ "Unknown Swan", ##kind of confused by the eggs, need to think about how to organize and will i separate them in a food web? 
    startsWith(Resource_Code, "430802") ~ "Sandhill Crane",
    startsWith(Resource_Code, "430899") ~ "Unknown Crane",
    startsWith(Resource_Code, "431004") ~ "Black Oystercatcher",
    startsWith(Resource_Code, "43109901" ) ~ "Unknown Small Shorebird",
    startsWith(Resource_Code, "43109902") ~ "Unknown Large Shorebird",
    startsWith(Resource_Code, "431210") ~ "Guillemot",
    startsWith(Resource_Code, "43121204") ~ "Glaucous Winged Gull", ##
    startsWith(Resource_Code, "43121299") ~ "Unknown Gull",
    startsWith(Resource_Code, "4312160") ~ "Loon",
    startsWith(Resource_Code, "4312169") ~ "Unknown Loon",
    startsWith(Resource_Code, "43121802") ~ "Common Murre", ##same value as the murre eggs? is the only breakdown for the murre... 
    startsWith(Resource_Code, "4312260") ~ "Tern",
    startsWith(Resource_Code, "4312269") ~ "Unknown Tern",
    startsWith(Resource_Code, "43129") ~ "Unknown Seabird",
    startsWith(Resource_Code, "431802") ~ "Grouse",
    startsWith(Resource_Code, "4318029") ~ "Unknown Grouse",
    startsWith(Resource_Code, "4318040") ~ "Ptarmigan",
    startsWith(Resource_Code, "4318049") ~ "Unknown Ptarmigan",
  )) %>% ##should i add an intermediate level? maybe..as there seem to be fair amount that have another level of categorization... 
  mutate(Species = case_when(
    startsWith(Resource_Code, "410202") ~ "Bufflehead",
    startsWith(Resource_Code, "410204") ~ "Canvasback",
    startsWith(Resource_Code, "410208") ~ "Gadwall",
    startsWith(Resource_Code, "41021002") ~ "Barrows Goldeneye", ##need to check, is goldeneye always broken down? or are there times when only goldeneye is recorded and not further broken down? 
    startsWith(Resource_Code, "41021004") ~ "Common Goldeneye",
    startsWith(Resource_Code, "4102109") ~ "Unknown Goldeneye",
    startsWith(Resource_Code, "410212") ~ "Harlequin",
    startsWith(Resource_Code, "410214") ~ "Mallard",
    startsWith(Resource_Code, "41021602") ~ "Common Merganser", ##merganser always broken down?
    startsWith(Resource_Code, "41021604") ~ "Red-Breasted Merganser",
    startsWith(Resource_Code, "41021699") ~ "Unknown Merganser",
    startsWith(Resource_Code, "410218") ~ "Long-tailed Duck (Oldsquaw)",
    startsWith(Resource_Code, "410220") ~ "Northern Pintail",
    startsWith(Resource_Code, "410222") ~ "Redhead Duck",
    startsWith(Resource_Code, "41022602") ~ "Greater Scaup", ##scaup always broken down?
    startsWith(Resource_Code, "41022604") ~"Lesser Scaup",
    startsWith(Resource_Code, "41022699") ~ "Unknown Scaup",
    startsWith(Resource_Code, "41022804") ~ "Surf Scoter", ##scoter always broken down?
    startsWith(Resource_Code, "41022806") ~ "White-winged Scoter",
    startsWith(Resource_Code, "41022899") ~ "Unknown Scoter",
    startsWith(Resource_Code, "41023206") ~ "Green-Winged Teal", ##teal always broken down?
    startsWith(Resource_Code, "41023299") ~ "Unknown Teal",
    startsWith(Resource_Code, "41023602") ~ "American Wigeon", ##wigeon always broken down?
    startsWith(Resource_Code, "41023699") ~ "Unknown Wigeon",
    startsWith(Resource_Code, "410299") ~ "Unknown Ducks",
    startsWith(Resource_Code, "410402") ~ "Brant",
    startsWith(Resource_Code, "41040406") ~ "Dusky Canada Geese", ##canada geese always broken down?
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
    startsWith(Resource_Code, "41109901") ~ "Unknown Small Shorebirds", ##do they always break down unknown into large/small?
    startsWith(Resource_Code, "41109902") ~ "Unknown Large Shorebirds",
    startsWith(Resource_Code, "411204") ~ "Cormorants",
    startsWith(Resource_Code, "41120802") ~ "Horned Grebe", ##grebe always broken down?
    startsWith(Resource_Code, "41120804") ~ "Red Necked Grebe",
    startsWith(Resource_Code, "41120899") ~ "Unknown Grebe",
    startsWith(Resource_Code, "411210") ~ "Guillemot",
    startsWith(Resource_Code, "411212") ~ "Gull",  ##different species than the glaucous winged gull?
    startsWith(Resource_Code, "4112160") ~ "Loon",
    startsWith(Resource_Code, "4112169") ~ "Unknown Loon",
    startsWith(Resource_Code, "4112220") ~ "Puffin",
    startsWith(Resource_Code, "4112229") ~ "Unknown Puffin",
    startsWith(Resource_Code, "411226") ~ "Tern",
    startsWith(Resource_Code, "411299") ~ "Unknown Seabirds",
    startsWith(Resource_Code, "4114") ~ "Great Blue Heron",
    startsWith(Resource_Code, "42180202") ~ "Spruce Grouse", ##grouse always broken down?
    startsWith(Resource_Code, "42180206") ~ "Ruffed Grouse",
    startsWith(Resource_Code, "4218029") ~ "Unknown Grouse",
    startsWith(Resource_Code, "42180402") ~ "Rock Ptarmigan", ##ptarmigan always broken down?
    startsWith(Resource_Code, "42180404") ~ "Willow Ptarmigan",
    startsWith(Resource_Code, "4218049") ~ "Unknown Ptarmigan",
    startsWith(Resource_Code, "421899") ~ "Unknown Upland Game Birds",
    startsWith(Resource_Code, "430214") ~ "Mallard", 
    startsWith(Resource_Code, "430299") ~ "Unknown Duck",
    startsWith(Resource_Code, "43040408") ~ "Lesser Canada Geese",
    startsWith(Resource_Code, "4304049") ~ "Unknown Canada Geese",
    startsWith(Resource_Code, "430499") ~ "Unknown Geese",
    startsWith(Resource_Code, "43060") ~ "Swan",
    startsWith(Resource_Code, "43069") ~ "Unknown Swan",
    startsWith(Resource_Code, "430802") ~ "Sandhill Crane",
    startsWith(Resource_Code, "430899") ~ "Unknown Crane",
    startsWith(Resource_Code, "431004") ~ "Black Oystercatcher",
    startsWith(Resource_Code, "43109901" ) ~ "Unknown Small Shorebird",
    startsWith(Resource_Code, "43109902") ~ "Unknown Large Shorebird",
    startsWith(Resource_Code, "431210") ~ "Guillemot",
    startsWith(Resource_Code, "43121204") ~ "Glaucous Winged Gull", ##
    startsWith(Resource_Code, "43121299") ~ "Unknown Gull",
    startsWith(Resource_Code, "4312160") ~ "Loon",
    startsWith(Resource_Code, "4312169") ~ "Unknown Loon",
    startsWith(Resource_Code, "43121802") ~ "Common Murre", ##same value as the murre eggs? is the only breakdown for the murre... 
    startsWith(Resource_Code, "4312260") ~ "Tern",
    startsWith(Resource_Code, "4312269") ~ "Unknown Tern",
    startsWith(Resource_Code, "43129") ~ "Unknown Seabird",
    startsWith(Resource_Code, "431802") ~ "Grouse",
    startsWith(Resource_Code, "4318029") ~ "Unknown Grouse",
    startsWith(Resource_Code, "4318040") ~ "Ptarmigan",
    startsWith(Resource_Code, "4318049") ~ "Unknown Ptarmigan",
  )) %>%
  mutate(Type = case_when(
    startsWith(Resource_Code, "41") ~ "Adult",
    startsWith(Resource_Code, "42") ~ "Adult", 
    startsWith(Resource_Code, "43") ~ "Egg",
  ))

be_str <- be %>%
  distinct(Habitat, General_Category, General_Category_lvl2, Family, Genus, Species,  Resource_Code, Resource_Name, Season, Type)

be_sp_list <- be %>%
  filter(!is.na(Species)) %>%
  filter(is.na(Season)) %>%
  distinct(Habitat, General_Category, General_Category_lvl2, Family, Genus, Species)

##do the species within families add up? 
be_fam_sum_test_func <- function(x){
  fam_db <- x %>%
    filter(is.na(Season)) %>%
    filter(is.na(Species)) %>%
    filter(is.na(Genus)) %>%
    group_by(Site_Year_Code, Family, Type) %>%
    summarise_at(vars(Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_db_sum"), Reported_Pounds_Harvested:Percent_Of_Total_Harvest)
  fam_calc <- x %>%
    filter(is.na(Season)) %>%
    filter(Species != "NA") %>%
    group_by(Site_Year_Code, Family, Type) %>%
    summarise_at(vars(Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_fam_sum"), Reported_Pounds_Harvested:Percent_Of_Total_Harvest)
  fam_sum <- inner_join(fam_db, fam_calc, by = c("Site_Year_Code", "Family", "Type")) %>%
    select(Site_Year_Code, Family, Type, sort(names(.)))
} 

be_fam_sum_test <- split(be, paste0(be$Site_Year_Code)) %>%
  map(be_fam_sum_test_func) %>%
  bind_rows()

be_fam_diff_test <- be_fam_sum_test %>%
  group_by(Site_Year_Code, Family, Type) %>%
  mutate(est_total_lb_harv_diff = Estimated_Total_Pounds_Harvested_db_sum - Estimated_Total_Pounds_Harvested_fam_sum) %>%
  mutate(est_amount_harv_diff = Estimated_Amount_Harvested_db_sum - Estimated_Amount_Harvested_fam_sum) %>%
  mutate(mean_lb_perhousehold_diff = Mean_Pounds_Per_Household_db_sum - Mean_Pounds_Per_Household_fam_sum) %>%
  mutate(num_res_harv_diff = Number_Of_Resource_Harvested_db_sum - Number_Of_Resource_Harvested_fam_sum) %>%
  mutate(percap_lb_harv_diff = Percapita_Pounds_Harvested_db_sum - Percapita_Pounds_Harvested_fam_sum) %>%
  mutate(per_total_harv_diff = Percent_Of_Total_Harvest_db_sum - Percent_Of_Total_Harvest_fam_sum) %>%
  mutate(rep_lb_harv_diff = Reported_Pounds_Harvested_db_sum - Reported_Pounds_Harvested_fam_sum) %>%
  select(Site_Year_Code, Family, Type, est_total_lb_harv_diff, est_amount_harv_diff, mean_lb_perhousehold_diff, num_res_harv_diff, percap_lb_harv_diff, per_total_harv_diff,  rep_lb_harv_diff)

str(be_fam_diff_test)
##determine where magnitude is > +/- 2
be_fam_diff_2 <- be_fam_diff_test %>%
  filter(if_any(est_total_lb_harv_diff:rep_lb_harv_diff, ~ .x > 2))

be_fam_diff_3 <- be_fam_diff_test %>%
  filter(if_any(est_total_lb_harv_diff:rep_lb_harv_diff, ~ .x < -2))
be_fam_diff <- rbind(be_fam_diff_2, be_fam_diff_3) %>%##these are not going to be mutually exclusive... some rows could be >2 some less than >2
  distinct(Site_Year_Code, Family, Type, .keep_all = TRUE)

setwd("~/Desktop/Wild Foods Repo/")
write.csv(be_fam_diff, "intermediate_files/be_family_sum_large_differences.csv")  

test <- be %>%
  filter(Site_Year_Code == "Angoon_2012") %>%
  filter(Family == "Ducks")


##Reasons why not adding up at:
##


##want to select genus level data if not broken down further, but keep species level if genus is broken down further.. 
##could i group by site_year_code, then filter out species not na and then species na but not genus na... ?? 
test2 <- be %>%
  group_by(Site_Year_Code) %>%
  filter(is.na(Season)) %>%
  filter(!is.na(Species))

test3 <- be %>%
  group_by(Site_Year_Code) %>%
  filter(is.na(Season)) %>%
  filter(is.na(Species)) %>%
  filter(!is.na(Genus))

test4 <- 
##no still not right.. 
  
##5) MARINE INVERTEBRATES -----------------
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
  mutate(General_Category = "Marine Invertebrates") %>%
  mutate(General_Category_lvl2 = "Marine Invertebrates") %>%
  mutate(Family = case_when(
    startsWith(Resource_Code, "5002") ~ "Abalone",
    startsWith(Resource_Code, "5004") ~ "Chiton",
    startsWith(Resource_Code, "5006") ~ "Clam",
    startsWith(Resource_Code, "5008") ~ "Cockle",
    startsWith(Resource_Code, "5010") ~ "Crab",
    startsWith(Resource_Code, "5012") ~ "Geoduck",
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
  mutate(Genus = case_when(
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
    startsWith(Resource_Code, "5012") ~ "Geoduck",
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
    startsWith(Resource_Code, "503206") ~ "Purple Sea Urchin",
    startsWith(Resource_Code, "50329") ~ "Unknown Sea Urchin",
    startsWith(Resource_Code, "5034") ~ "Shrimp",
    startsWith(Resource_Code, "5037") ~ "Starfish",
    startsWith(Resource_Code, "5038") ~ "Squid",
    startsWith(Resource_Code, "5040") ~ "Whelk",
    startsWith(Resource_Code, "5099") ~ "Unknown Marine Invertebrates",
  )) %>%
  mutate(Species = case_when(
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
    startsWith(Resource_Code, "5012") ~ "Geoduck",
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
    startsWith(Resource_Code, "503206") ~ "Purple Sea Urchin",
    startsWith(Resource_Code, "50329") ~ "Unknown Sea Urchin",
    startsWith(Resource_Code, "5034") ~ "Shrimp",
    startsWith(Resource_Code, "5037") ~ "Starfish",
    startsWith(Resource_Code, "5038") ~ "Squid",
    startsWith(Resource_Code, "5040") ~ "Whelk",
    startsWith(Resource_Code, "5099") ~ "Unknown Marine Invertebrates",
  )) %>%
  mutate(Habitat = case_when(
    grepl("King Crab", Species) ~ "Marine",
    startsWith(Family, "Octopus") ~ "Marine",
    startsWith(Family, "Scallop") ~ "Marine", 
    startsWith(Family, "Shrimp") ~ "Marine",
    grepl("Tanner Crab", Species) ~ "Marine",
    startsWith(Family, "Abalone") ~ "Nearshore",
    startsWith(Family, "Chiton") ~ "Nearshore",
    startsWith(Family, "Clam") ~ "Nearshore",
    startsWith(Family, "Cockle") ~ "Nearshore",
    startsWith(Species, "Dungeness") ~ "Nearshore",
    startsWith(Family, "Geoduck") ~ "Nearshore",
    startsWith(Family, "Limpet") ~ "Nearshore",
    startsWith(Family, "Mussel") ~ "Nearshore",
    startsWith(Family, "Sea Cucumber") ~ "Nearshore",
    startsWith(Family, "Sea Urchin") ~ "Nearshore",
    startsWith(Family, "Squid") ~ "Marine", 
    startsWith(Species, "Box Crab") ~ "Marine",
    startsWith(Species, "Unknown Crab") ~ "Marine",
    startsWith(Family, "Oyster") ~ "Marine",
    startsWith(Family, "Starfish") ~ "Nearshore",
    startsWith(Family, "Unknown Marine Invert") ~ "Marine",
  ))


mi_str <- mi %>%
  distinct(Habitat, General_Category, General_Category_lvl2, Family, Genus, Species,  Resource_Code, Resource_Name, Fishing_Gear_Type)

mi_sp_list <- mi %>%
  filter(!is.na(Species)) %>%
  filter(is.na(Fishing_Gear_Type)) %>%
  distinct(Habitat, General_Category, General_Category_lvl2, Family, Genus, Species)

##do the species within families add up? 
mi_fam_sum_test_func <- function(x){
  fam_db <- x %>%
    filter(is.na(Fishing_Gear_Type)) %>%
    filter(is.na(Genus)) %>%
    group_by(Site_Year_Code, Family) %>%
    summarise_at(vars(Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_db_sum"), Reported_Pounds_Harvested:Percent_Of_Total_Harvest)
  fam_calc <- x %>%
    filter(is.na(Fishing_Gear_Type)) %>%
    filter(Genus != "NA") %>%
    group_by(Site_Year_Code, Family) %>%
    summarise_at(vars(Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_fam_sum"), Reported_Pounds_Harvested:Percent_Of_Total_Harvest)
  fam_sum <- inner_join(fam_db, fam_calc, by = c("Site_Year_Code", "Family")) %>%
    select(Site_Year_Code, Family, sort(names(.)))
} 

mi_fam_sum_test <- split(mi, paste0(mi$Site_Year_Code)) %>%
  map(mi_fam_sum_test_func) %>%
  bind_rows()

mi_fam_diff_test <- mi_fam_sum_test %>%
  group_by(Site_Year_Code, Family) %>%
  mutate(est_total_lb_harv_diff = Estimated_Total_Pounds_Harvested_db_sum - Estimated_Total_Pounds_Harvested_fam_sum) %>%
  mutate(est_amount_harv_diff = Estimated_Amount_Harvested_db_sum - Estimated_Amount_Harvested_fam_sum) %>%
  mutate(mean_lb_perhousehold_diff = Mean_Pounds_Per_Household_db_sum - Mean_Pounds_Per_Household_fam_sum) %>%
  mutate(num_res_harv_diff = Number_Of_Resource_Harvested_db_sum - Number_Of_Resource_Harvested_fam_sum) %>%
  mutate(percap_lb_harv_diff = Percapita_Pounds_Harvested_db_sum - Percapita_Pounds_Harvested_fam_sum) %>%
  mutate(per_total_harv_diff = Percent_Of_Total_Harvest_db_sum - Percent_Of_Total_Harvest_fam_sum) %>%
  mutate(rep_lb_harv_diff = Reported_Pounds_Harvested_db_sum - Reported_Pounds_Harvested_fam_sum) %>%
  select(Site_Year_Code, Family, est_total_lb_harv_diff, est_amount_harv_diff, mean_lb_perhousehold_diff, num_res_harv_diff, percap_lb_harv_diff, per_total_harv_diff,  rep_lb_harv_diff)

str(mi_fam_diff_test)
##determine where magnitude is > +/- 2
mi_fam_diff_2 <- mi_fam_diff_test %>%
  filter(if_any(est_total_lb_harv_diff:rep_lb_harv_diff, ~ .x > 2))

mi_fam_diff_3 <- mi_fam_diff_test %>%
  filter(if_any(est_total_lb_harv_diff:rep_lb_harv_diff, ~ .x < -2))
mi_fam_diff <- rbind(mi_fam_diff_2, mi_fam_diff_3) %>%##these are not going to be mutually exclusive... some rows could be >2 some less than >2
  distinct(Site_Year_Code, Family, .keep_all = TRUE)

##trying to figure out why such a large magnitude of differences:
#setwd("~/Desktop/Wild Foods Repo/")
#write.csv(mi_fam_diff, "intermediate_files/mi_family_sum_large_differences.csv")  

test <- mi %>%
  filter(Family == "Crab")



##Crabs not broken down: surveys done before 1990 -- Angoon_1984,Angoon_1987, Beecher Pass_1987, Coffman Cove_1987, Craig_1987, Edna Bay_1987, Elfin Cove_1987, Gustavus_1987,  Haines_1983, Haines_1987, Hollis_1987, Hoonah_1985, Hoonah_1987, Hydaburg_1987, Hyder_1987, Kake_1985, Kake_1987, Kasaan_1987

##Reasons for large differences
##1. Crabs -- king crab and tanner crab are broken down further sometimes, so where broken down further, need to remove that intermediate genus summary line
###Seeming like king crab only not broken down in early years.. can then take same approach as irish lord
##for tanner crab, it is either unknown tanner crab or tanner crab - biardi, i think makes most sense to just keep the tanner crab level 
###Note: unknown tanner crab is a subsection of tanner crab... fuckng stupid 
##2. Family level sum of total estimated amount and/or reported number harvested are the weights, not the sum of # of individuals, this will be resolved when removing that family level sum line
##3. Minor addition errors at the family level, removing this level will fix these issues 


##6) VEGETATION --------------------

veg_code <- "6"

veg <- df_comp %>% 
  filter(str_detect(Resource_Code, '^6')) 

veg$Resource_Code  <- format(veg$Resource_Code, scientific = FALSE)
veg$Resource_Code <- as.character(veg$Resource_Code) 
str(veg)
sp_veg <- veg %>%
  distinct(Resource_Code, Resource_Name)

veg <- veg %>%
  mutate(General_Category = "Vegetation") %>%
  mutate(General_Category_lvl2 = "Vegetation") %>% ##don't think has another general cat
  mutate(Family = case_when(
    startsWith(Resource_Code, "6010") ~ "Berries",
    startsWith(Resource_Code, "6020400") ~ "Mushrooms", 
    startsWith(Resource_Code, "602046") ~ "Mushrooms",
    startsWith(Resource_Code, "6020") ~ "Plants/Greens", 
    ##want to break this down and separate this more... or maybe just add other functional traits but this is very broad
    startsWith(Resource_Code, "6030") ~ "Seaweed/Kelp",
    startsWith(Resource_Code, "6040") ~ "Wood", ##wood will have to be removed for diversity calculations
    startsWith(Resource_Code, "6050") ~ "Coal", ##remove for diversity calc
    startsWith(Resource_Code, "6053") ~ "Seaweed/Kelp", ##remove for diversity calc
  )) %>%
  mutate(Species = case_when(
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
    startsWith(Resource_Code, "6020460") ~ "Fungus",
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
    grepl("Kelp", Family) ~ "Nearshore",
    !grepl("Kelp", Family) ~ "Terrestrial",
  )) %>%
  mutate(Use = case_when(
    grepl("Fertilizer", Resource_Name) ~ "Fertilizer",
  ))

veg_str <- veg %>%
  distinct(Habitat, General_Category, General_Category_lvl2, Family, Species,  Resource_Code, Resource_Name, Harvest_Type, Use)

veg_sp_list <- mi %>%
  filter(!is.na(Species)) %>%
  filter(is.na(Fishing_Gear_Type)) %>%
  distinct(Habitat, General_Category, General_Category_lvl2, Family, Genus, Species)

##i changed the categories slightly to separate out mushrooms and added the kelp/seaweed for fertilizer to the seaweed/kelp cateogry, so things won't directly add up but just want to check the fungus/mushrooms thing, otehrwise this is all good to go
test <- veg %>%
  filter(Family == "Mushrooms")
##fungus and chaga only recorded in Yakutat_2015, where it is a duplicate, so will filter out the fungus row


##OLD CODE -------------------
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