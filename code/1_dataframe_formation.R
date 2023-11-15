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
  
##have 65 separate food webs to construct :D 

##function to test whether sum of gear types within species matches with sum value already given in database
##this function generates sums across numeric variables for rows in data with fishing gear, and those without, only for fish broken down into gear type
sp_gt_sum_test_func <- function(x){
  gt_sp <- x %>%
    filter(Species != "Herring Roe") %>%
    filter(Fishing_Gear_Type != "NA") %>%
    filter(!is.na(Species)) %>%
    group_by(Site_Year_Code, Family, Species) %>%
    summarise_at(vars(Percent_Using:Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest, Mean_Grams_Per_Capita_Use, Mean_Pounds_Per_Capita_Use, Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_gt_sum"), Percent_Using:Mean_Grams_Percapita_Harvest)
  ##need to do herring roe separate, as is a unique case -- because has another level of separation (based on where harvested from), so the ng total is double because it includes each levels no gear sum, so need to remove those.. 
  gt_hr <- x %>%
    filter(Species == "Herring Roe") %>%
    filter(Fishing_Gear_Type != "NA") %>%
    filter(is.na(Roe_Collection_Type)) %>% 
    group_by(Site_Year_Code, Family, Species) %>%
    summarise_at(vars(Percent_Using:Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest, Mean_Grams_Per_Capita_Use, Mean_Pounds_Per_Capita_Use, Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_gt_sum"), Percent_Using:Mean_Grams_Percapita_Harvest)
  gt_sp2 <- rbind(gt_sp, gt_hr)
  ng_sp <- x %>%
    filter(Species != "Herring Roe") %>%
    filter(Fishing_Gear_Type == "NA") %>%
    group_by(Site_Year_Code, Family, Species) %>%
    summarise_at(vars(Percent_Using:Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest, Mean_Grams_Per_Capita_Use, Mean_Pounds_Per_Capita_Use, Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_db_sum"), Percent_Using:Mean_Grams_Percapita_Harvest)
    ng_hr <- x %>%
    filter(Species == "Herring Roe") %>%
    filter(Fishing_Gear_Type == "NA") %>%
    filter(is.na(Roe_Collection_Type)) %>% ##so this keeps only the overall sum of herring roe row, not each sub-type of where collected sum row, so this should remove duplication
    group_by(Site_Year_Code, Family, Species) %>%
    summarise_at(vars(Percent_Using:Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest, Mean_Grams_Per_Capita_Use, Mean_Pounds_Per_Capita_Use, Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_db_sum"), Percent_Using:Mean_Grams_Percapita_Harvest)
  ng_sp2 <- rbind(ng_sp, ng_hr)
  gear_sum <- inner_join(gt_sp2, ng_sp2, by = c("Site_Year_Code", "Family", "Species")) %>%
    select(Site_Year_Code, Family, Species, sort(names(.)))
}
##next step is to compare the sum already in the database, to the one calculated here by summing the gear type

##function to test if species sum within family to the family level sum already given in the database
fam_sum_test_func <- function(x){
  fam_db <- x %>%
    filter(Fishing_Gear_Type == "NA") %>%
    filter(is.na(Species)) %>%
    group_by(Site_Year_Code, Family) %>%
    summarise_at(vars(Percent_Using:Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest, Mean_Grams_Per_Capita_Use, Mean_Pounds_Per_Capita_Use, Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
    rename_with(~paste0(., "_db_sum"), Percent_Using:Mean_Grams_Percapita_Harvest)
  fam_calc <- x %>%
    filter(Fishing_Gear_Type == "NA") %>%
    filter(Species != "NA") %>%
    group_by(Site_Year_Code, Family) %>%
    summarise_at(vars(Percent_Using:Estimated_Total_Pounds_Harvested, Mean_Pounds_Per_Household, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested, Percent_Of_Total_Harvest, Mean_Grams_Per_Capita_Use, Mean_Pounds_Per_Capita_Use, Mean_Grams_Percapita_Harvest), sum, na.rm = TRUE) %>%
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

##Which columns do not add up? i.e., the nested versions to do not generate the same values as already given in the database - set threshold of difference >1
##want to set up function that will tell me if values for specific pairs of columns are close to eachother or not
##there are some columns we may expect that they wouldn't be the same, for ex. confidence intervals, because these may not be a sum depending on how they are calculated (do want to figure out how they are)
head(sp_gt_sum_test)

angoon_1984<- fish %>%
  filter(Site_Year_Code == "Angoon_1984")


##testing calculating difference and then selecting those rows with a magnitude greater than 2 lbs (note for now just starting with est. total lbs harvested)
##to-do next once working: expand to other variables (only that you would expect to sum!!) and then add this into the function so all of this happens at once
##variables we would expect to add up: Estimated_Total_Pounds_Harvested, Mean_Grams_Percapita_Harvest, Mean_Pounds_Per_Household, Number_of_Resource_Harvested, Percapita_Pounds_Harvested, Percent_Attempting_to_Harvest, Percent_Giving, Percent_Harvesting, Percent_Recieving, Percent_Using, Reported_Pounds_Harvested, 
##variables we wouldn't expect to add up: the confidence intervals 
sp_gt_diff_test <- sp_gt_sum_test %>%
    group_by(Site_Year_Code, Family, Species) %>%
    mutate(est_total_lb_harv_diff = Estimated_Total_Pounds_Harvested_db_sum - Estimated_Total_Pounds_Harvested_gt_sum) %>%
    mutate(est_amount_harv_diff = Estimated_Amount_Harvested_db_sum - Estimated_Amount_Harvested_gt_sum) %>%
    mutate(mean_g_percapita_use_diff = Mean_Grams_Per_Capita_Use_db_sum - Mean_Grams_Per_Capita_Use_gt_sum) %>%
    mutate(mean_g_percapita_harv_diff = Mean_Grams_Percapita_Harvest_db_sum - Mean_Grams_Percapita_Harvest_gt_sum) %>%
    mutate(mean_lb_percapita_use_diff = Mean_Pounds_Per_Capita_Use_db_sum - Mean_Pounds_Per_Capita_Use_gt_sum) %>%
    mutate(mean_lb_perhousehold_diff = Mean_Pounds_Per_Household_db_sum - Mean_Pounds_Per_Household_gt_sum) %>%
    mutate(num_res_harv_diff = Number_Of_Resource_Harvested_db_sum - Number_Of_Resource_Harvested_gt_sum) %>%
    mutate(percap_lb_harv_diff = Percapita_Pounds_Harvested_db_sum - Percapita_Pounds_Harvested_gt_sum) %>%
    mutate(per_attempt_harv_diff = Percent_Attempting_to_Harvest_db_sum - Percent_Attempting_to_Harvest_gt_sum) %>%
    mutate(per_give_diff = Percent_Giving_db_sum - Percent_Giving_gt_sum) %>%
    mutate(per_harv_diff = Percent_Harvesting_db_sum - Percent_Harvesting_gt_sum) %>%
    mutate(per_total_harv_diff = Percent_Of_Total_Harvest_db_sum - Percent_Of_Total_Harvest_gt_sum) %>%
    mutate(per_rec_diff = Percent_Receiving_db_sum - Percent_Receiving_gt_sum) %>%
    mutate(per_use_diff = Percent_Using_db_sum - Percent_Using_gt_sum) %>%
    mutate(rep_lb_harv_diff = Reported_Pounds_Harvested_db_sum - Reported_Pounds_Harvested_gt_sum) %>%
    select(Site_Year_Code, Family, Species, est_total_lb_harv_diff, est_amount_harv_diff, mean_g_percapita_use_diff, mean_g_percapita_harv_diff, mean_lb_percapita_use_diff, mean_lb_perhousehold_diff, num_res_harv_diff, percap_lb_harv_diff, per_attempt_harv_diff, per_give_diff, per_harv_diff, per_total_harv_diff, per_rec_diff, per_use_diff, rep_lb_harv_diff)

##so differences in sums are not consistent... if low differences somewhere, doesn't mean low differences elsewhere.. 
str(sp_gt_diff_test)

   
sp_gt_diff_2 <- sp_gt_diff_test %>%
  filter(if_any(est_total_lb_harv_diff:rep_lb_harv_diff, ~ .x > 2))

sp_gt_diff_3 <- sp_gt_diff_test %>%
  filter(if_any(est_total_lb_harv_diff:rep_lb_harv_diff, ~ .x < -2))
sp_gt_diff <- rbind(sp_gt_diff_2, sp_gt_diff_3) %>%##these are not going to be mutually exclusive... some rows could be >2 some less than >2
  distinct(Site_Year_Code, Family, Species, .keep_all = TRUE)

##869/1604 have some variable where the difference is greater or less than a value of 2....
##I think the main issue is the percent using/giving etc. because they often just put the value for the sum, for each gear breakdown. so of course the sum is not going to add up. I am not sure how much I trust the % data. This will also then cause issues w/ any of the mean percapita % using.. if they are based on those values. 


##Seeing issues w/ non-percent variables
sp_gt_diff_test_2 <- sp_gt_diff_test %>%
  select(Site_Year_Code, Family, Species, est_total_lb_harv_diff, est_amount_harv_diff, num_res_harv_diff, percap_lb_harv_diff, rep_lb_harv_diff)


sp_gt_diff_4 <- sp_gt_diff_test_2 %>%
  filter(if_any(est_total_lb_harv_diff:rep_lb_harv_diff, ~ .x > 2))

sp_gt_diff_5 <- sp_gt_diff_test_2 %>%
  filter(if_any(est_total_lb_harv_diff:rep_lb_harv_diff, ~ .x < -2))
sp_gt_diff2 <- rbind(sp_gt_diff_4, sp_gt_diff_5) %>%
  distinct(Site_Year_Code, Family, Species, .keep_all = TRUE)
##okay, so when ignoring the percent data, or values derived from the % data, it is the same 73/1604 (4.5%) that have issues as only the est_total harvest, so same 3 reasons as listed below
##but the % stuff is kinda messed up, i need to understand more how those % values are input/what assumptions are made.. 

##variables that tend to have the greatest magnitude of differences, consistently 
##



#setwd("~/Desktop/Wild Foods Repo/")
#write.csv(sp_gt_diff, "intermediate_files/gear_type_sum_large_differences.csv")  

##so for only est total lbs harvested, differences +/- 2, 73/1604 (4.5%) records 
##need to figure out if there is some consistent pattern why... and which is more accurate in terms of moving forward, already summed or gear type breakdown? 
##trying to figure out why such a large magnitude of differences:
test <- fish %>%
  filter(Site_Year_Code == "Hoonah_2012") %>%
  filter(Species == "Halibut")


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
  mutate(est_total_lb_harv_diff = Estimated_Total_Pounds_Harvested_db_sum - Estimated_Total_Pounds_Harvested_fam_sum)

fam_diff_test$est_total_lb_harv_diff <- as.numeric(fam_diff_test$est_total_lb_harv_diff)
fam_diff_test$est_total_lb_harv_diff  <- format(fam_diff_test$est_total_lb_harv_diff, scientific = FALSE)


fam_diff_2 <- filter(fam_diff_test, est_total_lb_harv_diff > 2) %>%
  select(Site_Year_Code, Family, Estimated_Total_Pounds_Harvested_db_sum, Estimated_Total_Pounds_Harvested_fam_sum, est_total_lb_harv_diff)
fam_diff_3 <- filter(fam_diff_test, est_total_lb_harv_diff > -2) %>%
  select(Site_Year_Code, Family, Estimated_Total_Pounds_Harvested_db_sum, Estimated_Total_Pounds_Harvested_fam_sum, est_total_lb_harv_diff)
fam_diff <- rbind(fam_diff_2, fam_diff_3)

##this is not working, idk why come back to tmrw




  sp_gt_diff_test[abs(sp_gt_diff_test$est_total_lb_harv_diff > 2)]
  
  sp_gt_diff_test %>% filter(abs(est_total_lb_harv_diff > 2))
  
str(sp_gt_diff_test)
sp_gt_diff_test$est_total_lb_harv_diff  <- format(sp_gt_diff_test$est_total_lb_harv_diff, scientific = FALSE)
    
fish$Resource_Code  <- format(fish$Resource_Code, scientific = FALSE)

filter(abs(Estimated_Total_Pounds_Harvested_db_sum - Estimated_Total_Pounds_Harvested_gt_sum < 1))
    
    
           
               
               
               abs(long - long[ID == x]) < 1,
           abs(score - score[ID == x]) < 0.7,
           ID != x) %>%
    pull(ID) %>%
    paste0(collapse = ',')
  
})

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