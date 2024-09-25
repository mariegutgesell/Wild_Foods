##Food from Forests Project
##Calculating estimated % harvest and creating pie charts (not using for manuscript)


library(readxl)
library(tidyverse)
library(ggplot2)

library(ggrepel)
library(data.table)


getwd()
setwd("/Users/mariegutgesell/Desktop/Wild Foods Repo/")
##1) PIE CHARTS
####Total Harvest Data estimated for 2022 by Lauren (note: want to to all calculations raw, will have to read in population by year)
##read in data to get 2022 year population
terr <- read_excel("data/Foods_From_Forests_data/Copy of Southeast harvest summary_mg.xlsx", sheet = "Terrestrial") %>%
  select(Community, Study_Year, Per_capita_lb, `2022_population`, Estimated_total_lb) %>%
  mutate(Category = "Terrestrial")

##2) Calculate total harvest by resource group (can use for resource group pie charts and sankey diagram)

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
 
##Determine the surveys from the most representative year, make sure have all surveys/communities want to use for this analysis, remove double years
latest_surveys <- survey_demographics %>%
  filter(Most_Rep_Year == "Yes") %>%
  filter(Site_Year_Code != "Beecher Pass_1987") %>%
  filter(Site_Year_Code != "Hoonah_2016") %>%
  filter(Site_Year_Code != "Yakutat_2000") %>%
  filter(Site_Year_Code != "Klukwan_1996") %>%
  left_join(terr, by = c("Community")) %>%
  select(Site_Year_Code, Community, Year, `2022_population`)
         
         
##reduce dataframe to only focus on years/sites using for this project 
df_2 <- df %>%
  filter(Site_Year_Code %in% latest_surveys$Site_Year_Code) %>% ##selects only years where a comprehensive survey was done
  filter(!grepl("Marine Mammals", Project_Name))%>%
  dplyr::rename(Community = "Community_Name") %>%
  left_join(latest_surveys, by = c("Site_Year_Code", "Community"))

  
##select resource groups using for this study
resource_list <- read_excel("Foods_From_Forests_data/Copy of Southeast harvest summary_mg.xlsx", sheet = "Resource_List")


##generating list of all unique species to make sure have all categorized
#sp_all <- df_2 %>%
#  distinct(Resource_Code, Resource_Name)
#write.csv(sp_all, "Foods_From_Forests_data/Species_List.csv")

##calculate total pounds and kgs harvested based on per capita harvest weights from representative year and 2022 population, per resource group
df_2_res <- df_2 %>%
  filter(Resource_Name %in% resource_list$Resource_Name) %>%
  left_join(resource_list, by = "Resource_Name") %>%
  select(Site_Year_Code, Category, Resource_Group, Resource_Name, Percapita_Pounds_Harvested, `2022_population`) %>%
  mutate(est_total_lbs_2022 = Percapita_Pounds_Harvested*`2022_population`) %>% ##need to join 2022 population estimate and multiply our per capita harvested to get estimate of total in 2022
  mutate(est_total_kgs_2022 = est_total_lbs_2022*0.45359237) %>%
  group_by(Category, Resource_Group) %>% ##change back to resource_group, just testing to see where differences are
  summarise_at(vars(est_total_lbs_2022, est_total_kgs_2022), sum)


##calculate total harvest of all categories
total_harvest_all_lb <- sum(df_2_res$est_total_lbs_2022)
total_harvest_all_kg <- sum(df_2_res$est_total_kgs_2022)


##calculate the total harvest for each category (terrestrial, anadromous, marine, nearshore)
total_harvest_cat <- df_2_res %>%
  group_by(Category) %>%
  summarise_at(vars(est_total_lbs_2022, est_total_kgs_2022), sum) %>% ##calculate sum of lbs and kg harvested by category
  dplyr::rename(cat_total_est_lb = est_total_lbs_2022) %>% ##Rename columns to indicate sum of category
  dplyr::rename(cat_total_est_kg = est_total_kgs_2022) %>% 
  mutate(percent_total_harvest_cat_all = (cat_total_est_kg/total_harvest_all_kg)*100)
  

##join total harvest by category back to df_2_res so can calculate percentage of harvest by category, and percent total harvest
df_2_res <- df_2_res %>%
  left_join(total_harvest_cat, by= "Category") %>%
  mutate(percent_total_harvest_cat = (est_total_kgs_2022/cat_total_est_kg)*100)%>%
  mutate(total_harvest_all_lb = total_harvest_all_lb) %>%
  mutate(total_harvest_all_kg = total_harvest_all_kg) %>%
  mutate(percent_total_harvest_all = (est_total_kgs_2022/total_harvest_all_kg)*100) %>%
  mutate(est_total_1000kg_2022 = est_total_kgs_2022/1000)

str(df_2_res)


#df_2_res$Estimated_Total_Pounds_Harvested  <- format(df_2_res$Estimated_Total_Pounds_Harvested, scientific = FALSE)
#df_2_res$estimated_total_kg  <- format(df_2_res$estimated_total_kg, scientific = FALSE)

#df_2_res$Estimated_Total_Pounds_Harvested  <- as.numeric(df_2_res$Estimated_Total_Pounds_Harvested)
#df_2_res$estimated_total_kg  <- as.numeric(df_2_res$estimated_total_kg)
##anyway to stop these from going back to scientific format? annnnooying

df_2_res$cat_total_est_kg = round(df_2_res$cat_total_est_kg, digits = 2)
df_2_res$percent_total_harvest_ = round(df_2_res$cat_total_est_kg, digits = 2)




##Pie Charts --------------------
##generate pie chart for all harvest
total_harvest_cat$cat_total_est_kg = round(total_harvest_cat$cat_total_est_kg, digits = 2)
total_harvest_cat$percent_total_harvest_cat_all = round(total_harvest_cat$percent_total_harvest_cat_all, digits = 2)

total_var_plot <- total_harvest_cat %>%
  arrange(desc(Category)) %>%
  mutate(pos_2 = cumsum(cat_total_est_kg) - cat_total_est_kg/2) %>%
  ggplot(aes(x= "", y = cat_total_est_kg, fill = Category)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste(percent_total_harvest_cat_all, "%", sep = "")), position = position_stack(vjust = 0.5), color = "white", size=3) +
  scale_fill_manual(values = c("aquamarine3", "azure4", "deepskyblue3", "chartreuse4" )) +
  geom_label_repel(aes(x =1.4, y = pos_2, label = paste0(cat_total_est_kg, "kg")),
                   size = 4, nudge_x = 0.6, show.legend = FALSE, fill = alpha("white", 0.5), label.size = NA)

total_var_plot 

##NOTE: need to come back and clean this up to make category specific pie charts work 
##terrestrial plot
df_2_res$est_total_kgs_2022 = round(df_2_res$est_total_kgs_2022, digits = 2)
df_2_res$percent_total_harvest_cat = round(df_2_res$percent_total_harvest_cat, digits = 2)


terr_plot <- df_2_res %>%
  filter(Category == "Terrestrial") %>%
  arrange(desc(Resource_Group)) %>%
  mutate(pos_2 = cumsum(estimated_total_kg) - estimated_total_kg/2) %>%
  ggplot(aes(x= "", y = estimated_total_kg, fill = Resource_Group)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste(percent_total_harvest_cat, "%", sep = "")), position = position_stack(vjust = 0.5), color = "black", size=3) +
 # scale_fill_brewer() +
  geom_label_repel(aes(x =1.4, y = pos_2, label = paste0(estimated_total_kg, "kg")),
                   size = 4, nudge_x = 0.6, show.legend = FALSE, fill = alpha("white", 0.5), label.size = NA) +
 labs(title = "Terrestrial")

terr_plot


##anadromous plot
ana_plot <- df_2_res %>%
  filter(Category == "Anadromous") %>%
  arrange(desc(Resource_Group)) %>%
  mutate(pos_2 = cumsum(estimated_total_kg) - estimated_total_kg/2) %>%
  ggplot(aes(x= "", y = estimated_total_kg, fill = Resource_Group)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste(percent_total_harvest_cat, "%", sep = "")), position = position_stack(vjust = 0.5), color = "black", size=3) +
 # scale_fill_brewer() +
  geom_label_repel(aes(x =1.4, y = pos_2, label = paste0(estimated_total_kg, "kg")),
                   size = 4, nudge_x = 0.6, show.legend = FALSE, fill = alpha("white", 0.5), label.size = NA) +
  labs(title = "Anadromous")

ana_plot


##nearshore plot
ns_plot <- df_2_res %>%
  filter(Category == "Nearshore") %>%
  arrange(desc(Resource_Group)) %>%
  mutate(pos_2 = cumsum(estimated_total_kg) - estimated_total_kg/2) %>%
  ggplot(aes(x= "", y = estimated_total_kg, fill = Resource_Group)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste(percent_total_harvest_cat, "%", sep = "")), position = position_stack(vjust = 0.5), color = "black", size=3) +
  #scale_fill_brewer() +
  geom_label_repel(aes(x =1.4, y = pos_2, label = paste0(estimated_total_kg, "kg")),
                   size = 4, nudge_x = 0.6, show.legend = FALSE, fill = alpha("white", 0.5), label.size = NA) + 
  labs(title = "Nearshore")
ns_plot




##TO-DO:
##Need to sort and add the missing categories as listed in red on summary excel sheet - done
##Need to double check that all categorizations are appropriate - done
##Need to add Port Protection and point baker - done
##See if values are same as Lauren, if not want to double check and figure out why... - done they are the same!! 
##Then will need to update values for Sankey diagram to make sure all proper connections and % of total 
##Then it should just be making final figure... 
##Then clean up code and make script for final working figure
##Calculate species richness .. then will clean up script and put in separate script 

##Species Richness  -----------------

##generating list of all unique species to make sure have all categorized
sp_all <- df_2 %>%
  distinct(Resource_Code, Resource_Name)
