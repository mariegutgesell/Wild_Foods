##Food from Forests Project
##Calculating totals and generating figures


library(readxl)
library(tidyverse)
library(ggplot2)

getwd()
##1) Total Harvest Data
##read in data
terr <- read_excel("data/Foods_From_Forests_data/Copy of Southeast harvest summary_mg.xlsx", sheet = "Terrestrial") %>%
  select(Community, Study_Year, Per_capita_lb, `2022_population`, Estimated_total_lb) %>%
  mutate(Category = "Terrestrial")
ana <- read_excel("data/Foods_From_Forests_data/Copy of Southeast harvest summary_mg.xlsx", sheet = "Anadromous") %>%
  select(Community, Study_Year, Per_capita_lb, `2022_population`, Estimated_total_lb) %>%
  mutate(Category = "Anadromous")
ns <- read_excel("data/Foods_From_Forests_data/Copy of Southeast harvest summary_mg.xlsx", sheet = "Nearshore") %>%
  select(Community, Study_Year, Per_capita_lb, `2022_population`, Estimated_total_lb) %>%
  mutate(Category = "Nearshore")
mar <- read_excel("data/Foods_From_Forests_data/Copy of Southeast harvest summary_mg.xlsx", sheet = "Marine") %>%
  select(Community, Study_Year, Per_capita_lb, `2022_population`, Estimated_total_lb) %>%
  mutate(Category = "Marine")

sub_sum <- rbind(terr, ana, ns, mar) %>%
  mutate(estimated_total_kg = Estimated_total_lb*0.45359237)

total_harvest_all_lb <- sum(sub_sum$Estimated_total_lb)
total_harvest_all_kg <- sum(sub_sum$estimated_total_kg)

total_harvest <- sub_sum %>%
  group_by(Category) %>%
  summarise_at(vars(Estimated_total_lb, estimated_total_kg), sum) %>%
  mutate(total_harvest_all_cat_lb = total_harvest_all_lb) %>%
  mutate(total_harvest_all_cat_kg = total_harvest_all_kg) %>%
  mutate(percent_total_harvest = (estimated_total_kg/total_harvest_all_kg)*100) 

    


total_harvest$Category <- ordered(total_harvest$Category,
                                    levels = c("Anadromous", "Marine", "Nearshore", "Terrestrial"))

library(ggrepel)
total_harvest$pos = (cumsum(c(0, total_harvest$estimated_total_kg)) + c(total_harvest$estimated_total_kg / 2, .01))[1:nrow(total_harvest)]
  
total_harvest<-  total_harvest %>%
    arrange(desc(Category)) %>%
    mutate(pos_2 = cumsum(estimated_total_kg) - estimated_total_kg/2)
  


total_harvest$percent_total_harvest = round(total_harvest$percent_total_harvest, digits = 2)
total_harvest$estimated_total_kg = round(total_harvest$estimated_total_kg, digits = 2)



total_var_plot <- total_harvest %>%
  arrange(desc(Category)) %>%
  mutate(pos_2 = cumsum(estimated_total_kg) - estimated_total_kg/2) %>%
  ggplot(aes(x= "", y = estimated_total_kg, fill = Category)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste(percent_total_harvest, "%", sep = "")), position = position_stack(vjust = 0.5), color = "white", size=3) +
  scale_fill_manual(values = c("aquamarine3", "azure4", "deepskyblue3", "chartreuse4" )) +
  geom_label_repel(aes(x =1.4, y = pos_2, label = paste0(estimated_total_kg, "kg")),
                   size = 4, nudge_x = 0.6, show.legend = FALSE, fill = alpha("white", 0.5), label.size = NA)


total_var_plot  

library(data.table)
##doing individual categories pie charts
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
 

latest_surveys <- survey_demographics %>%
  filter(Most_Rep_Year == "Yes") %>%
  filter(Site_Year_Code != "Beecher Pass_1987") %>%
  filter(Site_Year_Code != "Hoonah_2016") %>%
  filter(Site_Year_Code != "Yakutat_2000") %>%
  filter(Site_Year_Code != "Klukwan_1996")
         
         
##reduce dataframe to only focus on years/sites using for this project 
df_2 <- df %>%
  filter(Site_Year_Code %in% latest_surveys$Site_Year_Code) %>% ##selects only years where a comprehensive survey was done
  filter(!grepl("Marine Mammals", Project_Name))
  
##select resource groups using for this study
resource_list <- read_excel("Foods_From_Forests_data/Copy of Southeast harvest summary_mg.xlsx", sheet = "Resource_List")

df_2_res <- df_2 %>%
  filter(Resource_Name %in% resource_list$Resource_Name) %>%
  left_join(resource_list, by = "Resource_Name") %>%
  select(Site_Year_Code, Category, Resource_Name, Estimated_Total_Pounds_Harvested) %>%
  mutate(estimated_total_kg = Estimated_Total_Pounds_Harvested*0.45359237) %>%
  group_by(Category, Resource_Name) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested, estimated_total_kg), sum)

df_2_res$Estimated_Total_Pounds_Harvested  <- format(df_2_res$Estimated_Total_Pounds_Harvested, scientific = FALSE)
df_2_res$estimated_total_kg  <- format(df_2_res$estimated_total_kg, scientific = FALSE)

df_2_res$Estimated_Total_Pounds_Harvested  <- as.numeric(df_2_res$Estimated_Total_Pounds_Harvested)
df_2_res$estimated_total_kg  <- as.numeric(df_2_res$estimated_total_kg)


total_harvest_cat <- df_2_res %>%
  group_by(Category) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested, estimated_total_kg), sum) %>%
  dplyr::rename(cat_total_est_lb = Estimated_Total_Pounds_Harvested) %>%
  dplyr::rename(cat_total_est_kg = estimated_total_kg)

df_2_res <- df_2_res %>%
  left_join(total_harvest_cat, by= "Category") %>%
  mutate(percent_total_harvest = (estimated_total_kg/cat_total_est_kg)*100) 



df_2_res$Estimated_Total_Pounds_Harvested  <- format(df_2_res$Estimated_Total_Pounds_Harvested, scientific = FALSE)
df_2_res$estimated_total_kg  <- format(df_2_res$estimated_total_kg, scientific = FALSE)

df_2_res$Estimated_Total_Pounds_Harvested  <- as.numeric(df_2_res$Estimated_Total_Pounds_Harvested)
df_2_res$estimated_total_kg  <- as.numeric(df_2_res$estimated_total_kg)
##anyway to stop these from going back to scientific format? annnnooying

df_2_res$estimated_total_kg = round(df_2_res$estimated_total_kg, digits = 2)
df_2_res$percent_total_harvest = round(df_2_res$percent_total_harvest, digits = 2)

str(df_2_res)

##my numbers are close to the terrestrial ones that Lauren got, but not exactly the same... why? 

##terrestrial plot
terr_plot <- df_2_res %>%
  filter(Category == "Terrestrial") %>%
  arrange(desc(Resource_Name)) %>%
  mutate(pos_2 = cumsum(estimated_total_kg) - estimated_total_kg/2) %>%
  ggplot(aes(x= "", y = estimated_total_kg, fill = Resource_Name)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste(percent_total_harvest, "%", sep = "")), position = position_stack(vjust = 0.5), color = "black", size=3) +
 # scale_fill_brewer() +
  geom_label_repel(aes(x =1.4, y = pos_2, label = paste0(estimated_total_kg, "kg")),
                   size = 4, nudge_x = 0.6, show.legend = FALSE, fill = alpha("white", 0.5), label.size = NA) +
 labs(title = "Terrestrial")

terr_plot


##anadromous plot
ana_plot <- df_2_res %>%
  filter(Category == "Anadromous") %>%
  arrange(desc(Resource_Name)) %>%
  mutate(pos_2 = cumsum(estimated_total_kg) - estimated_total_kg/2) %>%
  ggplot(aes(x= "", y = estimated_total_kg, fill = Resource_Name)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste(percent_total_harvest, "%", sep = "")), position = position_stack(vjust = 0.5), color = "black", size=3) +
 # scale_fill_brewer() +
  geom_label_repel(aes(x =1.4, y = pos_2, label = paste0(estimated_total_kg, "kg")),
                   size = 4, nudge_x = 0.6, show.legend = FALSE, fill = alpha("white", 0.5), label.size = NA) +
  labs(title = "Anadromous")

ana_plot


##nearshore plot
ns_plot <- df_2_res %>%
  filter(Category == "Nearshore") %>%
  arrange(desc(Resource_Name)) %>%
  mutate(pos_2 = cumsum(estimated_total_kg) - estimated_total_kg/2) %>%
  ggplot(aes(x= "", y = estimated_total_kg, fill = Resource_Name)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste(percent_total_harvest, "%", sep = "")), position = position_stack(vjust = 0.5), color = "black", size=3) +
  #scale_fill_brewer() +
  geom_label_repel(aes(x =1.4, y = pos_2, label = paste0(estimated_total_kg, "kg")),
                   size = 4, nudge_x = 0.6, show.legend = FALSE, fill = alpha("white", 0.5), label.size = NA) + 
  labs(title = "Nearshore")
ns_plot


##do percents of nearshore -- 
##if less than 
##categorize them 
##Marine -- > halibut/other? 
#Nearshore --> seaweed, herring spawn (keep location?), crabs, molluscs
##tree diagram?
##other ways of visualizing other than pie chart? 
##testing i have all the groups 
res_list <- unique(df_2_res$Resource_Name)
##yassssss

##seeing if I can make a sankey tree diagram.. 
#install.packages("remotes")
library(remotes)
remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)

