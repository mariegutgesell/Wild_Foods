##ADF&G Harvest Timings
library(tidyverse)
library(ggplot2)
library(readxl)
library(reshape2)
df_mam <- read_excel("data/Harvest_Timing_ADFG.xlsx", sheet = 1)
df_bird <- read_excel("data/Harvest_Timing_ADFG.xlsx", sheet = 2)

##clean up mammal dataframe to remove the sex specifics
df_mam <- df_mam %>%
  filter(!grepl("male", Resource)) %>%
  filter(!grepl("sex", Resource)) %>%
  filter(!grepl("cow", Resource)) %>%
  filter(!grepl("bull", Resource)) %>%
  rename(Lowest_Common_Taxon_Name = "Resource")

##read in taxa characteristics
char <- read_excel("data/harvest_species_list_characteristics_5.xlsx", sheet = 1)

df_mam_2 <- df_mam %>%
  left_join(char, by = "Lowest_Common_Taxon_Name") %>%
  select(Community, Year, Taxa_lvl1:Taxa_lvl5, Lowest_Common_Taxon_Name, Habitat:Trophic_Category, Jan:Unk)

test <- df_mam_2 %>%
  filter(is.na(Taxa_lvl1)) %>%
  select(Lowest_Common_Taxon_Name, Taxa_lvl1:Taxa_lvl5) %>%
  distinct()

##still need to fix to get squirrel info in there

df_mam_long <- df_mam_2 %>%
  select(Community, Year, Lowest_Common_Taxon_Name, Habitat, Jan:Dec) %>% ##for now not including unknown in time series
  melt(id.vars = c("Community", "Year", "Lowest_Common_Taxon_Name", "Habitat"), variable.name = "Month") %>%
  filter(value != 0)

df_mam_long %>%
  filter(Lowest_Common_Taxon_Name == "Deer") %>%
  ggplot(aes(x = Month, y = value, group = Lowest_Common_Taxon_Name, color = Lowest_Common_Taxon_Name)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~Community,)

##bird harvest
df_bird <- df_bird %>%
  dplyr::rename(Lowest_Common_Taxon_Name = "Resource")

df_bird_2 <- df_bird %>%
  left_join(char, by = "Lowest_Common_Taxon_Name") #%>%
  select(Community, Year, Taxa_lvl1:Taxa_lvl5, Lowest_Common_Taxon_Name, Habitat:Trophic_Category, Jan:Unk)
##need to clean up names..for now just plot 


df_bird_long <- df_bird %>%
  melt(id.vars = c("Community", "Year", "Lowest_Common_Taxon_Name"), variable.name = "Season") %>%
  filter(value != 0)

ggplot(df_bird_long, aes(x = Season, y = value, group = Lowest_Common_Taxon_Name, color = Lowest_Common_Taxon_Name)) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~Community, scale = "free")


df_bird_long$Season <- ordered(df_bird_long$Season, 
                               levels = c("Spring", "Summer", "Fall", "Winter", "Season Unknown"))

  
ggplot(df_bird_long, aes(x = Season, y = value,group = Lowest_Common_Taxon_Name, color = Lowest_Common_Taxon_Name )) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~Community)


##Looking at reported harvest for species w/ harvest data
df_final <- read.csv("data/intermediate_data/harvest_data_clean.csv")

df_final <- df_final %>%
  filter(!if_all(Percent_Using:Estimated_Amount_Harvested, ~ .x == 0))

birds <- df_final %>%
  filter(Taxa_lvl1 == "Birds and Eggs")

halibut <- df_final %>%
  filter(Taxa_lvl4 == "Halibut")

ggplot(halibut, aes(x = Site_Year_Code, y = Estimated_Total_Pounds_Harvested)) +
  geom_col() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x = element_blank(),  text = element_text(family = "Times New Roman"), strip.background = element_blank()) 

ggplot(halibut, aes(x = Site_Year_Code, y = Percapita_Pounds_Harvested)) +
  geom_col() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x = element_blank(),  text = element_text(family = "Times New Roman"), strip.background = element_blank()) 

test <- df_final %>%
  filter(Taxa_lvl5 == "Marten")
