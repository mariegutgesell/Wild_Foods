##Harvest Contributions across habitat and key groups, Sankey Figure -- Tongass and Chugach
##October 24, 2024
##Marie Gutgesell 


library(readxl)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(data.table)
library(networkD3)

setwd("/Users/mariegutgesell/Desktop/Wild Foods Repo/")
getwd()
##Read in estimated population size data -- latest year I have is 2020 for all communities in Chugach and Tongass
pop_df <- read_excel("data/CSIS_Community_Demographics.xlsx", sheet = 1) %>%
  filter(Year == 2020) %>%
  select(Community:Community_Population)



##Read in cleaned Tongass and Chugach data -- then will select most representative year
c_df <- read.csv("data/intermediate_data/chugach_harvest_data_clean.csv") %>%
  mutate(Forest = "Chugach")
t_df <- read.csv("data/intermediate_data/tongass_harvest_data_clean.csv") %>%
  mutate(Forest = "Tongass")
df <- rbind(t_df, c_df) %>%
  select(Forest, Project_Name:Mean_Grams_Percapita_Harvest) %>%
  separate(Site_Year_Code, into = c("Community", "Harvest_Survey_Year"), sep = "_", remove = FALSE)

##sp rich
sp_rich <- df %>%
  filter(Percapita_Pounds_Harvested != "0") %>%
  select(Taxa_lvl3, Taxa_lvl4, Taxa_lvl5) %>%
  distinct() %>%
  filter(!grepl("Unknown", Taxa_lvl5)) %>%
  filter(Taxa_lvl5 != Taxa_lvl3) %>%
  filter(Taxa_lvl5 != Taxa_lvl4)

##read in comprehensive survey demographics
survey_demographics <- read_excel("data/CSIS_SurveyData_Demographics.xlsx", sheet = 2) %>%
  unite(Site_Year_Code, c(Community, Year), sep = "_", remove = FALSE) 

##Determine the surveys from the most representative year, make sure have all surveys/communities want to use for this analysis, remove double years
latest_surveys <- survey_demographics %>%
  filter(Most_Rep_Year == "Yes") %>%
  filter(Site_Year_Code != "Beecher Pass_1987") %>%
  filter(Site_Year_Code != "Hoonah_2016") %>%
  filter(Site_Year_Code != "Yakutat_2000") %>%
  filter(Site_Year_Code != "Klukwan_1996") %>%
  rename(Harvest_Survey_Year = "Year") %>%
  left_join(pop_df, by = c("Community")) %>%
  select(Site_Year_Code, Community, Year, Community_Population)

##reduce dataframe to only focus on years/sites using for this project 
df_2 <- df %>%
  dplyr::filter(Site_Year_Code %in% latest_surveys$Site_Year_Code) %>% ##selects only years where a comprehensive survey was done
  dplyr::filter(!grepl("Marine Mammals", Project_Name))%>%
  left_join(latest_surveys, by = c("Site_Year_Code", "Community")) %>%
  rename(Community_Population_2020 = "Community_Population")
df_2$Community_Population_2020 <- as.numeric(df_2$Community_Population_2020)
str(df_2)
##calculate total pounds and kgs harvested based on per capita harvest weights from representative year and 2020 population
df_3 <- df_2 %>%
  select(Forest, Site_Year_Code, Habitat, Taxa_lvl1:Taxa_lvl5, Percapita_Pounds_Harvested, Community_Population_2020) %>%
  mutate(est_total_lbs_2020 = Percapita_Pounds_Harvested*Community_Population_2020) %>% 
  mutate(est_total_kgs_2020 = est_total_lbs_2020*0.45359237) %>%
  filter(est_total_kgs_2020 != "0")

##Want to add the general groups as used in the original Tongass Figure 
df_4 <- df_3 %>%
  mutate(resource_cat = case_when(
    startsWith(Taxa_lvl3, "Berries") ~ "Berries",
    startsWith(Taxa_lvl1, "Birds") ~ "Birds/Eggs",
    startsWith(Taxa_lvl2, "Large Land Mammals") ~ "Large Land Mammals",
    startsWith(Taxa_lvl3, "Plants") ~"Plants/Greens/Mushrooms",
    startsWith(Taxa_lvl3, "Mushro") ~ "Plants/Greens/Mushrooms",
    startsWith(Taxa_lvl2, "Small") ~ "Small Land Mammals",
    startsWith(Taxa_lvl3, "Rabbit") ~ "Small Land Mammals",
    startsWith(Taxa_lvl3, "Char") ~ "Char",
    startsWith(Taxa_lvl2, "Salmon") ~ "Salmon",
    startsWith(Taxa_lvl3, "Smelt") ~ "Smelt/Whitefish",
    startsWith(Taxa_lvl3, "Whitefish") ~ "Smelt/Whitefish", 
    startsWith(Taxa_lvl3, "Trout") ~ "Trout",
    startsWith(Taxa_lvl4, "Herring Roe") ~ "Herring Roe",
    startsWith(Taxa_lvl4, "Dungeness Crab") ~ "Crab",
    startsWith(Taxa_lvl3, "Abalone") ~ "Mollusc", 
    startsWith(Taxa_lvl3, "Chiton") ~ "Mollusc", 
    startsWith(Taxa_lvl3, "Cockle") ~ "Mollusc", 
    startsWith(Taxa_lvl3, "Clam") ~ "Mollusc", 
    startsWith(Taxa_lvl3, "Limpet") ~ "Mollusc", 
    startsWith(Taxa_lvl3, "Mussel") ~ "Mollusc", 
    startsWith(Taxa_lvl3, "Snail") ~ "Mollusc", 
    startsWith(Taxa_lvl3, "Sea Cucumber") ~ "Other", 
    startsWith(Taxa_lvl3, "Sea Urchin") ~ "Other", 
    startsWith(Taxa_lvl3, "Starfish") ~ "Other", 
    startsWith(Taxa_lvl3, "Seaweed") ~ "Seaweed/Kelp",
    startsWith(Taxa_lvl3, "Halibut") ~"Halibut",
    grepl("Marine", Habitat) & grepl("Non-Salmon Fish", Taxa_lvl2) & !grepl("Halibut", Taxa_lvl3) ~ "Non-halibut Fish",
    grepl("Marine", Habitat) & startsWith(Taxa_lvl1, "Marine Invert") ~ "Marine Invertebrates",
    startsWith(Taxa_lvl1, "Marine Mammal") ~ "Marine Mammals",
  ))

test <- df_4 %>%
  filter(is.na(resource_cat))

###Sankey Fig where both forests are combined ----------------
##Calculate total harvest per resource category across all communities
df_sum <- df_4 %>%
  select(Habitat, resource_cat, est_total_kgs_2020) %>%
  group_by(Habitat, resource_cat) %>%
  summarise_at(vars(est_total_kgs_2020), list(total_harvest_kg_2020 = sum))

##calculate total harvest of all categories
total_harvest_all_kg <- sum(df_sum$total_harvest_kg_2020)

total_harvest_all_kg/1000

##calculate the total harvest for each category (terrestrial, anadromous, marine, nearshore)
total_harvest_cat <- df_sum %>%
  group_by(Habitat) %>%
  summarise_at(vars(total_harvest_kg_2020), sum) %>% ##calculate sum of  kg harvested by habitat
  dplyr::rename(cat_total_est_kg = total_harvest_kg_2020) 



##join total harvest by category back to df_2_res so can calculate percentage of harvest by category, and percent total harvest
df_2_res <- df_sum %>%
  left_join(total_harvest_cat, by= "Habitat") %>%
  mutate(percent_total_harvest_cat = (total_harvest_kg_2020/cat_total_est_kg)*100)%>%
  mutate(total_harvest_all_kg = total_harvest_all_kg) %>%
  mutate(percent_total_harvest_all = (total_harvest_kg_2020/total_harvest_all_kg)*100) %>%
  mutate(est_total_1000kg_2020 = total_harvest_kg_2020/1000) %>%
  rename(Category = "Habitat", Resource_Group = "resource_cat")

str(df_2_res)


###Create Sankey Diagram

# Make a connection data frame
df_sank <- df_2_res %>%
  select(Category, Resource_Group, percent_total_harvest_all)

df_sank_2 <- df_sank %>%
  group_by(Category) %>%
  mutate(percent_total_harvest_all = sum(percent_total_harvest_all))


links <- data.frame(
  source=c("All Harvest Species", "All Harvest Species", "All Harvest Species", "All Harvest Species", "","", "", "", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Freshwater (Anadromous)", "Freshwater (Anadromous)", "Freshwater (Anadromous)", "Freshwater (Anadromous)", "Nearshore", "Nearshore", "Nearshore", "Nearshore", "Nearshore", "Marine", "Marine", "Marine", "Marine"), 
  target=c("", "", "", "",  "Terrestrial","Freshwater (Anadromous)", "Nearshore", "Marine", "Berries", "Birds/Eggs", "Large Land Mammals", "Plants/Greens/Mushrooms", "Small Land Mammals", "Char", "Salmon", "Smelt/Whitefish", "Trout", "Herring Roe", "Molluscs", "Crabs", "Other", "Seaweed/Kelp", "Halibut", "Non-Halibut Fish", "Marine Invertebrates", "Marine Mammals" ), 
  value=c(23.954, 34.385, 9.764, 31.897, 23.954, 34.385, 9.764, 31.897,  4.707, 0.640, 17.942, 0.575, 0.092, 1.367, 31.596, 0.733, 0.688, 2.833, 3.015, 2.550, 0.361, 1.006, 15.643, 7.643, 6.517, 2.094)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# prepare color scale: I give one specific color for each node.
#my_color <- 'd3.scaleOrdinal() .domain(["All Species", "Terrestrial","Anadromous", "Nearshore", "Marine", "Berries", "Birds/Eggs", "Large Land Mammals", "Plants/Greens/Mushrooms", "Small Land Mammals", "Char", "Salmon", "Smelt", "Trout", "Herring Roe", "Molluscs", "Crabs", "Other", "Seaweed/Kelp", "Halibut", "Non-Halibut Fish", "Marine Invertebrates", "Marine Mammals"]) .range(["black", "green", "lightblue", "blue", "grey",  "green", "green", "green", "green", "green", "lightblue", "lightblue", "lightblue", "lightblue", "blue", "blue", "blue", "blue", "blue", "grey", "grey", "grey", "grey"])'


# Add a 'group' column to each connection:
links$group <- as.factor(c("type_a","type_b","type_c","type_d","type_a","type_b","type_c","type_d","type_a","type_a", "type_a", "type_a", "type_a", "type_b", "type_b", "type_b", "type_b", "type_c", "type_c", "type_c", "type_c", "type_c", "type_d", "type_d", "type_d", "type_d"))

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
nodes$group <- as.factor(c("a","b","c","d","e","f", "c","c","c", "c", "c", "d", "d", "d", "d", "e", "e", "e", "e", "e", "f", "f", "f", "f"))

# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "type_c", "type_d", "a", "b", "c", "d", "e", "f"]) .range(["#66CC66", "#99FFCC", "#66CCFF", "#CCCCCC", "#000000", "#FFFFFF", "#006600", "#66CC99", "#3399CC", "#999999"])'

my_color_2 <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "type_c", "type_d", "a", "b", "c", "d", "e", "f"]) .range(["#006600", "#FF6699", "#CC9933", "#0066CC", "#000000", "#FFFFFF", "#006600", "#FF6699", "#CC9933", "#0066CC"])'

my_color_3 <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "type_c", "type_d", "a", "b", "c", "d", "e", "f"]) .range(["#339933", "#FF9999", "#33CCFF", "#003366", "#000000", "#FFFFFF", "#339933", "#FF9999", "#33CCFF", "#003366"])'

##this is the colour palette using
my_color_4 <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "type_c", "type_d", "a", "b", "c", "d", "e", "f"]) .range(["#339933", "#FF9999", "#CC9966", "#003366", "#000000", "#FFFFFF", "#339933", "#FF9999", "#CC9966", "#003366"])'

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", colourScale = my_color_4, LinkGroup = "group", NodeGroup = "group", fontSize = 13, nodePadding = 23)
p

nodes$name2 <- ""
p2 <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name2", colourScale = my_color_4, LinkGroup = "group", NodeGroup = "group", fontSize = 13, nodePadding = 23)
p2

###Seeing if I can separate Chugach and Tongass 

##Sankey Fig where both forests start separate ------------
##Calculate total harvest per resource category across all communities
df_sum <- df_4 %>%
  select(Habitat, resource_cat, est_total_kgs_2020) %>%
  group_by(Habitat, resource_cat) %>%
  summarise_at(vars(est_total_kgs_2020), list(total_harvest_kg_2020 = sum))


##Calculate total harvest of chugach and tongass
forest_sum <- df_4 %>%
  ungroup() %>%
  group_by(Forest) %>%
  summarise_at(vars(est_total_kgs_2020), list(forest_total_harvest_kg_2020 = sum))

forest_habitat_sum <- df_4 %>%
  group_by(Forest, Habitat) %>%
  summarise_at(vars(est_total_kgs_2020), list(forest_habitat_total_harvest_kg_2020 = sum)) %>%
  left_join(forest_sum, by = "Forest") %>%
  mutate(prop_habitat_forest = (forest_habitat_total_harvest_kg_2020/total_harvest_all_kg)*100)

##calculate total harvest of all categories
total_harvest_all_kg <- sum(df_sum$total_harvest_kg_2020)

total_harvest_all_kg/1000

##calculate the total harvest for each category (terrestrial, anadromous, marine, nearshore) 
total_harvest_cat <- df_sum %>%
  group_by(Habitat) %>%
  summarise_at(vars(total_harvest_kg_2020), sum) %>% ##calculate sum of  kg harvested by habitat
  dplyr::rename(cat_total_est_kg = total_harvest_kg_2020) 



##join total harvest by category back to df_2_res so can calculate percentage of harvest by category, and percent total harvest
df_2_res <- df_sum %>%
  left_join(total_harvest_cat, by= "Habitat") %>%
#  left_join(forest_total_harvest_cat, by = c("Forest", "Habitat")) %>%
#  mutate(percent_total_harvest_cat = (total_harvest_kg_2020/cat_total_est_kg)*100)%>%
#  mutate(percent_total_harvest_cat_forest = (total_harvest_kg_2020/forest_cat_total_est_kg)*100) %>%
  mutate(total_harvest_all_kg = total_harvest_all_kg) %>%
  mutate(percent_total_harvest_all = (total_harvest_kg_2020/total_harvest_all_kg)*100) %>%
  mutate(est_total_1000kg_2020 = total_harvest_kg_2020/1000) %>%
  rename(Category = "Habitat", Resource_Group = "resource_cat")

str(df_2_res)


###Create Sankey Diagram

# Make a connection data frame
df_sank <- df_2_res %>%
  select(Category, Resource_Group, percent_total_harvest_all)

df_sank_2 <- df_sank %>%
  group_by(Category) %>%
  mutate(percent_total_harvest_all = sum(percent_total_harvest_all))


links <- data.frame(
  source=c("Chugach1", "Chugach1", "Chugach1", "Chugach1", "Tongass1", "Tongass1", "Tongass1", "Tongass1","Chugach", "Chugach", "Chugach", "Chugach", "Tongass", "Tongass", "Tongass", "Tongass","all species terrestrial", "all species freshwater", "all species nearshore", "all species marine", "", "", "","", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Freshwater (Anadromous)", "Freshwater (Anadromous)", "Freshwater (Anadromous)", "Freshwater (Anadromous)", "Nearshore", "Nearshore", "Nearshore", "Nearshore", "Nearshore", "Marine", "Marine", "Marine", "Marine"), 
  target=c("Chugach", "Chugach", "Chugach", "Chugach", "Tongass", "Tongass", "Tongass", "Tongass", "all species terrestrial","all species freshwater","all species nearshore","all species marine", "all species terrestrial", "all species freshwater", "all species nearshore", "all species marine", "", "", "", "", "Terrestrial", "Freshwater (Anadromous)", "Nearshore", "Marine",  "Berries", "Birds/Eggs", "Large Land Mammals", "Plants/Greens/Mushrooms", "Small Land Mammals", "Char", "Salmon", "Smelt/Whitefish", "Trout", "Herring Roe", "Molluscs", "Crabs", "Other", "Seaweed/Kelp", "Halibut", "Non-Halibut Fish", "Marine Invertebrates", "Marine Mammals" ), 
  value=c(5.662, 9.643,0.428, 4.829, 18.292, 24.742, 9.336, 27.068, 5.662, 9.643,0.428, 4.829, 18.292, 24.742, 9.336, 27.068,23.954, 34.385, 9.764, 31.897, 23.954, 34.385, 9.764, 31.897,  4.707, 0.640, 17.942, 0.575, 0.092, 1.367, 31.596, 0.733, 0.688, 2.833, 3.015, 2.550, 0.361, 1.006, 15.643, 7.643, 6.517, 2.094)
)

test <- data.frame(
  source=c("Chugach", "Chugach", "Chugach", "Chugach", "Tongass", "Tongass", "Tongass", "Tongass", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Freshwater (Anadromous)", "Freshwater (Anadromous)", "Freshwater (Anadromous)", "Freshwater (Anadromous)", "Nearshore", "Nearshore", "Nearshore", "Nearshore", "Nearshore", "Marine", "Marine", "Marine", "Marine"), 
  target=c( "Terrestrial","Freshwater (Anadromous)", "Nearshore", "Marine","Terrestrial","Freshwater (Anadromous)", "Nearshore", "Marine", "Berries", "Birds/Eggs", "Large Land Mammals", "Plants/Greens/Mushrooms", "Small Land Mammals", "Char", "Salmon", "Smelt/Whitefish", "Trout", "Herring Roe", "Molluscs", "Crabs", "Other", "Seaweed/Kelp", "Halibut", "Non-Halibut Fish", "Marine Invertebrates", "Marine Mammals" ) 
 # value=c(27.537, 46.898,2.081, 23.483, 23.027, 31.146, 11.752, 34.074,  23.954, 34.385, 9.764, 31.897, 23.954, 34.385, 9.764, 31.897,  4.707, 0.640, 17.942, 0.575, 0.092, 1.367, 31.596, 0.733, 0.688, 2.833, 3.015, 2.550, 0.361, 1.006, 15.643, 7.643, 6.517, 2.094)
)
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# prepare color scale: I give one specific color for each node.
#my_color <- 'd3.scaleOrdinal() .domain(["All Species", "Terrestrial","Anadromous", "Nearshore", "Marine", "Berries", "Birds/Eggs", "Large Land Mammals", "Plants/Greens/Mushrooms", "Small Land Mammals", "Char", "Salmon", "Smelt", "Trout", "Herring Roe", "Molluscs", "Crabs", "Other", "Seaweed/Kelp", "Halibut", "Non-Halibut Fish", "Marine Invertebrates", "Marine Mammals"]) .range(["black", "green", "lightblue", "blue", "grey",  "green", "green", "green", "green", "green", "lightblue", "lightblue", "lightblue", "lightblue", "blue", "blue", "blue", "blue", "blue", "grey", "grey", "grey", "grey"])'


# Add a 'group' column to each connection:
links$group <- as.factor(c("type_a","type_b","type_c","type_d", "type_a","type_b","type_c","type_d", "type_a","type_b","type_c","type_d", "type_a","type_b","type_c","type_d", "type_a","type_b","type_c","type_d","type_a","type_b","type_c","type_d","type_a","type_a", "type_a", "type_a", "type_a", "type_b", "type_b", "type_b", "type_b", "type_c", "type_c", "type_c", "type_c", "type_c", "type_d", "type_d", "type_d", "type_d"))

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
nodes$group <- as.factor(c("a", "a", "a", "a", "a", "a", "b", "a","b","c","d","e","f", "c","c","c", "c", "c", "d", "d", "d", "d", "e", "e", "e", "e", "e", "f", "f", "f", "f"))

# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "type_c", "type_d", "a", "b", "c", "d", "e", "f"]) .range(["#66CC66", "#99FFCC", "#66CCFF", "#CCCCCC", "#000000", "#FFFFFF", "#006600", "#66CC99", "#3399CC", "#999999"])'

my_color_2 <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "type_c", "type_d", "a", "b", "c", "d", "e", "f"]) .range(["#006600", "#FF6699", "#CC9933", "#0066CC", "#000000", "#FFFFFF", "#006600", "#FF6699", "#CC9933", "#0066CC"])'

my_color_3 <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "type_c", "type_d", "a", "b", "c", "d", "e", "f"]) .range(["#339933", "#FF9999", "#33CCFF", "#003366", "#000000", "#FFFFFF", "#339933", "#FF9999", "#33CCFF", "#003366"])'

##this is the colour palette using
my_color_4 <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "type_c", "type_d", "a", "b", "c", "d", "e", "f"]) .range(["#339933", "#FF9999", "#CC9966", "#003366", "#000000", "#FFFFFF", "#339933", "#FF9999", "#CC9966", "#003366"])'

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", colourScale = my_color_4, LinkGroup = "group", NodeGroup = "group", fontSize = 13, nodePadding = 23)
p

nodes$name2 <- ""
p2 <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name2", colourScale = my_color_4, LinkGroup = "group", NodeGroup = "group", fontSize = 13, nodePadding = 23)
p2


##
##Calculate servings harvested 

##Based on USDA recommendations of 150g/serving for fruits/veg and 99g/serving for meat 

##total rural population in 2020
pop_df$Community_Population <- as.numeric(pop_df$Community_Population)

pop_df2 <- df_4 %>%
  select(Forest, Site_Year_Code, Community_Population_2020) %>%
  distinct()


total_pop <- sum(pop_df2$Community_Population_2020)

##comparing population size in tongass and chugach
test <- df_4 %>%
  select(Forest, Site_Year_Code, Community_Population_2020) %>%
  distinct() %>%
  group_by(Forest) %>%
  summarise_at(vars(Community_Population_2020), list(sum))

fruit_veg <- df_2_res %>%
  filter(Resource_Group %in% c("Seaweed", "Berries", "Plants/Greens/Mushrooms")) %>%
  select(Resource_Group, total_harvest_kg_2020) %>%
  mutate(servings = total_harvest_kg_2020/0.150) #%>%
  
  


meat <- df_2_res %>%
  filter(!Resource_Group %in% c("Seaweed", "Berries", "Plants/Greens/Mushrooms"))%>%
  select(Resource_Group, total_harvest_kg_2020) %>%
  mutate(servings = total_harvest_kg_2020/0.156) 

servings_all<- rbind(fruit_veg, meat) %>%
  mutate(servings_per_person = servings/total_pop) %>%
  mutate(servings_mill = servings/1000)

servings_all_sum <-rbind(fruit_veg, meat)  %>%
  ungroup() %>%
  summarise_at(vars(servings), list(sum)) %>%
  mutate(servings_per_person = servings/total_pop)




mutate(servings_mill = est_total_1000kg_2022/0.156) %>%

mutate(servings_per_person = servings/total_pop) 




##Calculate % of harvest that is directly and indirectly supported by forest
##direct: terrestrial, freshwater, nearshore



direct_prop <- df_2_res %>%
  filter(Category != "Marine") %>%
  ungroup() %>%
  summarise_at(vars(percent_total_harvest_all), list(sum))


indirect_prop <- df_2_res %>%
  filter(Category == "Marine") %>%
  ungroup() %>%
  summarise_at(vars(percent_total_harvest_all), list(sum))

