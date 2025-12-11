##Community Specific Sankey Figures 
##September 23, 2025
##Marie Gutgesell 


library(readxl)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(data.table)
library(networkD3)

##Set up dataframe of latest surveys, and 2020 harvest estimates ---------------

##Read in estimated population size data -- latest year I have is 2020 for all communities in Chugach and Tongass
pop_df <- read_excel("data/CSIS_Community_Demographics.xlsx", sheet = 1) %>%
  filter(Year == 2020) %>%
  select(Community:Community_Population)

pop_df$Community_Population <- as.numeric(pop_df$Community_Population)

total_pop_2020 <- pop_df %>%
  filter(!is.na(Community_Population)) %>%
  summarise_at(vars(Community_Population), list(sum))

##Read in cleaned Tongass and Chugach data -- then will select most representative year
c_df <- read.csv("data/intermediate_data/chugach_harvest_data_clean.csv") %>%
  mutate(Forest = "Chugach")
t_df <- read.csv("data/intermediate_data/tongass_harvest_data_clean.csv") %>%
  mutate(Forest = "Tongass")
df <- rbind(t_df, c_df) %>%
  select(Forest, Project_Name:Mean_Grams_Percapita_Harvest) %>%
  separate(Site_Year_Code, into = c("Community", "Harvest_Survey_Year"), sep = "_", remove = FALSE)


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



##testing large land mammals 
llm <- df_3 %>%
  filter(Taxa_lvl2 == "Large Land Mammals") %>%
  summarise_at(vars(est_total_kgs_2020), list(sum)) %>%
  mutate(est_total_mt_2020 = est_total_kgs_2020/1000)

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
    startsWith(Taxa_lvl3, "Smelt") ~ "Other Freshwater/Anadromous Fish",
    startsWith(Taxa_lvl3, "Whitefish") ~ "Other Freshwater/Anadromous Fish", 
    startsWith(Taxa_lvl3, "Smelt") ~ "Other Freshwater/Anadromous Fish",
    startsWith(Taxa_lvl3, "Pike") ~ "Other Freshwater/Anadromous Fish",
    startsWith(Taxa_lvl3, "Burbot") ~ "Other Freshwater/Anadromous Fish",
    startsWith(Taxa_lvl3, "Sturgeon") ~ "Other Freshwater/Anadromous Fish",
    startsWith(Taxa_lvl3, "Sheefish") ~ "Other Freshwater/Anadromous Fish",
    startsWith(Taxa_lvl3, "Lamprey") ~ "Other Freshwater/Anadromous Fish",
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


##Sankey Figure for Cordova ------------
##Calculate total harvest per resource category across all communities
cordova <- df_4 %>%
  filter(Site_Year_Code == "Cordova_2014")

df_sum <- df_4 %>%
  filter(Site_Year_Code == "Cordova_2014") %>%
  select(Habitat, resource_cat, est_total_lbs_2020) %>%
  group_by(Habitat, resource_cat) %>%
  summarise_at(vars(est_total_lbs_2020), list(total_harvest_lb_2020 = sum)) %>%
  mutate(harvest_1000_lbs = total_harvest_lb_2020/1000)

##calculate total harvest of all categories
total_harvest_all_lb <- sum(df_sum$total_harvest_lb_2020)

total_harvest_all_lb/1000

##calculate the total harvest for each category (terrestrial, anadromous, marine, nearshore)
total_harvest_cat <- df_sum %>%
  group_by(Habitat) %>%
  summarise_at(vars(total_harvest_lb_2020), sum) %>% ##calculate sum of  lb harvested by habitat
  dplyr::rename(cat_total_est_lb = total_harvest_lb_2020) 

##join total harvest by category back to df_2_res so can calculate percentage of harvest by category, and percent total harvest
df_2_res <- df_sum %>%
  left_join(total_harvest_cat, by= "Habitat") %>%
  mutate(percent_total_harvest_cat = (total_harvest_lb_2020/cat_total_est_lb)*100)%>%
  mutate(total_harvest_all_lb = total_harvest_all_lb) %>%
  mutate(percent_total_harvest_all = (total_harvest_lb_2020/total_harvest_all_lb)*100) %>%
  mutate(est_total_1000lb_2020 = total_harvest_lb_2020/1000) %>%
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
  source=c("All Harvest Species", "All Harvest Species", "All Harvest Species", "All Harvest Species", "","", "", "", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Freshwater (Anadromous)", "Freshwater (Anadromous)", "Freshwater (Anadromous)", "Freshwater (Anadromous)", "Nearshore", "Nearshore", "Nearshore", "Nearshore", "Marine", "Marine", "Marine"), 
  target=c("", "", "", "",  "Terrestrial","Freshwater (Anadromous)", "Nearshore", "Marine", "Berries", "Birds/Eggs", "Large Land Mammals", "Plants/Greens/Mushrooms", "Small Land Mammals", "Char","Other Freshwater/Anadromous Fish", "Salmon",  "Trout", "Herring Roe", "Molluscs", "Crabs", "Seaweed/Kelp", "Halibut", "Non-Halibut Fish", "Marine Invertebrates"), 
  value=c(44.516, 38.465, 1.094, 15.924, 44.516, 38.465, 1.094, 15.924,  8.220, 1.276, 34.439, 0.276, 0.305, 0.148,0.396, 37.701,  0.220, 0.317, 0.202, 0.0527, 0.523, 12.455, 1.738, 1.732)
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
links$group <- as.factor(c("type_a","type_b","type_c","type_d","type_a","type_b","type_c","type_d","type_a","type_a", "type_a", "type_a", "type_a", "type_b", "type_b", "type_b", "type_b", "type_c", "type_c", "type_c", "type_c",  "type_d", "type_d", "type_d"))

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
nodes$group <- as.factor(c("a","b","c","d","e","f", "c","c","c", "c", "c", "d", "d", "d", "d", "e", "e", "e", "e",  "f", "f", "f"))

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



##testing 
