##Food from Forests Project 
##Calculating estimated harvest for 2022 and making figure for manuscript


library(readxl)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(data.table)
library(networkD3)


####Total Harvest Data estimated for 2022 by Lauren (note: want to to all calculations raw, will have to read in population by year)
##read in terrestrial sheet in order to obtain 2022 population
terr <- read_excel("data/Foods_From_Forests_data/Copy of Southeast harvest summary_mg.xlsx", sheet = "Terrestrial") %>%
  select(Community, Study_Year, Per_capita_lb, `2022_population`, Estimated_total_lb) %>%
  mutate(Category = "Terrestrial")


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
  select(Site_Year_Code, Community, Year, `2022_population`, Est_Comm_Population)

##reduce dataframe to only focus on years/sites using for this project 
df_2 <- df %>%
  dplyr::filter(Site_Year_Code %in% latest_surveys$Site_Year_Code) %>% ##selects only years where a comprehensive survey was done
  dplyr::filter(!grepl("Marine Mammals", Project_Name))%>%
  dplyr::rename(Community = "Community_Name") %>%
  left_join(latest_surveys, by = c("Site_Year_Code", "Community"))


##select resource groups using for this study
resource_list <- read_excel("Foods_From_Forests_data/Copy of Southeast harvest summary_mg.xlsx", sheet = "Resource_List")

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

2055509.31/1000
641840.9 + 241324.9 + 476944.9
1360111/1000

1360111/2055509.31
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

##other nearshore organisms (crab, molluscs, other, seaweed/kelp)
(66720.331 + 69817.353 +9683.907 + 24259.910)/1000
((66720.331 + 69817.353 +9683.907 + 24259.910)/2055509.31)*100

##other terrestrial organisms (plants/fungi, birds/eggs, small land mammals)
(10441.739 + 12186.446 + 1404.958)/1000
((10441.739 + 12186.446 + 1404.958)/2055509.31)*100

##other freshwater species
(25546.420 + 19797.260 + 14484.445)/1000
((25546.420 + 19797.260 + 14484.445)/2055509.31)*100

###Create Sankey Diagram

# Make a connection data frame
df_sank <- df_2_res %>%
  select(Category, Resource_Group, percent_total_harvest_all)

df_sank_2 <- df_sank %>%
  group_by(Category) %>%
  mutate(percent_total_harvest_all = sum(percent_total_harvest_all))


links <- data.frame(
  source=c("All Harvest Species", "All Harvest Species", "All Harvest Species", "All Harvest Species", "","", "", "", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Freshwater (Anadromous)", "Freshwater (Anadromous)", "Freshwater (Anadromous)", "Freshwater (Anadromous)", "Nearshore", "Nearshore", "Nearshore", "Nearshore", "Nearshore", "Marine", "Marine", "Marine", "Marine"), 
  target=c("", "", "", "",  "Terrestrial","Freshwater (Anadromous)", "Nearshore", "Marine", "Berries", "Birds/Eggs", "Large Land Mammals", "Plants/Greens/Mushrooms", "Small Land Mammals", "Char", "Salmon", "Smelt", "Trout", "Herring Roe", "Molluscs", "Crabs", "Other", "Seaweed/Kelp", "Halibut", "Non-Halibut Fish", "Marine Invertebrates", "Marine Mammals" ), 
  value=c(23.203, 31.225, 11.740, 33.831, 23.203, 31.225, 11.740, 33.831, 4.678, 0.508, 17.356, 0.593, 0.068, 1.243, 28.314, 0.963, 0.705, 3.447, 3.397, 3.246, 0.471, 1.180, 15.771, 8.153, 7.581, 2.326)
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



##comparing population size in 2022 vs. the year the harvest was done
pop_change <- latest_surveys %>%
  mutate(pop_change = `2022_population` - Est_Comm_Population) %>%
  mutate(percent_change = (pop_change/Est_Comm_Population)*100)

pop_change_plot <- ggplot(pop_change, aes(x = Community, y = percent_change)) +
  geom_col() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x = element_blank(),  text = element_text(family = "Times New Roman"), strip.background = element_blank()) 

  
pop_change_plot
