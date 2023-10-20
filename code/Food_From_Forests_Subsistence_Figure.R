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
  select(Site_Year_Code, Category, Resource_Group, Resource_Name, Estimated_Total_Pounds_Harvested) %>%
  mutate(estimated_total_kg = Estimated_Total_Pounds_Harvested*0.45359237) %>%
  group_by(Category, Resource_Group) %>%
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
  arrange(desc(Resource_Group)) %>%
  mutate(pos_2 = cumsum(estimated_total_kg) - estimated_total_kg/2) %>%
  ggplot(aes(x= "", y = estimated_total_kg, fill = Resource_Group)) +
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
  arrange(desc(Resource_Group)) %>%
  mutate(pos_2 = cumsum(estimated_total_kg) - estimated_total_kg/2) %>%
  ggplot(aes(x= "", y = estimated_total_kg, fill = Resource_Group)) +
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
  arrange(desc(Resource_Group)) %>%
  mutate(pos_2 = cumsum(estimated_total_kg) - estimated_total_kg/2) %>%
  ggplot(aes(x= "", y = estimated_total_kg, fill = Resource_Group)) +
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

df_2_res_sankey <- df_2_res %>% 
  select(Category, Resource_Name, estimated_total_kg) %>%
  make_long(Category, Resource_Name)


ggplot(df_2_res_sankey, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node))) +
  geom_sankey() +
  theme_sankey(base_size = 16)


df_sank <- df_2_res %>%
  select(Category, Resource_Group, percent_total_harvest)


library(networkD3)
nodes = data.frame("name" = 
                     c("All_Species", # Node 0
                       "Terrestrial", # Node 1
                       "Anadromous", # Node 2
                       "Nearshore",#3
                       "Marine", #4
                       "Berries", #5
                       "Birds_Eggs", #6
                       "Large_land_mammals", #7
                       "Plants_Greens_Mushrooms", #8
                       "Small_land_Mammals", #9
                       "Char", #10
                       "Salmon", #11
                       "Smelt", #12
                       "Trout", #13
                       "Herring_Roe", #14
                       "Molluscs", #15
                       "Crabs", #16
                       "Other", #17
                       "Seaweed", #18
                       "Halibut", #19
                       "Non-Halibut_Fish", #20
                       "Marine Invertebrates", #21
                       "Marine Mammals" #22
                       ))# Node 3
links = as.data.frame(matrix(c(
  0, 1, 22, # Each row represents a link. The first number
  0, 2, 30, # represents the node being conntected from. 
  0, 3, 11, # the second number represents the node connected to.
  0, 4, 37,
  1, 5, 20,
  1, 6, 2,
  1, 7, 75,
  1, 8, 3,
  1, 9, 0.3,
  2, 10, 4, 
  2, 11, 91, 
  2, 12, 3, 
  2, 13, 2,
  3, 14, 29,
  3, 15, 30, 
  3, 16, 26, 
  3, 17, 5,
  3, 18, 10,
  4, 19, 46,
  4, 20, 24,
  4, 21, 22,
  4, 22, 8),# The third number is the value of the node
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")

# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal() .domain(["All_Species", # Node 0
                       "Terrestrial", # Node 1
                       "Anadromous", # Node 2
                       "Nearshore",#3
                       "Marine", #4
                       "Berries", #5
                       "Birds_Eggs", #6
                       "Large_land_mammals", #7
                       "Plants_Greens_Mushrooms", #8
                       "Small_land_Mammals", #9
                       "Char", #10
                       "Salmon", #11
                       "Smelt", #12
                       "Trout", #13
                       "Herring_Roe", #14
                       "Molluscs", #15
                       "Crabs", #16
                       "Other", #17
                       "Seaweed", #18
                       "Halibut", #19
                       "Non-Halibut_Fish", #20
                       "Marine Invertebrates", #21
                       "Marine Mammals"]) .range(["black", "green", "lightblue", "blue", "grey",  "green", "green", "green", "green", "green", "lightblue", "lightblue", "lightblue", "lightblue", "blue", "blue", "blue", "blue", "blue", "grey", "grey", "grey", "grey"])'
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30, colourScale = my_color)


##trying it again.. 
# Make a connection data frame
links <- data.frame(
  source=c("All Harvest Species","All Harvest Species", "All Harvest Species", "All Harvest Species", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Anadromous", "Anadromous", "Anadromous", "Anadromous", "Nearshore", "Nearshore", "Nearshore", "Nearshore", "Nearshore", "Marine", "Marine", "Marine", "Marine"), 
  target=c("Terrestrial","Anadromous", "Nearshore", "Marine", "Berries", "Birds/Eggs", "Large Land Mammals", "Plants/Greens/Mushrooms", "Small Land Mammals", "Char", "Salmon", "Smelt", "Trout", "Herring Roe", "Molluscs", "Crabs", "Other", "Seaweed/Kelp", "Halibut", "Non-Halibut Fish", "Marine Invertebrates", "Marine Mammals" ), 
  value=c(22, 30, 11, 37, 20, 2, 75, 3, 0.3, 4, 91, 3, 2, 29, 30, 26, 5, 10, 46, 24, 22, 8)
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
links$group <- as.factor(c("type_a","type_b","type_c","type_d","type_a","type_a", "type_a", "type_a", "type_a", "type_b", "type_b", "type_b", "type_b", "type_c", "type_c", "type_c", "type_c", "type_c", "type_d", "type_d", "type_d", "type_d"))

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
nodes$group <- as.factor(c("a","b","c","d","e","b","b","b", "b", "b", "c", "c", "c", "c", "d", "d", "d", "d", "d", "e", "e", "e", "e"))

# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "type_c", "type_d", "a", "b", "c", "d", "e"]) .range(["#66CC66", "#99FFCC", "#66CCFF", "#CCCCCC", "#000000", "#006600", "#66CC99", "#3399CC", "#999999"])'




# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", colourScale = my_color, LinkGroup = "group", NodeGroup = "group", fontSize = 13)
p
