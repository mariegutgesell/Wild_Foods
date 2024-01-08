###Visualizing food webs -- testing with Sankey Diagram Approach


library(readxl)

##import cleaned harvest data and trophic info
setwd("~/Desktop/Wild Foods Repo/")
source("code/1_dataframe_formation.R")
rm(list = ls()[!ls() %in% c("df_final")])
trophic_df <- read_excel("data/harvest_species_list_characteristics_5.xlsx")

##remove unknown and filter to only species harvested in harvest data, select only columns needed for food web interaction, then join trophic data
##note: in future will likely recalculate the harvest metrics, but lets not worry about that now -- also still need to fix those where conversion unit is 0 but did harvest resources
harvest_df <- df_final %>%
  select(Site_Year_Code, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Percapita_Pounds_Harvested, Estimated_Total_Pounds_Harvested) %>%
  filter(!if_all(Percapita_Pounds_Harvested:Estimated_Total_Pounds_Harvested, ~ .x == 0))

harvest_df$Taxa_lvl5 <- str_remove(harvest_df$Taxa_lvl5, "Unknown ")
harvest_df$Taxa_lvl4 <- str_remove(harvest_df$Taxa_lvl4, "Unknown ")

df <- harvest_df %>%
  left_join(trophic_df %>% select(Taxa_lvl4, Taxa_lvl5, Scientific_Name, Lowest_Taxonomic_Resolution, Habitat, Trophic_Level, Trophic_Category), by = c("Taxa_lvl4", "Taxa_lvl5")) %>%
  select(Site_Year_Code, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Scientific_Name, Lowest_Taxonomic_Resolution, Habitat, Trophic_Level, Trophic_Category,Percapita_Pounds_Harvested, Estimated_Total_Pounds_Harvested)

str(df)

##ryan wants to visualize the food web with the categories summed/avged by the general classifications as used in the food from forests manuscript (at least for the community engagement meeting)
##so need to add a column that gives that category level, and then calculate sum and avg trophic position

##generate column with upper level category -- based on Resource_List category in "Copy of Southeast harvest summary_mg.xlsx"
df_fwv <- df %>%
  mutate(General_Category = case_when(
    startsWith(Taxa_lvl2, "Salmon") ~ "Salmon",
    startsWith(Taxa_lvl3, "Smelt") ~ "Smelt",
    startsWith(Taxa_lvl3, "Char") ~ "Char",
    startsWith(Taxa_lvl3, "Trout") ~ "Trout",
    startsWith(Taxa_lvl3, "Sturgeon") ~ "Sturgeon",
    startsWith(Taxa_lvl1, "Birds") ~ "Birds/Eggs",
    startsWith(Taxa_lvl2, "Large Land") ~ "Large Land Mammals",
    startsWith(Taxa_lvl2, "Small Land") ~ "Small Land Mammals",
    startsWith(Taxa_lvl3, "Berries") ~ "Berries",
    startsWith(Taxa_lvl3, "Plants") ~ "Plants/Greens/Mushrooms",
    startsWith(Taxa_lvl3, "Mushrooms") ~ "Plants/Greens/Mushrooms",
    startsWith(Taxa_lvl3, "Seaweed") ~ "Seaweed/Kelp",
    startsWith(Taxa_lvl4, "Herring Roe") ~ "Herring Roe",
    startsWith(Taxa_lvl3, "Abalone") ~ "Mollusc",
    startsWith(Taxa_lvl3, "Chiton") ~ "Mollusc",
    startsWith(Taxa_lvl3, "Clam") ~ "Mollusc",
    startsWith(Taxa_lvl3, "Cockle") ~ "Mollusc",
    startsWith(Taxa_lvl4, "Geoduck") ~ "Mollusc",
    startsWith(Taxa_lvl3, "Limpet") ~ "Mollusc",
    startsWith(Taxa_lvl3, "Mussel") ~ "Mollusc",
    startsWith(Taxa_lvl4, "Dungeness") ~ "Crab",
    startsWith(Taxa_lvl3, "Sea Cucumber") ~ "Other",
    startsWith(Taxa_lvl3, "Sea Urchin") ~ "Other",
    startsWith(Taxa_lvl3, "Halibut") ~ "Halibut",
    startsWith(Taxa_lvl3, "Gadiform") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl3, "Hexagrammidae") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl3, "Flounder") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl4, "Herring") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl3, "Flounder") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl3, "Rockfish") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl3, "Sablefish") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl3, "Sculpin") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl3, "Skates") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl3, "Shark") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl3, "Sole") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl3, "Flounder") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl3, "Eel") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl3, "Perch") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl3, "Bass") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl3, "Unknown Non-Salmon") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl3, "Grayling") ~ "Non-Halibut Fish",
    startsWith(Taxa_lvl4, "King Crab") ~ "Marine Invertebrates",
    startsWith(Taxa_lvl3, "Octopus") ~ "Marine Invertebrates",
    startsWith(Taxa_lvl3, "Scallop") ~ "Marine Invertebrates",
    startsWith(Taxa_lvl3, "Shrimp") ~ "Marine Invertebrates",
    startsWith(Taxa_lvl4, "Tanner Crab") ~ "Marine Invertebrates",
    startsWith(Taxa_lvl4, "Box Crab") ~ "Marine Invertebrates",
    startsWith(Taxa_lvl4, "Unknown Crab") ~ "Marine Invertebrates",
    startsWith(Taxa_lvl3, "Squid") ~ "Marine Invertebrates",
    startsWith(Taxa_lvl3, "Oyster") ~ "Marine Invertebrates",
    startsWith(Taxa_lvl3, "Unknown Marine Invertebrates") ~ "Marine Invertebrates",
    startsWith(Taxa_lvl3, "Crab") ~ "Marine Invertebrates", ##for the 3 rows where crab is lowest level of taxonomic resolution 
    startsWith(Taxa_lvl1, "Marine Mammals") ~ "Marine Mammals",
  ))
str(df_fwv)


##want to calculate average trophic position per "group" and the total per capita harvest?
df_fwv_meantl <- df_fwv %>%
  filter(!is.na(Trophic_Level)) %>% ##for now, have to remove this NA because still need to determine trophic level of eel and shark (when sp not known, waiting to talk to lauren)
  group_by(Site_Year_Code, Habitat, General_Category) %>%
  summarise_at(vars(Trophic_Level), list(mean_trophic_level = mean))

df_fwv_sum <- df_fwv %>%
  group_by(Site_Year_Code, Habitat, General_Category) %>%
  summarise_at(vars(Percapita_Pounds_Harvested), list(total_percapitalb_harvested = sum)) %>%
  left_join(df_fwv_meantl, by = c("Site_Year_Code", "Habitat", "General_Category"))


##want to visualize two communities that may have quite different harvest structure

##based on preliminary cluster analysis -- starting with edna bay 1987 and skagway 1987 
ednabay_1987 <- df_fwv_sum %>%
  filter(Site_Year_Code == "Edna Bay_1987")

skagway_1987 <- df_fwv_sum %>%
  filter(Site_Year_Code == "Skagway_1987")

##Trying to visualize food web first with sankey diagram 

###Create Sankey Diagram
library(networkD3)
##First for edna bay
##trying first with percapita harvest, but maybe want to use percent of total harvest? want to think about best way, something closer to harvest structure or food web structure IS? 

##testing first w/ just terrestrial to see if we can get it working
eb_links <- data.frame(
  source = c("Berries", "Berries", "Plants/Greens/Mushrooms", "Plants/Greens/Mushrooms", "Plants/Greens/Mushrooms", "Birds/Eggs", "Large Land Mammals"),
  target = c("Human", "Large Land Mammals", "Human", "Birds/Eggs", "Large Land Mammals", "Human", "Human"),
  value = c(13.51, 1, 11.43, 1, 1, 4.43, 147.02)
)


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(eb_links$source), as.character(eb_links$target)) %>% 
    unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
eb_links$IDsource <- match(eb_links$source, nodes$name)-1 
eb_links$IDtarget <- match(eb_links$target, nodes$name)-1

# prepare color scale: I give one specific color for each node.
#my_color <- 'd3.scaleOrdinal() .domain(["All Species", "Terrestrial","Anadromous", "Nearshore", "Marine", "Berries", "Birds/Eggs", "Large Land Mammals", "Plants/Greens/Mushrooms", "Small Land Mammals", "Char", "Salmon", "Smelt", "Trout", "Herring Roe", "Molluscs", "Crabs", "Other", "Seaweed/Kelp", "Halibut", "Non-Halibut Fish", "Marine Invertebrates", "Marine Mammals"]) .range(["black", "green", "lightblue", "blue", "grey",  "green", "green", "green", "green", "green", "lightblue", "lightblue", "lightblue", "lightblue", "blue", "blue", "blue", "blue", "blue", "grey", "grey", "grey", "grey"])'


# Add a 'group' column to each connection:
eb_links$group <- as.factor(c("terr", "terr", "terr", "terr", "terr", "terr", "terr"))

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
nodes$group <- as.factor(c("t", "t", "t", "t", "t"))

# Give a color for each group:
##this is the colour palette using
my_color_4 <- 'd3.scaleOrdinal() .domain(["terr", "t"]) .range(["#339933", "#339933"])'

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = eb_links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", colourScale = my_color_4, LinkGroup = "group", NodeGroup = "group", fontSize = 13, nodePadding = 23)
p

##i am not sure the sankey diagram is going to work



##testing to see how other network visualization approach works when we have it summarized this way


##need to generate interaction matrix 
mat <- ednabay_1987 %>%
  dplyr::rename(Resource = "General_Category") %>%
  mutate(Consumer = "Human") 

is1 <- mat %>%
  ungroup() %>%
  select(Resource,  Consumer, total_percapitalb_harvested)

tl <- mat %>%
  ungroup() %>%
  select(Resource, mean_trophic_level, Habitat)

tl$mean_trophic_level <- as.character(tl$mean_trophic_level)

tl[nrow(tl) + 1,] = list("Human", "5", "NA")

tl <- tl %>%
  mutate(Resource_cat = case_when(
    startsWith(Resource, "Halibut") ~ "20",
    startsWith(Resource, "Marine") ~ "19.5",
    startsWith(Resource, "Non-Halibut") ~ "19",
    startsWith(Resource, "Herring") ~ "15",
    startsWith(Resource, "Crab") ~ "14.5",
    startsWith(Resource, "Other") ~ "15",
    startsWith(Resource, "Mollusc") ~ "14",
    startsWith(Resource, "Seaweed") ~ "14.5",
    startsWith(Resource, "Salmon") ~ "11.5",
    startsWith(Resource, "Char") ~ "10.5",
    startsWith(Resource, "Smelt") ~ "11",
    startsWith(Resource, "Birds") ~ "5",
    startsWith(Resource, "Large") ~ "6",
    startsWith(Resource, "Plants") ~ "5.5",
    startsWith(Resource, "Berries") ~ "6.5",
    startsWith(Resource, "Human") ~ "12",
  ))



tl <- tl %>%
  mutate(Habitat_cat = case_when(
    startsWith(Habitat, "Fresh") ~ "4",
    startsWith(Habitat, "Mar") ~ "1",
    startsWith(Habitat, "Near") ~ "2",
    startsWith(Habitat, "Terr") ~ "5",
    startsWith(Habitat, "NA") ~ "3",
  ))

tl$Habitat_cat <- as.numeric(tl$Habitat_cat)

tl$Resource_cat <- as.numeric(tl$Resource_cat)
str(tl)

#tl$Resource <- ordered(tl$Resource,
#                       levels = c("Birds/Eggs", "Large Land Mammals", "Plants/Greens/Mushrooms", "Berries", "Salmon", "Char", "Smelt", "Human", "Crab", "Herring Roe", "Mollusc", "Other", "Seaweed/Kelp", "Halibut", "Marine Invertebrates", "Non-Halibut Fish" ))


str(tl)
tl$Habitat <- ordered(tl$Habitat,
                      levels = c("Marine", "Nearshore",  "NA", "Freshwater_Anadromous", "Terrestrial"))

##Trying to graph network w/ igraph
#We start by converting the raw data to an igraph network object. Here we use igraph’s graph.data.frame function, which takes two data frames: d and vertices.

#d describes the edges of the network. Its first two columns are the IDs of the source and the target node for each edge. The following columns are edge attributes (weight, type, label, or anything else).
#vertices starts with a column of node IDs. Any following columns are interpreted as node attributes
library(igraph)
##so d would be my is1, and vertices would be tl, with trophic level as a node attribute
net <- graph_from_data_frame(d= is1, vertices = tl, directed = T)
class(net)
net

E(net)
V(net)

plot(net, edge.arrow.size = .4, vertex.label = NA)

##replace the vertex names
plot(net, edge.arrow.size = 0.2, edge.curved = 0, vertex.label = V(net)$Resource)

##Generate Colours based on Habitat 
#colrs <- c("salmon", "darkblue", "lightblue", "darkgreen", "black")
colrs <- c("#003366","#CC9966","black", "#FF9999","#339933")
#colrs <- c("#FF9999", "#FF9999","#FF9999","#003366","#003366","#003366","#CC9966", "#CC9966", "#CC9966","#CC9966","#CC9966","#339933", "#339933","#339933","#339933", "black")
#colrs <- c("#339933", "#339933","#339933","#339933","#FF9999", "#FF9999","#FF9999","black", "#CC9966", "#CC9966", "#CC9966","#CC9966","#CC9966","#003366","#003366","#003366")
V(net)$color <- colrs[V(net)$Habitat_cat] ##has to be numeric to assign colors this way

V(net)$Habitat_cat
V(net)$color

#Set edge width based on weight
E(net)$width <- E(net)$total_percapitalb_harvested/5


plot(net, edge.arrow.size = 0.5, edge.curved = 0, vertex.label = V(net)$Resource)


##color edges of graph based on source node color
edge.start <- ends(net, es = E(net), names = F)[,1]
edge.col <- V(net)$color[edge.start]

##want to create a matrix of coordinates, where y is the trophic level, and x is the habitat
tl$mean_trophic_level <- as.numeric(tl$mean_trophic_level)
lay <- matrix(nrow = nrow(tl), ncol = 2)
lay[,1] <- tl$Resource_cat
lay[,2] <- tl$mean_trophic_level
fw_plot <- plot(net, edge.color = edge.col, edge.curved = 0, arrow.size = 15, layout = lay)
fw_plot <- plot(net, edge.color = edge.col, edge.curved = 0, arrow.size = 15, layout = lay, vertex.label = NA, arrow.size = 25)


##i think also want to have connections between trophic layers, so need to figure out how to code in "dummy" interactions


##Now plot out skagway, see what it looks like
##need to generate interaction matrix 
mat <- skagway_1987 %>%
  dplyr::rename(Resource = "General_Category") %>%
  mutate(Consumer = "Human") 

is1 <- mat %>%
  ungroup() %>%
  select(Resource,  Consumer, total_percapitalb_harvested)

tl <- mat %>%
  ungroup() %>%
  select(Resource, mean_trophic_level, Habitat)

tl$mean_trophic_level <- as.character(tl$mean_trophic_level)

tl[nrow(tl) + 1,] = list("Human", "5", "NA")

tl <- tl %>%
  mutate(Resource_cat = case_when(
    startsWith(Resource, "Halibut") ~ "20",
    startsWith(Resource, "Marine") ~ "19.5",
    startsWith(Resource, "Non-Halibut") ~ "19",
    startsWith(Resource, "Crab") ~ "14.5",
    startsWith(Resource, "Mollusc") ~ "14",
    startsWith(Resource, "Seaweed") ~ "14.5",
    startsWith(Resource, "Salmon") ~ "11.5",
    startsWith(Resource, "Char") ~ "10.5",
    startsWith(Resource, "Smelt") ~ "11",
    startsWith(Resource, "Birds") ~ "5",
    startsWith(Resource, "Large") ~ "6",
    startsWith(Resource, "Plants") ~ "5.5",
    startsWith(Resource, "Berries") ~ "6.5",
    startsWith(Resource, "Human") ~ "12",
  ))



tl <- tl %>%
  mutate(Habitat_cat = case_when(
    startsWith(Habitat, "Fresh") ~ "4",
    startsWith(Habitat, "Mar") ~ "1",
    startsWith(Habitat, "Near") ~ "2",
    startsWith(Habitat, "Terr") ~ "5",
    startsWith(Habitat, "NA") ~ "3",
  ))

tl$Habitat_cat <- as.numeric(tl$Habitat_cat)

tl$Resource_cat <- as.numeric(tl$Resource_cat)
str(tl)

#tl$Resource <- ordered(tl$Resource,
#                       levels = c("Birds/Eggs", "Large Land Mammals", "Plants/Greens/Mushrooms", "Berries", "Salmon", "Char", "Smelt", "Human", "Crab", "Herring Roe", "Mollusc", "Other", "Seaweed/Kelp", "Halibut", "Marine Invertebrates", "Non-Halibut Fish" ))


str(tl)
tl$Habitat <- ordered(tl$Habitat,
                      levels = c("Marine", "Nearshore",  "NA", "Freshwater_Anadromous", "Terrestrial"))

##Trying to graph network w/ igraph
#We start by converting the raw data to an igraph network object. Here we use igraph’s graph.data.frame function, which takes two data frames: d and vertices.

#d describes the edges of the network. Its first two columns are the IDs of the source and the target node for each edge. The following columns are edge attributes (weight, type, label, or anything else).
#vertices starts with a column of node IDs. Any following columns are interpreted as node attributes
#library(igraph)
##so d would be my is1, and vertices would be tl, with trophic level as a node attribute
net <- graph_from_data_frame(d= is1, vertices = tl, directed = T)
class(net)
net

E(net)
V(net)

plot(net, edge.arrow.size = .4, vertex.label = NA)

##replace the vertex names
plot(net, edge.arrow.size = 0.2, edge.curved = 0, vertex.label = V(net)$Resource)

##Generate Colours based on Habitat 
#colrs <- c("salmon", "darkblue", "lightblue", "darkgreen", "black")
colrs <- c("#003366","#CC9966","black", "#FF9999","#339933")
#colrs <- c("#FF9999", "#FF9999","#FF9999","#003366","#003366","#003366","#CC9966", "#CC9966", "#CC9966","#CC9966","#CC9966","#339933", "#339933","#339933","#339933", "black")
#colrs <- c("#339933", "#339933","#339933","#339933","#FF9999", "#FF9999","#FF9999","black", "#CC9966", "#CC9966", "#CC9966","#CC9966","#CC9966","#003366","#003366","#003366")
V(net)$color <- colrs[V(net)$Habitat_cat] ##has to be numeric to assign colors this way

V(net)$Habitat_cat
V(net)$color

#Set edge width based on weight
E(net)$width <- E(net)$total_percapitalb_harvested/2


plot(net, edge.arrow.size = 0.5, edge.curved = 0, vertex.label = V(net)$Resource)


##color edges of graph based on source node color
edge.start <- ends(net, es = E(net), names = F)[,1]
edge.col <- V(net)$color[edge.start]

##want to create a matrix of coordinates, where y is the trophic level, and x is the habitat
tl$mean_trophic_level <- as.numeric(tl$mean_trophic_level)
lay <- matrix(nrow = nrow(tl), ncol = 2)
lay[,1] <- tl$Resource_cat
lay[,2] <- tl$mean_trophic_level
fw_plot <- plot(net, edge.color = edge.col, edge.curved = 0, arrow.size = 15, layout = lay)
fw_plot <- plot(net, edge.color = edge.col, edge.curved = 0, arrow.size = 15, layout = lay, vertex.label = NA, arrow.size = 25)

