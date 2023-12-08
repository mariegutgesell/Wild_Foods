##Food Web Interaction Matrix and Visualization

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
##for each community-year want to sum by habitat, and then trophic category
df_sum <- df %>%
  group_by(Site_Year_Code, Habitat, Trophic_Level) %>%
  summarise(across(where(is.numeric), sum))


##how are there 0's for percapita harvest but estimated total harvest is not 0? 

##Plotting frequency of interactions
hist <- ggplot(df_sum, aes(x = Percapita_Pounds_Harvested)) +
  geom_histogram() +
  facet_wrap(~Site_Year_Code) 
hist

##working out how to plot food web -- start w/ 1 community-year
angoon_2012 <- df_sum %>%
  filter(Site_Year_Code == "Angoon_2012")

##need to generate interaction matrix 
mat <- angoon_2012 %>%
  unite(Resource, c(Habitat, Trophic_Level), sep = "_", remove = FALSE) %>%
  mutate(Consumer = "Human") 

is1 <- mat %>%
  ungroup() %>%
  select(Resource,  Consumer, Percapita_Pounds_Harvested)

tl <- mat %>%
  ungroup() %>%
  select(Resource, Trophic_Level, Habitat)
tl$Trophic_Level <- as.character(tl$Trophic_Level)

tl[nrow(tl) + 1,] = list("Human", "4.5", "NA")

tl <- tl %>%
  mutate(Habitat_cat = case_when(
    startsWith(Habitat, "Fresh") ~ "1",
    startsWith(Habitat, "Mar") ~ "2",
    startsWith(Habitat, "Near") ~ "3",
    startsWith(Habitat, "Terr") ~ "4",
    startsWith(Habitat, "NA") ~ "5",
  ))

tl$Habitat_cat <- as.numeric(tl$Habitat_cat)
##Trying to graph network w/ igraph
#We start by converting the raw data to an igraph network object. Here we use igraph’s graph.data.frame function, which takes two data frames: d and vertices.

  #d describes the edges of the network. Its first two columns are the IDs of the source and the target node for each edge. The following columns are edge attributes (weight, type, label, or anything else).
  #vertices starts with a column of node IDs. Any following columns are interpreted as node attributes

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
colrs <- c("salmon", "darkblue", "lightblue", "darkgreen", "black")
V(net)$color <- colrs[V(net)$Habitat_cat] ##has to be numeric to assign colors this way

V(net)$Habitat_cat
V(net)$color

#Set edge width based on weight
E(net)$width <- E(net)$Percapita_Pounds_Harvested/3


plot(net, edge.arrow.size = 0.5, edge.curved = 0, vertex.label = V(net)$Resource)


##color edges of graph based on source node color
edge.start <- ends(net, es = E(net), names = F)[,1]
edge.col <- V(net)$color[edge.start]

plot(net, edge.color = edge.col, edge.curved = .3, arrow.size = 10, layout = layout_on_grid)

##interactive plotting
tkid <- tkplot(net)
l <- tkplot.getcoords(tkid)
tk_close(tkid)
plot(net, layot = l, edge.color = edge.col)
##this is not really working, not saving coordinates properly 
 



V(net)$color


# get row/column names of new matrix from columns 1 and 2 of data.frame
myNames <- sort(unique(as.character(unlist(is1[1:2]))))

# build matrix of 0s
myMat <- matrix(0, 14, 14, dimnames = list(myNames, myNames))

# fill in upper triangle
myMat[as.matrix(is1[c(1,2)])] <- is1$Percapita_Pounds_Harvested
##fill in lower triange
myMat[as.matrix(is1[c(2,1)])] <- is1$Percapita_Pounds_Harvested







tl <- mat %>%
  ungroup() %>%
  select(Resource, Trophic_Level)
tl$Trophic_Level <- as.character(tl$Trophic_Level)

tl[nrow(tl) + 1,] = list("Human", "4.5")




library(cheddar)
data(TL86)
PlotWebByLevel(TL86)
