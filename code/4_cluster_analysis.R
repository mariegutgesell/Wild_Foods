##Interaction matrix cluster analysis

##Requirements of data:
## - all samples (i.e., community-year) needs to have measurements for the same entities, so need to have 0's where certain trophic levels were not harvested (i..e, need to have every trophic level in every community-year)


library(dplyr)
library(vegan)
library(factoextra)

#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)

##import cleaned harvest data and trophic info
setwd("~/Desktop/Wild Foods Repo/")
source("code/1_dataframe_formation.R")
rm(list = ls()[!ls() %in% c("df_final")])
trophic_df <- read_excel("data/harvest_species_list_characteristics_5.xlsx")

##filter to only species harvested in harvest data, select only columns needed for food web interaction, then join trophic data
##note: in future will likely recalculate the harvest metrics, but lets not worry about that now -- also still need to fix those where conversion unit is 0 but did harvest resources
harvest_df <- df_final %>%
  select(Site_Year_Code, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Percapita_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Reported_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested) %>%
  filter(!if_all(Percapita_Pounds_Harvested:Estimated_Amount_Harvested, ~ .x == 0))

#harvest_df$Taxa_lvl5 <- str_remove(harvest_df$Taxa_lvl5, "Unknown ")
#harvest_df$Taxa_lvl4 <- str_remove(harvest_df$Taxa_lvl4, "Unknown ")

df <- harvest_df %>%
  left_join(trophic_df %>% select(Taxa_lvl4, Taxa_lvl5, Scientific_Name, Lowest_Taxonomic_Resolution, Habitat, Trophic_Level, Trophic_Category), by = c("Taxa_lvl4", "Taxa_lvl5")) %>%
  select(Site_Year_Code, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Scientific_Name, Lowest_Taxonomic_Resolution, Habitat, Trophic_Level, Trophic_Category,Percapita_Pounds_Harvested:Estimated_Amount_Harvested)

str(df)
##NOTE: need to sort out cases where for example reported number or pounds harvested is 0, but have an estimate of total harvested 

##Cluster/PCA analysis for all species/years -- by HABITAT and TROPHIC CATEGORY ------------
##for each community-year want to sum by habitat, and then trophic category
df_sum <- df %>%
  group_by(Site_Year_Code, Habitat, Trophic_Level) %>%
  summarise(across(where(is.numeric), sum)) %>%
  unite(Resource, c(Habitat, Trophic_Level), sep = "_", remove = FALSE) %>%
  ungroup() %>%
  select(Site_Year_Code, Resource, Percapita_Pounds_Harvested)

##convert into wide format 
df_wide <- spread(df_sum, key = Resource, value = Percapita_Pounds_Harvested)
df_wide[is.na(df_wide)] <- 0
str(df_wide)
df_wide2 <- df_wide %>%
  select(Freshwater_Anadromous_3:Terrestrial_3)
is.na(df_wide2)


##example cluster analysis (still need to figure out what is the best one)
##scale data so it is normalized (i think)
df_wide_scale <- as.data.frame(scale(df_wide2))

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
library(factoextra)
k2 <- kmeans(df_wide_scale, iter.max = 1000, centers = 3)
fviz_cluster(k2, data = df_wide_scale)

##why do clusters change when re-running it? how do you then determine which clusters are appropirate? does this mean the clusters aren't strong? 
##what is an appropriate amount of variance that should be explained?

fviz_nbclust(df_wide_scale,kmeans, method = "silhouette")


##Next steps:
##- Identify level of identification that is comparable across years -- or potentially do this within years?
##    - Maybe try just 
##- Need to figure out what these clusters mean -- what is driving the presence of these different clusters?
###   - Is there something like PCA or other constrained clustering approach that is approporiate but also shows us what is driving the clusters?
###   - Is kmeans the best clustering approach? 
###   - what does it mean if your clusters keep changing? 

##Could community harvest structure be becoming more similar over time? -- lots of the spread on the axis seems to be driven by older surveys, 
##definitely want to look at things with year independently, but could have a way of quantifying within and across year differences... maybe showing things are more similar now?
##somehow need to control for differences in sample size.. but an idea of something to look into

##playing around with PCA
library(vegan)

site_codes <- df_wide[,1]
trophic_pca <- prcomp(df_wide_scale, scale = TRUE)
summary(trophic_pca)
pca_loadings <- trophic_pca[["x"]]
trophic_pca_df <- as.data.frame(cbind(site_codes, pca_loadings))


#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)

trophic_pca_plot <- ggbiplot(trophic_pca, choices = 1:2, obs.scale = 1, var.scale = 1) + ##can add in ellipse for the groups.. 
  geom_point(size = 3)+
  scale_color_manual("black")+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman"))
trophic_pca_plot

##could colour by cluster - once know which clusters actually make sense.. 


##Cluster/PCA analysis for all species and years --- by habitat and lowest taxonomic resolution  -------------
##i think need to think about this, because there are differences in what is present (depending on level of detail of survey) across years could drive differences

##convert into wide format 
df_test <- df %>%
  ungroup() %>%
  select(Site_Year_Code, Taxa_lvl2, Percapita_Pounds_Harvested) %>%
  unique() %>%
  group_by(Site_Year_Code, Taxa_lvl2) %>%
  dplyr::summarise(across(where(is.numeric), sum)) 
  
df_wide <- spread(df_test, key = Taxa_lvl2, value = Percapita_Pounds_Harvested)
df_wide[is.na(df_wide)] <- 0

df_wide <- df_wide %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")

df_wide2 <- df_wide %>%
  ungroup() %>%
  select(`Bird Eggs`:Vegetation)
df_wide2 <- as.data.frame(df_wide2)
##example cluster analysis (still need to figure out what is the best one)
##scale data so it is normalized (i think)
df_wide_scale <- as.data.frame(scale(df_wide2))

df_wide_scale <- df_wide_scale[, colSums(is.na(df_wide_scale)) < nrow(df_wide_scale)]

#df_wide_scale <- na.omit(df_wide_scale)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid

k2 <- kmeans(df_wide_scale, iter.max = 1000, centers = 2)
fviz_cluster(k2, data = df_wide_scale)

##why do clusters change when re-running it? how do you then determine which clusters are appropirate? does this mean the clusters aren't strong? 
##what is an appropriate amount of variance that should be explained?

fviz_nbclust(df_wide_scale,kmeans, method = "silhouette")



##playing around with PCA


site_codes <- df_wide[,1]
sp_pca <- prcomp(df_wide_scale, scale = TRUE)
summary(sp_pca)
pca_loadings <- sp_pca[["x"]]
sp_pca_df <- as.data.frame(cbind(site_codes, pca_loadings))



sp_pca_plot <- ggbiplot(sp_pca, choices = 1:2, obs.scale = 1, var.scale = 1, labels = rownames(df_wide_scale)) + ##can add in ellipse for the groups.. 
  geom_point(size = 3)+
  scale_color_manual("black")+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman"))
sp_pca_plot

##hierarchical clustering
#clusters <- hclust(dist(df_wide_scale))
#plot(clusters)

##Trying NMDS, with BC similarity
sp_NMDS <- metaMDS(df_wide2, # Our community-by-species matrix
                    k=2)
stressplot(sp_NMDS)
plot(sp_NMDS)
ordiplot(sp_NMDS,type="n")
orditorp(sp_NMDS,display="species",col="red",air=0.01)
orditorp(sp_NMDS,display="sites",cex=1,air=0.01)

##Cluster/PCA by decade/survey type ----------------------
##convert into wide format 
df_test <- df %>%
  ungroup() %>%
  select(Site_Year_Code, Taxa_lvl2, Percapita_Pounds_Harvested) %>%
  unique() %>%
  group_by(Site_Year_Code, Taxa_lvl2) %>%
  dplyr::summarise(across(where(is.numeric), sum)) %>%
  separate(Site_Year_Code, c("Site", "Year"), sep = "_")

df_wide <- spread(df_test, key = Taxa_lvl2, value = Percapita_Pounds_Harvested)
df_wide[is.na(df_wide)] <- 0

##Resources that have NA (i.e., all 0) for all percapita harvest: Alder, Bark, Coal, Eskimo potato, Goldeneye, Land Otter, Mink, Other Wood, Plantain, Red Fox, Red-Breasted Merganser, Roots, Scaup, Sorrel, Sourdock, Starfish, Unknown Kelp, Unknown Large Land Mammal, Unknown Marine Mammals, Unknown Merganser, Unknown Sculpin, Weasel, Wild Parsley, Wolf, Wolverine, Wood, 
df_nopercap <- df %>%
  filter(Taxa_lvl5 %in% c("Alder", "Bark", "Coal", "Eskimo potato", "Goldeneye", "Land Otter", "Mink", "Other Wood", "Plantain", "Red Fox", "Red-Breasted Merganser", "Roots", "Scaup", "Sorrel", "Sourdock", "Starfish", "Unknown Kelp", "Unknown Large Land Mammal", "Unknown Marine Mammals", "Unknown Merganser", "Unknown Sculpin, Weasel", "Wild Parsley", "Wolf", "Wolverine", "Wood"))
##these all have some estimate of either resources harvested or estimated resources harvested from whole community, how? 


##split year/site
survey_years <- df_test %>%
  ungroup() %>%
  select(Year) %>%
  unique()

##3 Survey clusters, that likely had same survey approaches...will want to confirm this with Lauren
##1983-1987, 1996-2000, 2012-2016

##Create separate df for each survey cluster, also look at communities within and see if survey detail level seems similar
survey_1 <- df_wide %>%
  filter(between (Year ,1983, 1987)) %>%
  unite(Site_Year_Code, c(Site, Year), sep = "_", remove = FALSE) %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")


##there are some communities that were surveyed more than once in each survey cluster.. 
##Angoon, Haines, Hoonah, Klukwan, Yakutat
#survey_1_comm <- survey_1 %>%
#  ungroup() %>%
#  select(Site, Year) %>%
#  unique()

survey_2 <- df_wide %>%
  filter(between (Year, 1996, 2000)) %>%
  unite(Site_Year_Code, c(Site, Year), sep = "_", remove = FALSE) %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")

#survey_2_comm <- survey_2 %>%
#  ungroup() %>%
#  select(Site, Year) %>%
#  unique()
##No repeat communities in this cluster

survey_3 <- df_wide %>%
  filter(between (Year, 2012, 2016))%>%
  unite(Site_Year_Code, c(Site, Year), sep = "_", remove = FALSE) %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")
#survey_3_comm <- survey_3 %>%
#  ungroup() %>%
#  select(Site, Year) %>%
#  unique()
##Honnah surveyed twice in this cluster


##Try cluster analysis/pca for each survey group
##Survey group 1 -- scale data
sg1_wide <- survey_1 %>%
  ungroup() %>%
  select(`Bird Eggs`:Vegetation)
sg1_wide <- as.data.frame(sg1_wide)


##scale data so it is normalized (i think)
sg1_wide_scale <- as.data.frame(scale(sg1_wide))
##remove the NA columns for now until recalculate harvest metrics
sg1_wide_scale <- sg1_wide_scale[, colSums(is.na(sg1_wide_scale)) < nrow(sg1_wide_scale)]

##Survey group 2 -- scale data
sg2_wide <- survey_2 %>%
  ungroup() %>%
  select(`Bird Eggs`:Vegetation)
sg2_wide <- as.data.frame(sg2_wide)


##scale data so it is normalized (i think)
sg2_wide_scale <- as.data.frame(scale(sg2_wide))
##remove the NA columns for now until recalculate harvest metrics
sg2_wide_scale <- sg2_wide_scale[, colSums(is.na(sg2_wide_scale)) < nrow(sg2_wide_scale)]

##Survey group 3 -- scale data
sg3_wide <- survey_3 %>%
  ungroup() %>%
  select(`Bird Eggs`:Vegetation)
sg3_wide <- as.data.frame(sg3_wide)

##scale data so it is normalized (i think)
sg3_wide_scale <- as.data.frame(scale(sg3_wide))
##remove the NA columns for now until recalculate harvest metrics
sg3_wide_scale <- sg3_wide_scale[, colSums(is.na(sg3_wide_scale)) < nrow(sg3_wide_scale)]



##largest number of unique columns in 1996-2000 cluster
##1982-1987: 103 unique taxa; 1996-2000: 179 unique taxa; 2012-2016: 170 unique taxa

##CLuster/PCA analysis - survey group 1
sg1_kmeans <- kmeans(sg1_wide_scale, iter.max = 1000, centers = 2)
fviz_cluster(sg1_kmeans, data = sg1_wide_scale)

fviz_nbclust(sg1_wide_scale,kmeans, method = "silhouette")


sg1_site_codes <- survey_1[,1]
sg1_pca <- prcomp(sg1_wide_scale, scale = TRUE)
summary(sg1_pca)
sg1_pca_loadings <- sg1_pca[["x"]]
sg1_pca_df <- as.data.frame(cbind(sg1_site_codes, sg1_pca_loadings))

sg1_pca_plot <- ggbiplot(sg1_pca, choices = 1:2, obs.scale = 1, var.scale = 1, labels = rownames(sg1_wide_scale)) + ##can add in ellipse for the groups.. 
  geom_point(size = 3)+
  scale_color_manual("black")+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman"))
sg1_pca_plot

##Trying NMDS, with BC similarity
sg1_NMDS <- metaMDS(sg1_wide, # Our community-by-species matrix
                     k=2)
stressplot(sg1_NMDS)
plot(sg1_NMDS)
ordiplot(sg1_NMDS,type="n")
orditorp(sg1_NMDS,display="species",col="red",air=0.01)
orditorp(sg1_NMDS,display="sites",cex=1,air=0.01)


##CLuster/PCA analysis - survey group 2
sg2_kmeans <- kmeans(sg2_wide_scale, iter.max = 1000, centers = 2)
fviz_cluster(sg2_kmeans, data = sg2_wide_scale)

fviz_nbclust(sg2_wide_scale,kmeans, method = "silhouette")

##playing around with PCA

sg2_site_codes <- survey_2[,1]
sg2_pca <- prcomp(sg2_wide_scale, scale = TRUE)
summary(sg2_pca)
sg2_pca_loadings <- sg2_pca[["x"]]
sg2_pca_df <- as.data.frame(cbind(sg2_site_codes, sg2_pca_loadings))

sg2_pca_plot <- ggbiplot(sg2_pca, choices = 1:2, obs.scale = 1, var.scale = 1, labels = rownames(sg2_wide_scale)) + ##can add in ellipse for the groups.. 
  geom_point(size = 3)+
  scale_color_manual("black")+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman"))
sg2_pca_plot
#:NOTE: with PCA, not allowed to have more columns than units, so we violate assumptions of PCA.. 

##Trying NMDS, with BC similarity
sg2_NMDS <- metaMDS(sg2_wide, # Our community-by-species matrix
                    k=2)
stressplot(sg2_NMDS)
plot(sg2_NMDS)
ordiplot(sg2_NMDS,type="n")
orditorp(sg2_NMDS,display="species",col="red",air=0.01)
orditorp(sg2_NMDS,display="sites",cex=1,air=0.01)
##CLuster/PCA analysis - survey group 3
sg3_kmeans <- kmeans(sg3_wide_scale, iter.max = 1000, centers = 2)
fviz_cluster(sg3_kmeans, data = sg3_wide_scale)

fviz_nbclust(sg3_wide_scale,kmeans, method = "silhouette")

##playing around with PCA

sg3_site_codes <- survey_3[,1]
sg3_pca <- prcomp(sg3_wide_scale, scale = TRUE)
summary(sg3_pca)
sg3_pca_loadings <- sg3_pca[["x"]]
sg3_pca_df <- as.data.frame(cbind(sg3_site_codes, sg3_pca_loadings))

sg3_pca_plot <- ggbiplot(sg3_pca, choices = 1:2, obs.scale = 1, var.scale = 1, labels = rownames(sg3_wide_scale)) + ##can add in ellipse for the groups.. 
  geom_point(size = 1)+
  scale_color_manual("black")+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman"))
sg3_pca_plot

##Trying NMDS, with BC similarity
sg3_NMDS <- metaMDS(sg3_wide, # Our community-by-species matrix
                    k=2)
stressplot(sg3_NMDS)
plot(sg3_NMDS)
ordiplot(sg3_NMDS,type="n")
orditorp(sg3_NMDS,display="species",col="red",air=0.01)
orditorp(sg3_NMDS,display="sites",cex=1,air=0.01)
###since there are alot of 0's, could be recommended to do a bray-curtis similarity matrix and nmds



##Histograms of interaction strengths by survey decade
df_test2 <- df %>%
  ungroup() %>%
  select(Site_Year_Code, Taxa_lvl5, Percapita_Pounds_Harvested) %>%
  unique() %>%
  group_by(Site_Year_Code, Taxa_lvl5) %>%
  dplyr::summarise(across(where(is.numeric), sum)) %>%
  separate(Site_Year_Code, c("Site", "Year"), sep = "_")

sg1_hist <- df_test2 %>%
  filter(between (Year ,1983, 1987)) %>%
  unite(Site_Year_Code, c(Site, Year), sep = "_", remove = FALSE) %>%
  ggplot(aes(x = Percapita_Pounds_Harvested)) +
  geom_histogram() +
  facet_wrap(~Site_Year_Code) 
sg1_hist

sg2_hist <- df_test2 %>%
  filter(between (Year ,1996, 2000)) %>%
  unite(Site_Year_Code, c(Site, Year), sep = "_", remove = FALSE) %>%
  ggplot(aes(x = Percapita_Pounds_Harvested)) +
  geom_histogram() +
  facet_wrap(~Site_Year_Code) 
sg2_hist

sg3_hist <- df_test2 %>%
  filter(between (Year ,2012, 2016)) %>%
  unite(Site_Year_Code, c(Site, Year), sep = "_", remove = FALSE) %>%
  ggplot(aes(x = Percapita_Pounds_Harvested)) +
  geom_histogram() +
  facet_wrap(~Site_Year_Code) 
sg3_hist

##Next steps:
##- Identify level of identification that is comparable across years -- or potentially do this within years?
##    - Maybe try just 
##- Need to figure out what these clusters mean -- what is driving the presence of these different clusters?
###   - Is there something like PCA or other constrained clustering approach that is approporiate but also shows us what is driving the clusters?
###   - Is kmeans the best clustering approach? 
###   - what does it mean if your clusters keep changing? 

##Could community harvest structure be becoming more similar over time? -- lots of the spread on the axis seems to be driven by older surveys, 
##definitely want to look at things with year independently, but could have a way of quantifying within and across year differences... maybe showing things are more similar now?
##somehow need to control for differences in sample size.. but an idea of something to look into


##PLAYing around with visualizing communities that may have different harvest structure
##haines 1983
haines_1983 <- df_test %>%
  unite(Site_Year_Code, c(Site, Year), sep = "_") %>%
  filter(Site_Year_Code == "Haines_1983") %>%
  filter(!is.na(Percapita_Pounds_Harvested))

mat <- haines_1983 %>%
  dplyr::rename(Resource = "Taxa_lvl2") %>%
  mutate(Consumer = "Human") 

is1 <- mat %>%
  ungroup() %>%
  select(Resource,  Consumer, Percapita_Pounds_Harvested)

tl <- mat %>%
  ungroup() %>%
  select(Resource)


tl[nrow(tl) + 1,] = list("Human")

library(igraph)
net <- graph_from_data_frame(d= is1, vertices = tl, directed = T)
class(net)
net

E(net)
V(net)

plot(net, edge.arrow.size = .4, vertex.label = NA)

##replace the vertex names
plot(net, edge.arrow.size = 0.2, edge.curved = 0, vertex.label = V(net)$Resource)

#Set edge width based on weight
E(net)$width <- E(net)$Percapita_Pounds_Harvested/3


plot(net, edge.arrow.size = 0.5, edge.curved = 0, vertex.label = V(net)$Resource)


##meyers chuck 1987
mc_1987 <- df_test %>%
  unite(Site_Year_Code, c(Site, Year), sep = "_") %>%
  filter(Site_Year_Code == "Meyers Chuck_1987") %>%
  filter(!is.na(Percapita_Pounds_Harvested))

mat <- mc_1987 %>%
  dplyr::rename(Resource = "Taxa_lvl2") %>%
  mutate(Consumer = "Human") 

is1 <- mat %>%
  ungroup() %>%
  select(Resource,  Consumer, Percapita_Pounds_Harvested)

tl <- mat %>%
  ungroup() %>%
  select(Resource)


tl[nrow(tl) + 1,] = list("Human")

library(igraph)
net <- graph_from_data_frame(d= is1, vertices = tl, directed = T)
class(net)
net

E(net)
V(net)

plot(net, edge.arrow.size = .4, vertex.label = NA)

##replace the vertex names
plot(net, edge.arrow.size = 0.2, edge.curved = 0, vertex.label = V(net)$Resource)

#Set edge width based on weight
E(net)$width <- E(net)$Percapita_Pounds_Harvested/3


plot(net, edge.arrow.size = 0.5, edge.curved = 0, vertex.label = V(net)$Resource)

##yakutat 1984
yt_1984 <- df_test %>%
  unite(Site_Year_Code, c(Site, Year), sep = "_") %>%
  filter(Site_Year_Code == "Yakutat_1984") %>%
  filter(!is.na(Percapita_Pounds_Harvested))

mat <- yt_1984 %>%
  dplyr::rename(Resource = "Taxa_lvl2") %>%
  mutate(Consumer = "Human") 

is1 <- mat %>%
  ungroup() %>%
  select(Resource,  Consumer, Percapita_Pounds_Harvested)

tl <- mat %>%
  ungroup() %>%
  select(Resource)


tl[nrow(tl) + 1,] = list("Human")

library(igraph)
net <- graph_from_data_frame(d= is1, vertices = tl, directed = T)
class(net)
net

E(net)
V(net)

plot(net, edge.arrow.size = .4, vertex.label = NA)

##replace the vertex names
plot(net, edge.arrow.size = 0.2, edge.curved = 0, vertex.label = V(net)$Resource)

#Set edge width based on weight
E(net)$width <- E(net)$Percapita_Pounds_Harvested/3


plot(net, edge.arrow.size = 0.5, edge.curved = 0, vertex.label = V(net)$Resource)

##gamecreek 1996
gc_1996<- df_test %>%
  unite(Site_Year_Code, c(Site, Year), sep = "_") %>%
  filter(Site_Year_Code == "Game Creek_1996") %>%
  filter(!is.na(Percapita_Pounds_Harvested))

mat <- gc_1996 %>%
  dplyr::rename(Resource = "Taxa_lvl2") %>%
  mutate(Consumer = "Human") 

is1 <- mat %>%
  ungroup() %>%
  select(Resource,  Consumer, Percapita_Pounds_Harvested)

tl <- mat %>%
  ungroup() %>%
  select(Resource)


tl[nrow(tl) + 1,] = list("Human")

library(igraph)
net <- graph_from_data_frame(d= is1, vertices = tl, directed = T)
class(net)
net

E(net)
V(net)

plot(net, edge.arrow.size = .4, vertex.label = NA)

##replace the vertex names
plot(net, edge.arrow.size = 0.2, edge.curved = 0, vertex.label = V(net)$Resource)

#Set edge width based on weight
E(net)$width <- E(net)$Percapita_Pounds_Harvested/3


plot(net, edge.arrow.size = 0.5, edge.curved = 0, vertex.label = V(net)$Resource)


##yakutat 2000
yt_2000<- df_test %>%
  unite(Site_Year_Code, c(Site, Year), sep = "_") %>%
  filter(Site_Year_Code == "Yakutat_2000") %>%
  filter(!is.na(Percapita_Pounds_Harvested))

mat <- yt_2000 %>%
  dplyr::rename(Resource = "Taxa_lvl2") %>%
  mutate(Consumer = "Human") 

is1 <- mat %>%
  ungroup() %>%
  select(Resource,  Consumer, Percapita_Pounds_Harvested)

tl <- mat %>%
  ungroup() %>%
  select(Resource)


tl[nrow(tl) + 1,] = list("Human")

library(igraph)
net <- graph_from_data_frame(d= is1, vertices = tl, directed = T)
class(net)
net

E(net)
V(net)

plot(net, edge.arrow.size = .4, vertex.label = NA)

##replace the vertex names
plot(net, edge.arrow.size = 0.2, edge.curved = 0, vertex.label = V(net)$Resource)

#Set edge width based on weight
E(net)$width <- E(net)$Percapita_Pounds_Harvested/3


plot(net, edge.arrow.size = 0.5, edge.curved = 0, vertex.label = V(net)$Resource)


##hoonah 2012
hn_2012<- df_test %>%
  unite(Site_Year_Code, c(Site, Year), sep = "_") %>%
  filter(Site_Year_Code == "Hoonah_2012") %>%
  filter(!is.na(Percapita_Pounds_Harvested))

mat <- hn_2012 %>%
  dplyr::rename(Resource = "Taxa_lvl2") %>%
  mutate(Consumer = "Human") 

is1 <- mat %>%
  ungroup() %>%
  select(Resource,  Consumer, Percapita_Pounds_Harvested)

tl <- mat %>%
  ungroup() %>%
  select(Resource)


tl[nrow(tl) + 1,] = list("Human")

library(igraph)
net <- graph_from_data_frame(d= is1, vertices = tl, directed = T)
class(net)
net

E(net)
V(net)

plot(net, edge.arrow.size = .4, vertex.label = NA)

##replace the vertex names
plot(net, edge.arrow.size = 0.2, edge.curved = 0, vertex.label = V(net)$Resource)

#Set edge width based on weight
E(net)$width <- E(net)$Percapita_Pounds_Harvested/3


plot(net, edge.arrow.size = 0.5, edge.curved = 0, vertex.label = V(net)$Resource)

##hoonah 2016
hn_2016<- df_test %>%
  unite(Site_Year_Code, c(Site, Year), sep = "_") %>%
  filter(Site_Year_Code == "Hoonah_2016") %>%
  filter(!is.na(Percapita_Pounds_Harvested))

mat <- hn_2016 %>%
  dplyr::rename(Resource = "Taxa_lvl2") %>%
  mutate(Consumer = "Human") 

is1 <- mat %>%
  ungroup() %>%
  select(Resource,  Consumer, Percapita_Pounds_Harvested)

tl <- mat %>%
  ungroup() %>%
  select(Resource)


tl[nrow(tl) + 1,] = list("Human")

library(igraph)
net <- graph_from_data_frame(d= is1, vertices = tl, directed = T)
class(net)
net

E(net)
V(net)

plot(net, edge.arrow.size = .4, vertex.label = NA)

##replace the vertex names
plot(net, edge.arrow.size = 0.2, edge.curved = 0, vertex.label = V(net)$Resource)

#Set edge width based on weight
E(net)$width <- E(net)$Percapita_Pounds_Harvested/3


plot(net, edge.arrow.size = 0.5, edge.curved = 0, vertex.label = V(net)$Resource)


##whalepass 2012
wp_2012<- df_test %>%
  unite(Site_Year_Code, c(Site, Year), sep = "_") %>%
  filter(Site_Year_Code == "Whale Pass_2012") %>%
  filter(!is.na(Percapita_Pounds_Harvested))

mat <- wp_2012 %>%
  dplyr::rename(Resource = "Taxa_lvl2") %>%
  mutate(Consumer = "Human") 

is1 <- mat %>%
  ungroup() %>%
  select(Resource,  Consumer, Percapita_Pounds_Harvested)

tl <- mat %>%
  ungroup() %>%
  select(Resource)


tl[nrow(tl) + 1,] = list("Human")

library(igraph)
net <- graph_from_data_frame(d= is1, vertices = tl, directed = T)
class(net)
net

E(net)
V(net)

plot(net, edge.arrow.size = .4, vertex.label = NA)

##replace the vertex names
plot(net, edge.arrow.size = 0.2, edge.curved = 0, vertex.label = V(net)$Resource)

#Set edge width based on weight
E(net)$width <- E(net)$Percapita_Pounds_Harvested/3


plot(net, edge.arrow.size = 0.5, edge.curved = 0, vertex.label = V(net)$Resource)

