##Interaction matrix cluster analysis

##Requirements of data:
## - all samples (i.e., community-year) needs to have measurements for the same entities, so need to have 0's where certain trophic levels were not harvested (i..e, need to have every trophic level in every community-year)


library(dplyr)

##i think would want the dataframe set up so each row is a community-year, and each column is a trophic level/habitat
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


##trying cluster analysis by all species -------------
##i think need to think about this, because there are differences in what is present (depending on level of detail of survey) across years could drive differences

##convert into wide format 
df_test <- df %>%
  ungroup() %>%
  select(Site_Year_Code, Taxa_lvl5, Percapita_Pounds_Harvested) %>%
  unique() %>%
  group_by(Site_Year_Code, Taxa_lvl5) %>%
  dplyr::summarise(across(where(is.numeric), sum)) 
  
df_wide <- spread(df_test, key = Taxa_lvl5, value = Percapita_Pounds_Harvested)
df_wide[is.na(df_wide)] <- 0


str(df_wide)
df_wide2 <- df_wide %>%
  ungroup() %>%
  select(Abalone:`Yellow Eye Rockfish`)
na <- is.na(df_wide2)
str(df_wide2)
df_wide2 <- as.data.frame(df_wide2)
##example cluster analysis (still need to figure out what is the best one)
##scale data so it is normalized (i think)
df_wide_scale <- as.data.frame(scale(df_wide2))

df_wide_scale <- df_wide_scale[, colSums(is.na(df_wide_scale)) < nrow(df_wide_scale)]

##why eskimo potato, plantain NaN? -- because all values are 0 so standard deviation is 0 so can't scale.. 


#df_wide_scale <- na.omit(df_wide_scale)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
library(factoextra)
k2 <- kmeans(df_wide_scale, iter.max = 1000, centers = 2)
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
sp_pca <- prcomp(df_wide_scale, scale = TRUE)
summary(sp_pca)
pca_loadings <- sp_pca[["x"]]
sp_pca_df <- as.data.frame(cbind(site_codes, pca_loadings))


#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)

sp_pca_plot <- ggbiplot(trophic_pca, choices = 1:2, obs.scale = 1, var.scale = 1) + ##can add in ellipse for the groups.. 
  geom_point(size = 3)+
  scale_color_manual("black")+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman"))
sp_pca_plot

clusters <- hclust(dist(df_wide_scale))
plot(clusters)

