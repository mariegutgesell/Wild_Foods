##NMDS and cluster analysis by habitat

library(dplyr)
library(vegan)
library(factoextra)
library(tidyverse)
#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)

##import cleaned harvest data and trophic info that is comparable across all years
df <- read.csv("data/intermediate_data/comparable_harvest_df.csv")


##Calculate sum by habitat and then proportion harvest across habitats
total_harvest <- df %>%
  select(Site_Year_Code, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum) %>%
  filter(!is.na(Estimated_Total_Pounds_Harvested_sum)) %>%
 # filter(Percapita_Pounds_Harvested_sum != 0) %>%
  group_by(Site_Year_Code) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum), list(total = sum))

habitat_harvest <- df %>%
  select(Site_Year_Code, Habitat,Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum)%>%
  filter(!is.na(Estimated_Total_Pounds_Harvested_sum)) %>%
  group_by(Site_Year_Code, Habitat) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum), list(habitat.total = sum))

df_2 <- habitat_harvest %>%
  left_join(total_harvest, by = "Site_Year_Code") %>%
  filter(!is.na(Estimated_Total_Pounds_Harvested_sum_habitat.total)) %>%
  mutate(Total_Harvest_prop = (Estimated_Total_Pounds_Harvested_sum_habitat.total/Estimated_Total_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop = (Percapita_Pounds_Harvested_sum_habitat.total/Percapita_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop_log = log(Percapita_Harvest_prop)) %>%
  mutate(Percapita_Harvest_prop_sqrt = sqrt(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_log = log(Total_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_sqrt = sqrt(Total_Harvest_prop)) %>%
  group_by(Site_Year_Code) %>%
  mutate(Percapita_Harvest_prop_scale = scale(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_scale = scale(Total_Harvest_prop)) #%>%
#filter(!grepl("Unknown", Lowest_Common_Taxon_Name))

hist(df_2$Total_Harvest_prop)

test <- total_harvest %>%
  filter(is.na(Estimated_Total_Pounds_Harvested_sum_total))
##NMDS and cluster analysis

df_wide_1 <- df_2 %>%
  select(Site_Year_Code, Habitat, Percapita_Harvest_prop) %>%
  filter(Percapita_Harvest_prop != 0) %>% ##keep only values that are not 0 for percapita harvest, 
  spread(key = Habitat, value = Percapita_Harvest_prop)
df_wide_1[is.na(df_wide_1)] <- 0

df_wide_1 <- df_wide_1 %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")
##select only numerical rows
df_wide_1a <- df_wide_1 %>%
  select(Freshwater_Anadromous:Terrestrial)
is.na(df_wide_1a)


##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
library(factoextra)
k2 <- kmeans(df_wide_1a, iter.max = 1000, centers = 2)
fviz_cluster(k2, data = df_wide_1a)

##why do clusters change when re-running it? how do you then determine which clusters are appropirate? does this mean the clusters aren't strong? 
##what is an appropriate amount of variance that should be explained?

fviz_nbclust(df_wide_1a,kmeans, method = "silhouette")


##Trying NMDS, with Manhattan distance
sp_NMDS_1 <- metaMDS(df_wide_1a, k=2, autotransform = TRUE, distance = "manhattan") ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)


##Habitat proportion Plot
df_2$Habitat <- ordered(df_2$Habitat, 
                                  levels = c("Marine", "Nearshore", "Freshwater_Anadromous", "Terrestrial"))

ggplot(df_2, aes(x = Site_Year_Code, y = Percapita_Harvest_prop, fill = Habitat)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#003366","#CC9966", "#FF9999","#339933"))+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x = element_blank(),  text = element_text(family = "Times New Roman"), strip.background = element_blank()) 

ggplot(df_2, aes(x = Site_Year_Code, y = Percapita_Pounds_Harvested_sum_habitat.total, fill = Habitat)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#003366","#CC9966", "#FF9999","#339933"))+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x = element_blank(),  text = element_text(family = "Times New Roman"), strip.background = element_blank()) 

ggplot(df_2, aes(x = Site_Year_Code, y = Estimated_Total_Pounds_Harvested_sum_habitat.total, fill = Habitat)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#003366","#CC9966", "#FF9999","#339933"))+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x = element_blank(),  text = element_text(family = "Times New Roman"), strip.background = element_blank()) 


##mean across years
df_comm_avg <- df_2 %>%
  select(Site_Year_Code, Habitat, Estimated_Total_Pounds_Harvested_sum_habitat.total, Percapita_Pounds_Harvested_sum_habitat.total, Total_Harvest_prop, Percapita_Harvest_prop) %>%
  separate(Site_Year_Code, c("Site", "Year"), sep = "_") %>%
  group_by(Site, Habitat) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum_habitat.total, Percapita_Pounds_Harvested_sum_habitat.total, Total_Harvest_prop, Percapita_Harvest_prop), list(avg = mean))

##Habitat proportion Plot
df_comm_avg$Habitat <- ordered(df_comm_avg$Habitat, 
                        levels = c("Marine", "Nearshore", "Freshwater_Anadromous", "Terrestrial"))

ggplot(df_comm_avg, aes(x = Site, y = Percapita_Harvest_prop_avg, fill = Habitat)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#003366","#CC9966", "#FF9999","#339933"))+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x = element_blank(),  text = element_text(family = "Times New Roman"), strip.background = element_blank()) 

ggplot(df_comm_avg, aes(x = Site, y = Percapita_Pounds_Harvested_sum_habitat.total_avg, fill = Habitat)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#003366","#CC9966", "#FF9999","#339933"))+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x = element_blank(),  text = element_text(family = "Times New Roman"), strip.background = element_blank()) 

ggplot(df_comm_avg, aes(x = Site, y = Estimated_Total_Pounds_Harvested_sum_habitat.total_avg, fill = Habitat)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#003366","#CC9966", "#FF9999","#339933"))+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x = element_blank(),  text = element_text(family = "Times New Roman"), strip.background = element_blank()) 

##Looking at NMDS by habitat for community avg
df_wide <- df_comm_avg %>%
  select(Site, Habitat, Percapita_Harvest_prop_avg) %>%
  filter(Percapita_Harvest_prop_avg != 0) %>% ##keep only values that are not 0 for percapita harvest, 
  spread(key = Habitat, value = Percapita_Harvest_prop_avg)
df_wide[is.na(df_wide)] <- 0

df_wide <- df_wide %>%
  remove_rownames %>% 
  column_to_rownames(var="Site")
##select only numerical rows
df_wide_a <- df_wide %>%
  select(Marine:Terrestrial)
is.na(df_wide_a)


##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
library(factoextra)
k2 <- kmeans(df_wide_a, iter.max = 1000, centers = 2)
fviz_cluster(k2, data = df_wide_a)

##why do clusters change when re-running it? how do you then determine which clusters are appropirate? does this mean the clusters aren't strong? 
##what is an appropriate amount of variance that should be explained?

fviz_nbclust(df_wide_a,kmeans, method = "silhouette")


##Trying NMDS, with Manhattan distance
sp_NMDS_1 <- metaMDS(df_wide_a, k=2, autotransform = TRUE, distance = "manhattan") ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)

##looking into klukwan
klukwan <- df %>%
  filter(grepl("Klukwan", Site_Year_Code))


##Freshwater NMDS 
fw <- df %>%
  filter(Habitat == "Freshwater_Anadromous") %>%
  left_join(total_harvest, by = "Site_Year_Code") %>%
  mutate(Total_Harvest_prop = (Estimated_Total_Pounds_Harvested_sum/Estimated_Total_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop = (Percapita_Pounds_Harvested_sum/Percapita_Pounds_Harvested_sum_total)*100) 

##NMDS and cluster analysis

df_wide_2 <- fw %>%
  select(Site_Year_Code, Lowest_Common_Taxon_Name, Percapita_Harvest_prop) %>%
  filter(Percapita_Harvest_prop != 0) %>% ##keep only values that are not 0 for percapita harvest, 
  spread(key = Lowest_Common_Taxon_Name, value = Percapita_Harvest_prop)
df_wide_2[is.na(df_wide_2)] <- 0

df_wide_2 <- df_wide_2 %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")
##select only numerical rows
df_wide_2a <- df_wide_2 %>%
  select(`Arctic Char`:`Unknown Trout`)
is.na(df_wide_2a)


##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
library(factoextra)
k2 <- kmeans(df_wide_2a, iter.max = 1000, centers = 2)
fviz_cluster(k2, data = df_wide_2a)

fviz_nbclust(df_wide_2a,kmeans, method = "silhouette")


##Trying NMDS, with Manhattan distance
sp_NMDS_2 <- metaMDS(df_wide_2a, k=2, autotransform = TRUE, distance = "manhattan") ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_2)
plot(sp_NMDS_2)
ordiplot(sp_NMDS_2,type="n")
orditorp(sp_NMDS_2,display="species",col="red",air=0.01)
orditorp(sp_NMDS_2,display="sites",cex=1,air=0.01)
  

##Nearshore NMDS 
ns <- df %>%
  filter(Habitat == "Nearshore") %>%
  left_join(total_harvest, by = "Site_Year_Code") %>%
  mutate(Total_Harvest_prop = (Estimated_Total_Pounds_Harvested_sum/Estimated_Total_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop = (Percapita_Pounds_Harvested_sum/Percapita_Pounds_Harvested_sum_total)*100) 

##NMDS and cluster analysis

df_wide_3 <- ns %>%
  select(Site_Year_Code, Lowest_Common_Taxon_Name, Percapita_Harvest_prop) %>%
  filter(Percapita_Harvest_prop != 0) %>% ##keep only values that are not 0 for percapita harvest, 
  spread(key = Lowest_Common_Taxon_Name, value = Percapita_Harvest_prop)
df_wide_3[is.na(df_wide_3)] <- 0

df_wide_3 <- df_wide_3 %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")
##select only numerical rows
df_wide_3a <- df_wide_3 %>%
  select(Abalone:`Seaweed/Kelp`)
is.na(df_wide_3a)


##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
library(factoextra)
k2 <- kmeans(df_wide_3a, iter.max = 1000, centers = 2)
fviz_cluster(k2, data = df_wide_3a)

fviz_nbclust(df_wide_3a,kmeans, method = "silhouette")


##Trying NMDS, with Manhattan distance
sp_NMDS_3 <- metaMDS(df_wide_3a, k=2, autotransform = TRUE, distance = "manhattan") ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_3)
plot(sp_NMDS_3)
ordiplot(sp_NMDS_3,type="n")
orditorp(sp_NMDS_3,display="species",col="red",air=0.01)
orditorp(sp_NMDS_3,display="sites",cex=1,air=0.01)

##Marine NMDS 
mr <- df %>%
  filter(Habitat == "Marine") %>%
  left_join(total_harvest, by = "Site_Year_Code") %>%
  mutate(Total_Harvest_prop = (Estimated_Total_Pounds_Harvested_sum/Estimated_Total_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop = (Percapita_Pounds_Harvested_sum/Percapita_Pounds_Harvested_sum_total)*100) 

##NMDS and cluster analysis

df_wide_4 <- mr %>%
  select(Site_Year_Code, Lowest_Common_Taxon_Name, Percapita_Harvest_prop) %>%
  filter(Percapita_Harvest_prop != 0) %>% ##keep only values that are not 0 for percapita harvest, 
  spread(key = Lowest_Common_Taxon_Name, value = Percapita_Harvest_prop)
df_wide_4[is.na(df_wide_4)] <- 0

df_wide_4 <- df_wide_4 %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")
##select only numerical rows
df_wide_4a <- df_wide_4 %>%
  select(Bowhead:`White Sturgeon`)
is.na(df_wide_4a)


##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
library(factoextra)
k2 <- kmeans(df_wide_4a, iter.max = 1000, centers = 8)
fviz_cluster(k2, data = df_wide_4a)

fviz_nbclust(df_wide_4a,kmeans, method = "silhouette")


##Trying NMDS, with Manhattan distance
sp_NMDS_4 <- metaMDS(df_wide_4a, k=2, autotransform = TRUE, distance = "manhattan") ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_4)
plot(sp_NMDS_4)
ordiplot(sp_NMDS_4,type="n")
orditorp(sp_NMDS_4,display="species",col="red",air=0.01)
orditorp(sp_NMDS_4,display="sites",cex=1,air=0.01)

##Terrestrial NMDS 
tr <- df %>%
  filter(Habitat == "Terrestrial") %>%
  left_join(total_harvest, by = "Site_Year_Code") %>%
  mutate(Total_Harvest_prop = (Estimated_Total_Pounds_Harvested_sum/Estimated_Total_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop = (Percapita_Pounds_Harvested_sum/Percapita_Pounds_Harvested_sum_total)*100) 

##NMDS and cluster analysis

df_wide_5 <- tr %>%
  select(Site_Year_Code, Lowest_Common_Taxon_Name, Percapita_Harvest_prop) %>%
  filter(Percapita_Harvest_prop != 0) %>% ##keep only values that are not 0 for percapita harvest, 
  spread(key = Lowest_Common_Taxon_Name, value = Percapita_Harvest_prop)
df_wide_5[is.na(df_wide_5)] <- 0

df_wide_5 <- df_wide_5 %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")
##select only numerical rows
df_wide_5a <- df_wide_5 %>%
  select(Beaver:`Wilson's Snipe`)
is.na(df_wide_5a)


##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
library(factoextra)
k2 <- kmeans(df_wide_5a, iter.max = 1000, centers = 2)
fviz_cluster(k2, data = df_wide_5a)

fviz_nbclust(df_wide_5a,kmeans, method = "silhouette")


##Trying NMDS, with Manhattan distance
sp_NMDS_5 <- metaMDS(df_wide_5a, k=2, autotransform = TRUE, distance = "manhattan") ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_5)
plot(sp_NMDS_5)
ordiplot(sp_NMDS_5,type="n")
orditorp(sp_NMDS_5,display="species",col="red",air=0.01)
orditorp(sp_NMDS_5,display="sites",cex=1,air=0.01)


