##Harvest structure patterns for top species across all communities

library(tidyverse)
library(vegan)
library(factoextra)

##import cleaned harvest data and trophic info that is comparable across all years
df <- read.csv("data/intermediate_data/comparable_harvest_df.csv") %>%
  filter(!Site_Year_Code %in% c("Hoonah_2016") )

library(forcats)
df$Estimated_Total_Pounds_Harvested_sum <- as.numeric(df$Estimated_Total_Pounds_Harvested_sum)
ggplot(df, aes(x = reorder(Lowest_Common_Taxon_Name, -Estimated_Total_Pounds_Harvested_sum), y = Estimated_Total_Pounds_Harvested_sum, fill = Habitat)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("salmon", "darkblue", "brown", "darkgreen", "grey")) +
  theme(axis.text.x =  element_text(angle = 45, hjust = 1))

test <- df %>%
  filter(Lowest_Common_Taxon_Name == "Mackerel")

##Calculate total proportion of harvest and rank
total_harvest_all <- 
  df %>%
  dplyr:: select(Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum) %>%
  filter(Percapita_Pounds_Harvested_sum != 0) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum), list(total_all = sum))

total_harvest_df <- df %>%
  dplyr:: select(Lowest_Common_Taxon_Name, Habitat, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum) %>%
  filter(Percapita_Pounds_Harvested_sum != 0) %>%
  group_by(Lowest_Common_Taxon_Name, Habitat) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum), list(total = sum)) %>%
  mutate(Total_Harvest_All = 	13026567, Percapita_Harvest = 18526.83) %>%
  mutate(Prop_total_harvest = Estimated_Total_Pounds_Harvested_sum_total/Total_Harvest_All) %>%
  ungroup() %>%
  mutate(harvest_rank = rank(-Prop_total_harvest, ties.method = "min"))


total_harvest_df$harvest_rank <- as.numeric(total_harvest_df$harvest_rank)
ggplot(total_harvest_df, aes(x = harvest_rank, y = Prop_total_harvest, fill = Habitat)) +
  geom_col()+
  scale_fill_manual(values = c("salmon", "darkblue", "brown", "darkgreen", "grey")) +
  geom_text(aes(label = Lowest_Common_Taxon_Name), hjust = -0.25, angle = 90, size = 3) + 
  theme(axis.text.x =  element_text(angle = 45, hjust = 1))

##list of taxons at different ranks
top_5_taxa <- total_harvest_df %>%
  filter(harvest_rank <= 5) %>%
  distinct(Lowest_Common_Taxon_Name)

top_10_taxa <- total_harvest_df %>%
  filter(harvest_rank <= 10) %>%
  distinct(Lowest_Common_Taxon_Name)

top_25_taxa <- total_harvest_df %>%
  filter(harvest_rank <= 25) %>%
  distinct(Lowest_Common_Taxon_Name)

##create dataframes w/ only top taxa
df_5 <- df %>%
  filter(Lowest_Common_Taxon_Name %in% top_5_taxa$Lowest_Common_Taxon_Name)

df_10 <- df %>%
  filter(Lowest_Common_Taxon_Name %in% top_10_taxa$Lowest_Common_Taxon_Name)

df_25 <- df %>%
  filter(Lowest_Common_Taxon_Name %in% top_25_taxa$Lowest_Common_Taxon_Name)

##Cluster/NMDS Analysis 
##Top 5 
th_rank_5_wide <- df_5 %>%
  dplyr::select(Site_Year_Code, Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum) %>%
  filter(Estimated_Total_Pounds_Harvested_sum != 0) %>% 
  spread(key = Lowest_Common_Taxon_Name, value = Estimated_Total_Pounds_Harvested_sum)

th_rank_5_wide[is.na(th_rank_5_wide)] <- 0
th_rank_5_wide <- th_rank_5_wide %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")
##select only numerical rows
th_rank_5_wide <- th_rank_5_wide %>%
  dplyr:: select(`Chinook Salmon`:`Sockeye Salmon`)

##Standardize by total: divide by margin total (default margin = 1, which is the rows)
##this is proportion, just not multiplied by 100
th_rank_5_std_total <- decostand(th_rank_5_wide, "total", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid

k2 <- kmeans(th_rank_5_std_total, iter.max = 1000, centers = 2)
fviz_nbclust(th_rank_5_std_total,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_5_std_total)

th_5_clusters <- hclust(dist(th_rank_5_std_total))
plot(th_5_clusters)

##NMDS
sp_NMDS_1 <- metaMDS(th_rank_5_std_total,  autotransform = FALSE, distance = "bray", na.rm = TRUE) 
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)

##Top 10 species
th_rank_10_wide <- df_10 %>%
  dplyr::select(Site_Year_Code, Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum) %>%
  filter(Estimated_Total_Pounds_Harvested_sum != 0) %>% 
  spread(key = Lowest_Common_Taxon_Name, value = Estimated_Total_Pounds_Harvested_sum)

th_rank_10_wide[is.na(th_rank_10_wide)] <- 0
th_rank_10_wide <- th_rank_10_wide %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")
##select only numerical rows
th_rank_10_wide <- th_rank_10_wide %>%
  dplyr:: select(`Berries`:`Sockeye Salmon`)

##Standardize by total: divide by margin total (default margin = 1, which is the rows)
##this is proportion, just not multiplied by 100
th_rank_10_std_total <- decostand(th_rank_10_wide, "total", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid

k2 <- kmeans(th_rank_10_std_total, iter.max = 1000, centers = 2)
fviz_nbclust(th_rank_10_std_total,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_10_std_total)

th_10_clusters <- hclust(dist(th_rank_10_std_total))
plot(th_10_clusters)

##NMDS
sp_NMDS_1 <- metaMDS(th_rank_10_std_total,  autotransform = FALSE, distance = "bray", na.rm = TRUE) 
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)

##Top 25
th_rank_25_wide <- df_25 %>%
  dplyr::select(Site_Year_Code, Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum) %>%
  filter(Estimated_Total_Pounds_Harvested_sum != 0) %>% 
  spread(key = Lowest_Common_Taxon_Name, value = Estimated_Total_Pounds_Harvested_sum)

th_rank_25_wide[is.na(th_rank_25_wide)] <- 0
th_rank_25_wide <- th_rank_25_wide %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")
##select only numerical rows
th_rank_25_wide <- th_rank_25_wide %>%
  dplyr:: select(`Berries`:`Tanner Crab`)

##Standardize by total: divide by margin total (default margin = 1, which is the rows)
##this is proportion, just not multiplied by 100
th_rank_25_std_total <- decostand(th_rank_25_wide, "total", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid

k2 <- kmeans(th_rank_25_std_total, iter.max = 1000, centers = 2)
fviz_nbclust(th_rank_25_std_total,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_25_std_total)

th_25_clusters <- hclust(dist(th_rank_25_std_total))
plot(th_25_clusters)

##NMDS
sp_NMDS_1 <- metaMDS(th_rank_25_std_total,  autotransform = FALSE, distance = "bray", na.rm = TRUE) 
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)
