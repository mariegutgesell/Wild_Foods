##Harvest Rank Aggregations -- Taking temporal average per community 
##Creating new dataframes of aggregated harvest of taxa comprising small portions of harvest 

library(tidyverse)
library(vegan)
library(factoextra)

##import cleaned harvest data and trophic info that is comparable across all years
df <- read.csv("data/intermediate_data/comparable_harvest_df.csv") %>%
  filter(!Site_Year_Code %in% c("Hoonah_2016") )


##Calculate proportions, do transformations and normalize percapita harvest and total harvest 
total_harvest <- df %>%
  dplyr:: select(Site_Year_Code, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum) %>%
  filter(Percapita_Pounds_Harvested_sum != 0) %>%
  group_by(Site_Year_Code) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum), list(total = sum))

df_2 <- df %>%
  left_join(total_harvest, by = "Site_Year_Code") %>%
  filter(!is.na(Estimated_Total_Pounds_Harvested_sum)) %>%
  filter(!is.na(Percapita_Pounds_Harvested_sum)) %>%
  mutate(Total_Harvest_prop = (Estimated_Total_Pounds_Harvested_sum/Estimated_Total_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop = (Percapita_Pounds_Harvested_sum/Percapita_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop_log = log(Percapita_Harvest_prop)) %>%
  mutate(Percapita_Harvest_prop_sqrt = sqrt(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_log = log(Total_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_sqrt = sqrt(Total_Harvest_prop)) %>%
  group_by(Site_Year_Code) %>%
  mutate(Percapita_Harvest_prop_scale = scale(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_scale = scale(Total_Harvest_prop)) #%>%

df_comm_avg <- df_2 %>%
  dplyr::select(Site_Year_Code, Habitat, Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum, Total_Harvest_prop) %>% ##total harvest proportion and percapita harvest proportion are the same
  separate(Site_Year_Code, c("Site", "Year"), sep = "_") %>%
  group_by(Site, Habitat, Lowest_Common_Taxon_Name) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum, Total_Harvest_prop), list(avg = mean)) 

##For each community/year, create rank # based on proportion of total harvest
##first for total harvest
total_harvest_rank <- df_comm_avg %>%
  dplyr::select(Site, Lowest_Common_Taxon_Name, Habitat, Estimated_Total_Pounds_Harvested_sum_avg, Total_Harvest_prop_avg) %>%
  group_by(Site) %>%
  mutate(harvest_rank = rank(-Total_Harvest_prop_avg, ties.method = "min")) %>%
  dplyr:: rename(Estimated_Total_Pounds_Harvested_sum = "Estimated_Total_Pounds_Harvested_sum_avg", Total_Harvest_prop = "Total_Harvest_prop_avg") #%>%
  
#group_by(Site_Year_Code, harvest_rank) %>%
# mutate(unique_identifier = letter()) %>%
#mutate(harvest_rank = paste0(harvest_rank, "", unique_identifier)) %>%
#select(-unique_identifier)

##Can aggregate based on rank... e.g., top 10 species, then aggregate below that
##Can also try based on proportion... so top 25%, 50% etc... but this could be very few species for some communities, so maybe better to do based on rank? 
##try multiple ways... and do for total harvest and also percapita harvest.. is rank same? 

##Aggregate based on rank -- top 10 species in total harvest --------------
th_rank_10_a <- total_harvest_rank %>%
  filter(harvest_rank > 10) %>%
  group_by(Site) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Total_Harvest_prop), list(total = sum)) %>%
  mutate(Lowest_Common_Taxon_Name = "Aggregated Taxa", Habitat = NA, harvest_rank = ">10", Trophic_Level = NA, Trophic_Category = NA) %>%
  dplyr:: rename(Estimated_Total_Pounds_Harvested_sum = "Estimated_Total_Pounds_Harvested_sum_total", Total_Harvest_prop = "Total_Harvest_prop_total") %>%
  dplyr:: select(Site, Lowest_Common_Taxon_Name, Habitat, Estimated_Total_Pounds_Harvested_sum, Total_Harvest_prop, harvest_rank)


th_rank_10_b <- total_harvest_rank %>%
  filter(harvest_rank <= 10) %>%
  dplyr:: select(Site, Lowest_Common_Taxon_Name, Habitat, Estimated_Total_Pounds_Harvested_sum, Total_Harvest_prop, harvest_rank)

th_rank_10 <- rbind(th_rank_10_a, th_rank_10_b)

th_rank_10$harvest_rank <- as.numeric(th_rank_10$harvest_rank)

ggplot(th_rank_10, aes(x = harvest_rank, y= Total_Harvest_prop, fill = Habitat)) +
  geom_col() +
  scale_fill_manual(values = c("salmon", "darkblue", "brown", "darkgreen", "grey")) +
  geom_text(aes(label = Lowest_Common_Taxon_Name), hjust = -0.25, angle = 90, size = 2) + 
  facet_wrap(~Site, scale = "free")


##Aggregate based on rank -- top 5 species in total harvest ----------------------
th_rank_5_a <- total_harvest_rank %>%
  filter(harvest_rank > 5) %>%
  group_by(Site) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Total_Harvest_prop), list(total = sum)) %>%
  mutate(Lowest_Common_Taxon_Name = "Aggregated Taxa", Habitat = NA, harvest_rank = ">10", Trophic_Level = NA, Trophic_Category = NA) %>%
  dplyr::rename(Estimated_Total_Pounds_Harvested_sum = "Estimated_Total_Pounds_Harvested_sum_total", Total_Harvest_prop = "Total_Harvest_prop_total") %>%
  dplyr:: select(Site, Lowest_Common_Taxon_Name, Habitat, Estimated_Total_Pounds_Harvested_sum, Total_Harvest_prop, harvest_rank)


th_rank_5_b <- total_harvest_rank %>%
  filter(harvest_rank <= 5) %>%
  dplyr::select(Site, Lowest_Common_Taxon_Name, Habitat,  Estimated_Total_Pounds_Harvested_sum, Total_Harvest_prop, harvest_rank)

th_rank_5 <- rbind(th_rank_5_a, th_rank_5_b)

th_rank_5$harvest_rank <- as.numeric(th_rank_5$harvest_rank)

ggplot(th_rank_5, aes(x = harvest_rank, y= Total_Harvest_prop, fill = Habitat)) +
  geom_col() +
  scale_fill_manual(values = c("salmon", "darkblue", "brown", "darkgreen", "grey")) +
  geom_text(aes(label = Lowest_Common_Taxon_Name), hjust = -0.25, angle = 90, size = 2) + 
  facet_wrap(~Site, scale = "free")


##Testing standardization effects on NMDS and cluster analysis ---------------
##Harvest > rank 10 aggregated ------------------
##take total harvest (untransformed) and convert into matrix 
th_rank_10_wide <- th_rank_10 %>%
  dplyr::select(Site, Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum) %>%
  filter(Estimated_Total_Pounds_Harvested_sum != 0) %>% 
  spread(key = Lowest_Common_Taxon_Name, value = Estimated_Total_Pounds_Harvested_sum)

th_rank_10_wide[is.na(th_rank_10_wide)] <- 0
th_rank_10_wide <- th_rank_10_wide %>%
  remove_rownames %>% 
  column_to_rownames(var="Site")
##select only numerical rows
th_rank_10_wide <- th_rank_10_wide %>%
  dplyr:: select(`Aggregated Taxa`:`Tanner Crab`)

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

##Standardize by max: divide by margin maximum (default margin = 2, which is the columns)
th_rank_10_std_max <- decostand(th_rank_10_wide, "max", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_10_std_max, iter.max = 1000, centers = 7)
fviz_nbclust(th_rank_10_std_max,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_10_std_max)

th_10_clusters <- hclust(dist(th_rank_10_std_max))
plot(th_10_clusters)

##NMDS
sp_NMDS_1 <- metaMDS(th_rank_10_std_max,  autotransform = FALSE, distance = "bray", na.rm = TRUE) 
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)

##standardizing using max approach does have lower stress, and explains more variation... 
##Standardize by frequency: divide by margin total and multiply by the number of non-zero items, so the average of non-zero entris is one (Oksanen, 1983; default margin = 2, which is the columns)
th_rank_10_std_freq <- decostand(th_rank_10_wide, "frequency", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_10_std_freq, iter.max = 1000, centers = 2)
fviz_nbclust(th_rank_10_std_freq,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_10_std_freq)

th_10_clusters <- hclust(dist(th_rank_10_std_freq))
plot(th_10_clusters)

##NMDS --- bray-curtis 
sp_NMDS_1 <- metaMDS(th_rank_10_std_freq,  autotransform = FALSE, distance = "bray", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)

##standardizing using frequency so far has lowest stress, not a lot of variation though...
##Standardize by normalizing: make margin sum of squares equal to one (default margin = 1, which is the rows)
th_rank_10_std_norm <- decostand(th_rank_10_wide, "normalize", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_10_std_norm, iter.max = 1000, centers = 2)
fviz_nbclust(th_rank_10_std_norm,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_10_std_norm)

th_10_clusters <- hclust(dist(th_rank_10_std_norm))
plot(th_10_clusters)

##NMDS, 
sp_NMDS_1 <- metaMDS(th_rank_10_std_norm,  autotransform = FALSE, distance = "manhattan", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)

##pretty high stress and low variation...
##Standardize by range: standardize values into range 0...1 (default margin = 2, which is the rows) -- so this seems like proportion but across species not sites
th_rank_10_std_range <- decostand(th_rank_10_wide, "range", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_10_std_range, iter.max = 1000, centers = 7)
fviz_nbclust(th_rank_10_std_range,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_10_std_range)

th_10_clusters <- hclust(dist(th_rank_10_std_range))
plot(th_10_clusters)

##NMDS 
sp_NMDS_1 <- metaMDS(th_rank_10_std_range,  autotransform = FALSE, distance = "bray", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)
##looks similar to max approach

##Standardize by rank: replaces abundance values by their increasing ranks leaving zeros unchanged (default margin = 1, which is the rows) 
th_rank_10_std_rank <- decostand(th_rank_10_wide, "rank", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_10_std_rank, iter.max = 1000, centers = 7)
fviz_nbclust(th_rank_10_std_rank,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_10_std_rank)

th_10_clusters <- hclust(dist(th_rank_10_std_rank))
plot(th_10_clusters)

##NMDS 
sp_NMDS_1 <- metaMDS(th_rank_10_std_rank,  autotransform = FALSE, distance = "manhattan", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)
##high stress 

##Standardize by scaling to zero mean and unit variance: (default margin = 2, which is the columns) 
th_rank_10_std <- decostand(th_rank_10_wide, "standardize", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_10_std, iter.max = 1000, centers = 2)
fviz_nbclust(th_rank_10_std,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_10_std)

th_10_clusters <- hclust(dist(th_rank_10_std))
plot(th_10_clusters)

##NMDS 
sp_NMDS_1 <- metaMDS(th_rank_10_std,  autotransform = FALSE, distance = "bray", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)

##Standardize by chi-square: divide by row sums and square root of column sums, and adjust for square root of matrix total (Legendre & Gallagher 2001)(default margin = 1, which is the rows) 
th_rank_10_std_chi <- decostand(th_rank_10_wide, "chi.square", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_10_std_chi, iter.max = 1000, centers = 2)
fviz_nbclust(th_rank_10_std_chi,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_10_std_chi)

th_10_clusters <- hclust(dist(th_rank_10_std_chi))
plot(th_10_clusters)

##NMDS 
sp_NMDS_1 <- metaMDS(th_rank_10_std_chi,  autotransform = FALSE, distance = "bray", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)
##high stress

##Standardize by hellinger: square root of method = "total" (Legendre & Gallagher 2001) (default margin = 1, which is the rows) 
th_rank_10_std_sq <- decostand(th_rank_10_wide, "hellinger", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_10_std_sq, iter.max = 1000, centers = 8)
fviz_nbclust(th_rank_10_std_sq,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_10_std_sq)

th_10_clusters <- hclust(dist(th_rank_10_std_sq))
plot(th_10_clusters)

##NMDS 
sp_NMDS_1 <- metaMDS(th_rank_10_std_sq,  autotransform = FALSE, distance = "bray", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)

##Standardize by log: logarithmic transformation as suggested by Anderson et al. (2006): log_b (x) + 1 for x > 0, where b is the base of the logarithm; zeros are left as zeros.Higher bases give less weight to quantities and more to presences, and logbase = Inf gives the presence/absence scaling. Please note this is not log(x+1). (default margin = 1, which is the rows) 
th_rank_10_std_log <- decostand(th_rank_10_wide, "log", logbase = 2, na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_10_std_log, iter.max = 1000, centers = 5)
fviz_nbclust(th_rank_10_std_log,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_10_std_log)

th_10_clusters <- hclust(dist(th_rank_10_std_log))
plot(th_10_clusters)

##NMDS 
sp_NMDS_1 <- metaMDS(th_rank_10_std_log,  autotransform = FALSE, distance = "bray", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)
##high stress, little variation 


##Testing standardization approaches -- top 5,all others aggregated   ----------------
##using vegan decostand function
##take total harvest (untransformed) and convert into matrix 
th_rank_5_wide <- th_rank_5 %>%
  dplyr::select(Site, Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum) %>%
  filter(Estimated_Total_Pounds_Harvested_sum != 0) %>% 
  spread(key = Lowest_Common_Taxon_Name, value = Estimated_Total_Pounds_Harvested_sum)


th_rank_5_wide[is.na(th_rank_5_wide)] <- 0
th_rank_5_wide <- th_rank_5_wide %>%
  remove_rownames %>% 
  column_to_rownames(var="Site")
##select only numerical rows
th_rank_5_wide <- th_rank_5_wide %>%
  dplyr::select(`Aggregated Taxa`:`Sockeye Salmon`)

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

##Standardize by max: divide by margin maximum (default margin = 2, which is the columns)
th_rank_5_std_max <- decostand(th_rank_5_wide, "max", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_5_std_max, iter.max = 1000, centers = 2)
fviz_nbclust(th_rank_5_std_max,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_5_std_max)

th_5_clusters <- hclust(dist(th_rank_5_std_max))
plot(th_5_clusters)

##NMDS
sp_NMDS_1 <- metaMDS(th_rank_5_std_max,  autotransform = FALSE, distance = "bray", na.rm = TRUE) 
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)

##standardizing using max approach does have lower stress, and explains more variation... 
##Standardize by frequency: divide by margin total and multiply by the number of non-zero items, so the average of non-zero entris is one (Oksanen, 1983; default margin = 2, which is the columns)
th_rank_5_std_freq <- decostand(th_rank_5_wide, "frequency", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_5_std_freq, iter.max = 1000, centers = 2)
fviz_nbclust(th_rank_5_std_freq,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_5_std_freq)

th_5_clusters <- hclust(dist(th_rank_5_std_freq))
plot(th_5_clusters)

##NMDS --- bray-curtis 
sp_NMDS_1 <- metaMDS(th_rank_5_std_freq,  autotransform = FALSE, distance = "bray", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)

##standardizing using frequency so far has lowest stress, not a lot of variation though...
##Standardize by normalizing: make margin sum of squares equal to one (default margin = 1, which is the rows)
th_rank_5_std_norm <- decostand(th_rank_5_wide, "normalize", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_5_std_norm, iter.max = 1000, centers = 2)
fviz_nbclust(th_rank_5_std_norm,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_5_std_norm)

th_5_clusters <- hclust(dist(th_rank_5_std_norm))
plot(th_5_clusters)

##NMDS, 
sp_NMDS_1 <- metaMDS(th_rank_5_std_norm,  autotransform = FALSE, distance = "bray", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)

##pretty high stress and low variation...
##Standardize by range: standardize values into range 0...1 (default margin = 2, which is the rows) -- so this seems like proportion but across species not sites
th_rank_5_std_range <- decostand(th_rank_5_wide, "range", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_5_std_range, iter.max = 1000, centers = 2)
fviz_nbclust(th_rank_5_std_range,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_5_std_range)

th_5_clusters <- hclust(dist(th_rank_5_std_range))
plot(th_5_clusters)

##NMDS 
sp_NMDS_1 <- metaMDS(th_rank_5_std_range,  autotransform = FALSE, distance = "bray", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)
##looks similar to max approach

##Standardize by rank: replaces abundance values by their increasing ranks leaving zeros unchanged (default margin = 1, which is the rows) 
th_rank_5_std_rank <- decostand(th_rank_5_wide, "rank", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_5_std_rank, iter.max = 1000, centers = 2)
fviz_nbclust(th_rank_5_std_rank,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_5_std_rank)

th_5_clusters <- hclust(dist(th_rank_5_std_rank))
plot(th_5_clusters)

##NMDS 
sp_NMDS_1 <- metaMDS(th_rank_5_std_rank,  autotransform = FALSE, distance = "bray", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)
##high stress 

##Standardize by scaling to zero mean and unit variance: (default margin = 2, which is the columns) 
th_rank_5_std <- decostand(th_rank_5_wide, "standardize", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_5_std, iter.max = 1000, centers = 2)
fviz_nbclust(th_rank_5_std,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_5_std)

th_5_clusters <- hclust(dist(th_rank_5_std))
plot(th_5_clusters)

##NMDS 
sp_NMDS_1 <- metaMDS(th_rank_5_std,  autotransform = FALSE, distance = "bray", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)

##Standardize by chi-square: divide by row sums and square root of column sums, and adjust for square root of matrix total (Legendre & Gallagher 2001)(default margin = 1, which is the rows) 
th_rank_5_std_chi <- decostand(th_rank_5_wide, "chi.square", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_5_std_chi, iter.max = 1000, centers = 2)
fviz_nbclust(th_rank_5_std_chi,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_5_std_chi)

th_5_clusters <- hclust(dist(th_rank_5_std_chi))
plot(th_5_clusters)

##NMDS 
sp_NMDS_1 <- metaMDS(th_rank_5_std_chi,  autotransform = FALSE, distance = "bray", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)
##high stress

##Standardize by hellinger: square root of method = "total" (Legendre & Gallagher 2001) (default margin = 1, which is the rows) 
th_rank_5_std_sq <- decostand(th_rank_5_wide, "hellinger", na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_5_std_sq, iter.max = 1000, centers = 2)
fviz_nbclust(th_rank_5_std_sq,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_5_std_sq)

th_5_clusters <- hclust(dist(th_rank_5_std_sq))
plot(th_5_clusters)

##NMDS 
sp_NMDS_1 <- metaMDS(th_rank_5_std_sq,  autotransform = FALSE, distance = "bray", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)

##Standardize by log: logarithmic transformation as suggested by Anderson et al. (2006): log_b (x) + 1 for x > 0, where b is the base of the logarithm; zeros are left as zeros.Higher bases give less weight to quantities and more to presences, and logbase = Inf gives the presence/absence scaling. Please note this is not log(x+1). (default margin = 1, which is the rows) 
th_rank_5_std_log <- decostand(th_rank_5_wide, "log", logbase = 2, na.rm = TRUE)

##K-means clustering  -- common clustering approach that uses algorithm that minimizes intracluster distances, based on sum of squares of euclidean distances to centroid
k2 <- kmeans(th_rank_5_std_log, iter.max = 1000, centers = 4)
fviz_nbclust(th_rank_5_std_log,kmeans, method = "silhouette")
fviz_cluster(k2, data = th_rank_5_std_log)

th_5_clusters <- hclust(dist(th_rank_5_std_log))
plot(th_5_clusters)

##NMDS 
sp_NMDS_1 <- metaMDS(th_rank_5_std_log,  autotransform = FALSE, distance = "bray", na.rm = TRUE) ##The sqrt() transformation is applied if the maximum value is ≤ 50 and wisconsin() standardization is applied if the maximum value > 9, but it is recommended to do desired transformations/standardizations first and then do autotransform = FALSE
stressplot(sp_NMDS_1)
##a rule of thumb is that stress should be less than 0.2, ideally less than 0.1 
sp_NMDS_1$stress
plot(sp_NMDS_1)
ordiplot(sp_NMDS_1,type="n")
orditorp(sp_NMDS_1,display="species",col="red",air=0.01)
orditorp(sp_NMDS_1,display="sites",cex=1,air=0.01)
##high stress, little variation 