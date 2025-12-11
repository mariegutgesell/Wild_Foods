##comparing % alaska native to harvest stability 

library(tidyverse)
library(ggplot2)
library(readxl)

cd <- read_excel("data/CSIS_Community_Demographics.xlsx", sheet = 2) %>%
  unite(Site_Year_Code, c(Community, Survey_Year), sep = "_", remove = FALSE) %>%
  select(Site_Year_Code, Percent_Native_by_Household, Percent_Native_by_Individual)

cv <- read.csv("data/intermediate_data/temporal_harvest_phenology_summary_metrics_percapita.csv") %>%
  select(site, harvest_total_cv) %>%
  rename(Site_Year_Code = "site")
rob <- read.csv("data/intermediate_data/temporal_harvest_removal_results_percapita.csv") %>%
  select(Site_Year_Code, alpha)


df <- left_join(cd, cv, by = "Site_Year_Code") %>%
  left_join(rob, by = "Site_Year_Code")

df$Percent_Native_by_Household <- as.numeric(df$Percent_Native_by_Household)
df$Percent_Native_by_Individual <- as.numeric(df$Percent_Native_by_Individual)



##Plot 
ggplot(df, aes(y = harvest_total_cv, x = Percent_Native_by_Household)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()


ggplot(df, aes(y = harvest_total_cv, x = Percent_Native_by_Individual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

ggplot(df, aes(y = alpha, x= Percent_Native_by_Individual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()


lm1 <- lm(harvest_total_cv ~ Percent_Native_by_Individual, data = df) 
summary(lm1)

lm2 <- lm(alpha~ Percent_Native_by_Individual, data = df) 
summary(lm2)


##testing w/o klukwan
df_2 <- df %>%
  filter(!grepl("Klukwan", Site_Year_Code))

ggplot(df_2, aes(y = harvest_total_cv, x = Percent_Native_by_Individual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

ggplot(df_2, aes(y = alpha, x= Percent_Native_by_Individual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()


lm3 <- lm(harvest_total_cv ~ Percent_Native_by_Individual, data = df_2) 
summary(lm3)

lm4 <- lm(alpha~ Percent_Native_by_Individual, data = df_2) 
summary(lm4)
