##average percapita harvest per community

library(tidyverse)

df <- read.csv("data/intermediate_data/comparable_harvest_df.csv") %>%
  filter(!Site_Year_Code %in% c("Hoonah_2016") )

df_total <- df %>%
  select(Site_Year_Code, Lowest_Common_Taxon_Name, Percapita_Pounds_Harvested_sum) %>%
  filter(!is.na(Percapita_Pounds_Harvested_sum)) %>%
  group_by(Site_Year_Code) %>%
  summarise_at(vars(Percapita_Pounds_Harvested_sum), list(total = sum))

percap_avg <- df_total %>%
  summarise_at(vars(total), list(mean = mean))

hist(df_total$total)
  