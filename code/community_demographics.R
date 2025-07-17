##Community size and demographics


library(tidyverse)
library(ggplot2)
library(readxl)

df <- read_excel("data/CSIS_Community_Demographics.xlsx", sheet = 1)

str(df)
df$Community_Population <- as.numeric(df$Community_Population)
df$Percent_Native_Population <- as.numeric(df$Percent_Native_Population)
df$Occupied_Housing_Units <- as.numeric(df$Occupied_Housing_Units)
mean_pop <- df %>%
  filter(!is.na(Community_Population)) %>%
  group_by(Community) %>%
  summarise_at(vars(Community_Population:Occupied_Housing_Units), list(mean = mean, sd = sd))


ggplot(df, aes(x = Year, y = Community_Population, group = Community, color = Community)) +
  geom_point() +
  geom_line() +
  theme_classic()


ggplot(df, aes(x = Year, y = Percent_Native_Population, group = Community, color = Community)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~Community)
