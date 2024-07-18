##comparing harvest diversity characteristics calculated from % of total harvest and percapita harvest

prop <- read.csv("data/intermediate_data/harvest_diversity_metrics.csv")

pc <- read.csv("data/intermediate_data/harvest_diversity_metrics_percap.csv")

df <- left_join(prop, pc, by = c("Forest", "Site"))


##x = proportion, y = percap

ggplot(df, aes(x = richness.x, y= richness.y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

ggplot(df, aes(x = sw_diversity.x, y= sw_diversity.y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

ggplot(df, aes(x = evenness.x, y= evenness.y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

ggplot(df, aes(x = sd.x, y= sd.y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

ggplot(df, aes(x = evenness_h.x, y= evenness_h.y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

##all are essentially 1:1 except SD -- but coupling in food web lit is always a %, so i think SD of % makes more sense... 
