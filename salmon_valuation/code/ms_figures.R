##Figures for Salmon Valuation Manuscript 

library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)

##Figures to create:
##For each fishery type: sport, commercial, and then subsistence+personal use
##total catch (conservative and liberal estimates)
##percent of each species in harvest 2011-2020 - i think proportion based on sum of # caugh 2011-2020
##estimated # of servings
##estimated replacement cost -- ex-vessel and store?? 

##Create dataframe for personal use, subsistence and sport data ------------
##Read in raw total number of fish estimated for tongass and chugach - personal use, subsistence, sport 
c_fish_num <- read_excel("salmon_valuation/data/Regional_Analyses&Graphs_7-15-2025.xlsx", sheet = 4) %>%
  select(Year:`ForestFish_Liberal_100percentif CNF overlap>0`) %>%
  mutate(Forest ="Chugach") %>%
  rename(Forest_fish_conservative = "ForestFish_conservative_percent_CNF_Johnson et al 2019", Forest_fish_liberal = "ForestFish_Liberal_100percentif CNF overlap>0") %>%
  select(Forest, Fishery, Species, Year, Forest_fish_conservative, Forest_fish_liberal)

head(c_fish_num)

t_fish_num <- read_excel("salmon_valuation/data/Regional_Analyses&Graphs_7-15-2025.xlsx", sheet = 5) %>%
  select(Year:ID, Forest_fish_conservative, Forest_fish_liberal) %>%
  mutate(Forest ="Tongass") %>%
  select(Forest, Fishery, Species, Year, Forest_fish_conservative, Forest_fish_liberal)


fish_num <- rbind(t_fish_num, c_fish_num)

##sum across regions within the forests so get total per forest 
fish_num_summary <- fish_num %>%
  group_by(Forest, Fishery, Species, Year) %>%
  summarise_at(vars(Forest_fish_conservative, Forest_fish_liberal), list(sum = sum)) 


#Resale values 
resale_value <- read_excel("salmon_valuation/data/Regional_Analyses&Graphs_7-15-2025.xlsx", sheet = "Resale Values") %>%
  filter(!is.na(`Salmon Species`)) %>%
  rename(Species = "Salmon Species")

resale_value_mean <- resale_value %>%
  group_by(Species) %>%
  summarise_at(vars(Price_per_Pound), list(mean_value = mean))


##Fish Weights -- to convert fish num to lbs 
fish_weight <- read_excel("salmon_valuation/data/Regional_Analyses&Graphs_7-15-2025.xlsx", sheet = "Fish_Weight")

##Join fish number, value and weight dfs together and then convert number of fish to weight, and use that to estimate # of 6oz servings and replacement value 
fish_df_1 <- left_join(fish_num_summary, resale_value_mean, by = "Species") %>%
  left_join(fish_weight, by = "Species") %>%
  mutate(conversion_lb_to_oz = 6/16) %>%
  mutate(fish_total_weight_conservative = if_else(
    Forest == "Chugach",
    Forest_fish_conservative_sum * Chugach_fish_mass_lbs * Recovery_rate,
    Forest_fish_conservative_sum * Tongass_fish_lbs * Recovery_rate
  )) %>%
  mutate(fish_total_weight_liberal = if_else(
    Forest == "Chugach",
    Forest_fish_liberal_sum * Chugach_fish_mass_lbs * Recovery_rate,
    Forest_fish_liberal_sum * Tongass_fish_lbs * Recovery_rate
  )) %>%
  mutate(num_6oz_servings_conservative = fish_total_weight_conservative/conversion_lb_to_oz,
         num_6oz_servings_liberal = fish_total_weight_liberal/conversion_lb_to_oz,
         total_value_conservative = fish_total_weight_conservative*mean_value,
         total_value_liberal = fish_total_weight_liberal*mean_value) %>%
  select(Forest, Fishery, Species, Year, Forest_fish_conservative_sum, Forest_fish_liberal_sum,fish_total_weight_conservative,fish_total_weight_liberal, num_6oz_servings_conservative,  num_6oz_servings_liberal,  total_value_conservative, total_value_liberal) %>%
  pivot_longer(
    cols = c(
      Forest_fish_conservative_sum,
      Forest_fish_liberal_sum,
      fish_total_weight_conservative,
      fish_total_weight_liberal,
      num_6oz_servings_conservative,
      num_6oz_servings_liberal, 
      total_value_conservative,
      total_value_liberal
    ),
    names_to = c(".value", "Scenario"),
    names_pattern = "(.*)_(conservative|liberal)"
  ) %>%
  mutate(Scenario = str_to_title(Scenario)) %>%
  select(Forest, Fishery, Species, Year, Scenario, everything()) %>%
  rename(fish_total_num = "Forest_fish")

str(fish_df_1)

fish_df_1$Year <- as.numeric(fish_df_1$Year)

##Create dataframe for commercial fish -----------

##Read in total number of fish for commercial 
comm_df <- read_excel("salmon_valuation/data/Regional_Analyses&Graphs_7-15-2025.xlsx", sheet = "Commercial exvessel") %>%
  select(Year:`Total exvessel $`) %>%
  mutate(Fishery = "Commercial") %>%
  rename(Species = "Species Name", Forest_fish_conservative = "Forest Fish", Forest_weight_lb = "Forest Weight (lbs)", total_value = "Total exvessel $") %>%
  select(Forest, Fishery, Species, Year, Forest_fish_conservative, Forest_weight_lb, total_value) %>%
  group_by(Forest, Fishery, Species, Year) %>%
  summarise_at(vars(Forest_fish_conservative, Forest_weight_lb, total_value), list(sum = sum)) %>%
  rename(fish_total_weight = "Forest_weight_lb_sum", total_value = "total_value_sum", fish_total_num = "Forest_fish_conservative_sum") 


comm_recovery_rate <- read_excel("salmon_valuation/data/Regional_Analyses&Graphs_7-15-2025.xlsx", sheet = "Commercial") %>%
  select(Forest, `Species Name`, `Recovery Rate`) %>%
  unique() %>%
  rename(Species = "Species Name", recover_rate = "Recovery Rate")


comm_df_1 <- left_join(comm_df, comm_recovery_rate, by = c("Forest", "Species")) %>%
  mutate(conversion_lb_to_oz = 6/16,
         num_6oz_servings = fish_total_weight/conversion_lb_to_oz) %>%
  mutate(Scenario = "Conservative") %>%
  select(Forest, Fishery, Species, Year, Scenario, fish_total_num, fish_total_weight,  num_6oz_servings,   total_value)
comm_df_1$Year <- as.numeric(comm_df_1$Year)

##Combine all data together
df_all <- rbind(fish_df_1, comm_df_1)


##Now, want to calculate total across forests and species - not across years as using variation for boxplots - i think ? 
df_all_summary <- df_all %>%
  ungroup() %>%
  group_by(Fishery, Year, Scenario) %>%
  summarise_at(vars(fish_total_num, fish_total_weight,  num_6oz_servings,   total_value), list(annual_total = sum))


df_all_median <- df_all_summary %>%
  ungroup() %>%
  group_by(Fishery, Scenario) %>%
  summarise_at(vars(fish_total_num_annual_total, fish_total_weight_annual_total,  num_6oz_servings_annual_total,   total_value_annual_total), list(median = median))

##calculate proportion of total harvest each species for each year 
df_all_sp_total <- df_all %>%
  group_by(Fishery, Scenario) %>%
  summarise_at(vars(fish_total_num), list(total_num = sum))

df_all_sp_prop <- df_all %>%
  group_by(Fishery, Species, Scenario) %>%
  summarise_at(vars(fish_total_num), list(total_num_sp = sum)) %>%
  left_join(df_all_sp_total, by = c("Fishery", "Scenario"))  %>%
  mutate(sp_prop_harvest = total_num_sp/total_num * 100)

#df_all_sp_prop_mean <- df_all_sp_prop %>%
#  group_by(Fishery, Species, Scenario) %>%
#  summarise_at(vars(sp_prop_harvest), list(mean_prop = mean)) 


##Questions for Ryan: 
##When calculating proportion take mean of annual proportion, or proportion from total harvest across all years 2011-2022? 
##values for prop look different than in pie charts -- using conservative number? or by wieght? 
##liberal or conservative values for proportions?

##total harvest across all years, not mean 
##by number 
##use liberal 
##something is off about the replacement cost, need to go back and fix 
##do icons for $$ and servings, w/ median 


##playing around with some plots
##Sport plot 
sport_df <- df_all_summary %>%
  filter(Fishery == "Sport") %>%
  group_by(Scenario) %>%
  summarise_at(vars(fish_total_num_annual_total:total_value_annual_total), list(mean = mean, sd = sd, median = median))
  


head(sport_df)

sport_df$label_y_position <- sport_df$fish_total_num_annual_total_mean + sport_df$fish_total_num_annual_total_sd + 50000


sport_fish_num <- ggplot(sport_df, aes(x = Scenario, y = fish_total_num_annual_total_mean, fill = Scenario)) +
  geom_col() +
  geom_errorbar(aes(ymin =  fish_total_num_annual_total_mean - fish_total_num_annual_total_sd, ymax = fish_total_num_annual_total_mean + fish_total_num_annual_total_sd), position = position_dodge(width = 0.9), width = 0.25) + 
  geom_text(aes(y = label_y_position,    label = paste0("$", comma(total_value_annual_total_median))),
         size = 7, family = "Arial") + 
  geom_text(aes(y = label_y_position + 50000,     label = paste0(comma(num_6oz_servings_annual_total_median), " servings")),
          size = 7, family = "Arial") + 
  scale_fill_manual(values = c("#4E5A5D", "#C7DDE4"))+
  scale_colour_manual(values = c("black")) +
  theme_classic() +
  ylab("Annual Number of Fish") +
  theme(axis.title.x = element_blank(), legend.position = "none")+
  theme(axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),axis.title.y=element_text(size = 18, face = "bold"), text = element_text(family = "Arial"), strip.background = element_blank()) +
  
  scale_y_continuous(labels = comma) 
 
sport_fish_num

sport_fish_servings <- ggplot(sport_df, aes(x = Scenario, y = num_6oz_servings_annual_total, fill = Scenario)) +
  geom_boxplot() +
  scale_fill_manual(values = c("darkgreen", "lightgreen"))+
  scale_colour_manual(values = c("black")) +
  theme_classic() +
  ylab("Annual 6oz Servings") +
  theme(axis.title.x = element_blank(), legend.position = "none")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank()) +
  
  scale_y_continuous(labels = comma)
sport_fish_servings
##hmm this looks identical, something weird going on 

sport_fish_value <- ggplot(sport_df, aes(x = Scenario, y = total_value_annual_total, fill = Scenario)) +
  geom_boxplot() +
  scale_fill_manual(values = c("darkgreen", "lightgreen"))+
  scale_colour_manual(values = c("black")) +
  theme_classic() +
  ylab("Annual Estimated $") +
  theme(axis.title.x = element_blank(), legend.position = "none")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank()) +
  
  scale_y_continuous(labels = comma)
sport_fish_value

##Harvest proportion pie chart
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    #   plot.title=element_text(size=14, face="bold")
  )
clrs2 = c("#000066", "#CC99FF", "#FFCCFF", "#FF9999","#990000")

sport_df_prop <- df_all_sp_prop %>%
  filter(Fishery == "Sport") %>%
  filter(Scenario == "Liberal")


sport_df_prop$Species <- ordered(sport_df_prop$Species, levels = c("Chinook", "Chum", "Coho", "Pink", "Sockeye"))

sport_piechart <- ggplot(sport_df_prop, aes(x = "", y = sp_prop_harvest, fill = Species)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = clrs2)+
  blank_theme +
  theme(axis.text.x = element_blank(), legend.position = "right", axis.text.y = element_blank(), legend.text = element_text(size = 18, family = "Arial"), legend.title =element_text(size = 20, family = "Arial", face = "bold"))

sport_piechart

library(ggpubr)

sport_plot <- ggarrange(sport_fish_num, sport_piechart,  nrow = 1, ncol = 2, labels = c("a)", "b)"),  font.label = list(size = 20, face = "bold", family = "Arial"))
sport_plot


##personal use + subsistence 
####Commercial plot 
ps_df <- df_all_summary %>%
  filter(Fishery %in% c("Personal use","Subsistence")) %>%
  group_by(Scenario, Year) %>%
  summarise_at(vars(fish_total_num_annual_total:total_value_annual_total), list(sum = sum)) %>%
  ungroup() %>%
  group_by(Scenario) %>%
  summarise_at(vars(fish_total_num_annual_total_sum:total_value_annual_total_sum), list(mean = mean, sd = sd, median = median))
head(ps_df)


ps_df$label_y_position <- ps_df$fish_total_num_annual_total_sum_mean + ps_df$fish_total_num_annual_total_sum_sd + 30000


ps_fish_num <- ggplot(ps_df, aes(x = Scenario, y = fish_total_num_annual_total_sum_mean, fill = Scenario)) +
  geom_col() +
  geom_errorbar(aes(ymin =  fish_total_num_annual_total_sum_mean - fish_total_num_annual_total_sum_sd, ymax = fish_total_num_annual_total_sum_mean + fish_total_num_annual_total_sum_sd), position = position_dodge(width = 0.9), width = 0.25) + 
  geom_text(aes(y = label_y_position,    label = paste0("$", comma(total_value_annual_total_sum_median))),
            size = 7, family = "Arial") + 
  geom_text(aes(y = label_y_position + 30000,     label = paste0(comma(num_6oz_servings_annual_total_sum_median), " servings")),
            size = 7, family = "Arial") + 
  scale_fill_manual(values = c("#4E5A5D", "#C7DDE4"))+
  scale_colour_manual(values = c("black")) +
  theme_classic() +
  ylab("Annual Number of Fish") +
  theme(axis.title.x = element_blank(), legend.position = "none")+
  theme(axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),axis.title.y=element_text(size = 18, face = "bold"), text = element_text(family = "Arial"), strip.background = element_blank()) +
  
  scale_y_continuous(labels = comma) 

ps_fish_num





ps_fish_num <- ggplot(ps_df, aes(x = Scenario, y = fish_total_num_annual_total_sum, fill = Scenario)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#003366", "#6699CC"))+
  scale_colour_manual(values = c("black")) +
  theme_classic() +
  ylab("Annual Number of Fish") +
  theme(axis.title.x = element_blank(), legend.position = "none")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank()) +
  scale_y_continuous(labels = comma)
ps_fish_num

ps_fish_servings <- ggplot(ps_df, aes(x = Scenario, y = num_6oz_servings_annual_total_sum, fill = Scenario)) +
  geom_boxplot() +
  scale_fill_manual(values = c("darkgreen", "lightgreen"))+
  scale_colour_manual(values = c("black")) +
  theme_classic() +
  ylab("Annual 6oz Servings") +
  theme(axis.title.x = element_blank(), legend.position = "none")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank()) +
  scale_y_continuous(labels = comma)
ps_fish_servings

ps_fish_value <- ggplot(ps_df, aes(x = Scenario, y = total_value_annual_total_sum, fill = Scenario)) +
  geom_boxplot() +
  scale_fill_manual(values = c("darkgreen", "lightgreen"))+
  scale_colour_manual(values = c("black")) +
  theme_classic() +
  ylab("Annual Estimated $") +
  theme(axis.title.x = element_blank(), legend.position = "none")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank()) +
  scale_y_continuous(labels = comma)
ps_fish_value

##Harvest proportion pie chart
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    #   plot.title=element_text(size=14, face="bold")
  )
clrs2 = c("#000066", "#CC99FF", "#FFCCFF", "#FF9999","#990000")


df_all_sp_total_2 <- df_all %>%
  filter(Fishery %in% c("Personal use","Subsistence")) %>%
  group_by(Scenario) %>%
  summarise_at(vars(fish_total_num), list(total_num = sum))

df_all_sp_prop_2 <- df_all %>%
  filter(Fishery %in% c("Personal use","Subsistence")) %>%
  group_by(Species, Scenario) %>%
  summarise_at(vars(fish_total_num), list(total_num_sp = sum)) %>%
  left_join(df_all_sp_total_2, by = c("Scenario"))  %>%
  mutate(sp_prop_harvest = total_num_sp/total_num * 100)



ps_df_prop <- df_all_sp_prop_2 %>%
  filter(Scenario == "Liberal")


ps_df_prop$Species <- ordered(ps_df_prop$Species, levels = c("Chinook", "Chum", "Coho", "Pink", "Sockeye"))
##remove salmon part of species names


ps_piechart <- ggplot(ps_df_prop, aes(x = "", y = sp_prop_harvest, fill = Species)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = clrs2)+
  blank_theme +
  theme(axis.text.x = element_blank(), legend.position = "right", axis.text.y = element_blank(), legend.text = element_text(size = 18, family = "Arial"), legend.title =element_text(size = 20, family = "Arial", face = "bold"))

ps_piechart

library(ggpubr)

ps_plot <- ggarrange(ps_fish_num, ps_piechart,  nrow = 1, ncol = 2, labels = c("a)", "b)"),  font.label = list(size = 20, face = "bold", family = "Arial"))
ps_plot




####Commercial plot 
comm_df <- df_all_summary %>%
  filter(Fishery == "Commercial") %>%
  ungroup() %>%
  group_by(Scenario) %>%
  summarise_at(vars(fish_total_num_annual_total:total_value_annual_total), list(mean = mean, sd = sd, median = median))

head(comm_df)


comm_df$label_y_position <- comm_df$fish_total_num_annual_total_mean + comm_df$fish_total_num_annual_total_sd + 5000000


comm_fish_num <- ggplot(comm_df, aes(x = Scenario, y = fish_total_num_annual_total_mean, fill = Scenario)) +
  geom_col() +
  geom_errorbar(aes(ymin =  fish_total_num_annual_total_mean - fish_total_num_annual_total_sd, ymax = fish_total_num_annual_total_mean + fish_total_num_annual_total_sd), position = position_dodge(width = 0.9), width = 0.25) + 
  geom_text(aes(y = label_y_position,    label = paste0("$", comma(total_value_annual_total_median))),
            size = 7, family = "Arial") + 
  geom_text(aes(y = label_y_position + 5000000,     label = paste0(comma(num_6oz_servings_annual_total_median), " servings")),
            size = 7, family = "Arial") + 
  scale_fill_manual(values = c("#4E5A5D", "#C7DDE4"))+
  scale_colour_manual(values = c("black")) +
  theme_classic() +
  ylab("Annual Number of Fish") +
  theme(axis.title.x = element_blank(), legend.position = "none")+
  theme(axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),axis.title.y=element_text(size = 18, face = "bold"), text = element_text(family = "Arial"), strip.background = element_blank()) +
  scale_y_continuous(labels = comma) 

comm_fish_num




comm_fish_num <- ggplot(comm_df, aes(x = Scenario, y = fish_total_num_annual_total, fill = Scenario)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#003366", "#6699CC"))+
  scale_colour_manual(values = c("black")) +
  theme_classic() +
  ylab("Annual Number of Fish") +
  theme(axis.title.x = element_blank(), legend.position = "none")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank()) +
  
  scale_y_continuous(labels = comma)
comm_fish_num

comm_fish_servings <- ggplot(comm_df, aes(x = Scenario, y = num_6oz_servings_annual_total, fill = Scenario)) +
  geom_boxplot() +
  scale_fill_manual(values = c("darkgreen", "lightgreen"))+
  scale_colour_manual(values = c("black")) +
  theme_classic() +
  ylab("Annual 6oz Servings") +
  theme(axis.title.x = element_blank(), legend.position = "none")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank()) +
  
  scale_y_continuous(labels = comma)
comm_fish_servings

comm_fish_value <- ggplot(comm_df, aes(x = Scenario, y = total_value_annual_total, fill = Scenario)) +
  geom_boxplot() +
  scale_fill_manual(values = c("darkgreen", "lightgreen"))+
  scale_colour_manual(values = c("black")) +
  theme_classic() +
  ylab("Annual Estimated $") +
  theme(axis.title.x = element_blank(), legend.position = "none")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank()) +
  
  scale_y_continuous(labels = comma)
comm_fish_value

##Harvest proportion pie chart
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    #   plot.title=element_text(size=14, face="bold")
  )
clrs2 = c("#000066", "#CC99FF", "#FFCCFF", "#FF9999","#990000")

comm_df_prop <- df_all_sp_prop %>%
  filter(Fishery == "Commercial") 


comm_df_prop$Species <- ordered(comm_df_prop$Species, levels = c("Chinook Salmon", "Chum Salmon", "Coho Salmon", "Pink Salmon", "Sockeye Salmon"))
##remove salmon part of species names


comm_piechart <- ggplot(comm_df_prop, aes(x = "", y = sp_prop_harvest, fill = Species)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = clrs2)+
  blank_theme +
  theme(axis.text.x = element_blank(), legend.position = "right", axis.text.y = element_blank(), legend.text = element_text(size = 18, family = "Arial"), legend.title =element_text(size = 20, family = "Arial", face = "bold"))

comm_piechart

library(ggpubr)

comm_plot <- ggarrange(comm_fish_num, comm_piechart,  nrow = 1, ncol = 2, labels = c("a)", "b)"),  font.label = list(size = 20, face = "bold", family = "Arial"))
comm_plot



##to-do: update legend font for pie charts 
##recalculate median values for subsistence + personal use
