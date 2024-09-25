##Plotting Figures 

library(tidyverse)
library(ggplot2)

mm_df <- read.csv("code/Marine_Mammals_RC/marine_mammal_harvest_data_all.csv")

##For now focus on harbor seal, stellar sea lion, sea otter
mm_df <- mm_df %>%
  filter(Taxa_lvl5 %in% c("Harbor Seal", "Stellar Sea Lion", "Sea Otter"))

##Estimated total number taken (this has values from both comprehensive and marine mammal surveys)
ggplot(mm_df, aes(x = Study_Year, y = Estimated_Amount_Harvested, color = Community_Name, group = Community_Name)) +
  geom_point(aes(shape = Project_Type), size = 2) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("Estimated Number Harvested (# individuals)") +
  facet_wrap(~Taxa_lvl5, scale = "free")

mm_df %>%
  group_by(Taxa_lvl5, Study_Year) %>%
  summarise_at(vars(Estimated_Amount_Harvested), list(Estimated_Amount_Harvested = sum)) %>%
  ggplot(aes(x = Study_Year, y = Estimated_Amount_Harvested, group = Taxa_lvl5)) +
  geom_point(size = 2) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("Estimated Number Harvested (# individuals)") +
  facet_wrap(~Taxa_lvl5)


##Percent Harvesting
ggplot(mm_df, aes(x = Study_Year, y = Percent_Harvesting, color = Community_Name, group = Community_Name)) +
  geom_point(aes(shape = Project_Type), size = 2) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("% Harvesting") +
  facet_wrap(~Taxa_lvl5, scale = "free")

mm_df %>%
  group_by(Taxa_lvl5, Study_Year) %>%
  filter(!is.na(Percent_Harvesting)) %>%
  summarise_at(vars(Percent_Harvesting), list(Percent_Harvesting_mean = mean, Percent_Harvesting_sd = sd)) %>%
  ggplot(aes(x = Study_Year, y = Percent_Harvesting_mean, group = Taxa_lvl5)) +
  geom_point(size = 2) +
 # geom_errorbar(aes(ymin = Percent_Harvesting_mean - Percent_Harvesting_sd, ymax = Percent_Harvesting_mean + Percent_Harvesting_sd)) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("Average % Harvesting") +
  facet_wrap(~Taxa_lvl5)

##Percent Using
ggplot(mm_df, aes(x = Study_Year, y = Percent_Using, color = Community_Name, group = Community_Name)) +
  geom_point(aes(shape = Project_Type), size = 2) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("% Using") +
  facet_wrap(~Taxa_lvl5, scale = "free")

mm_df %>%
  group_by(Taxa_lvl5, Study_Year) %>%
  filter(!is.na(Percent_Using)) %>%
  summarise_at(vars(Percent_Using), list(Percent_Using_mean = mean, Percent_Using_sd = sd)) %>%
  ggplot(aes(x = Study_Year, y = Percent_Using_mean, group = Taxa_lvl5)) +
  geom_point(size = 2) +
  # geom_errorbar(aes(ymin = Percent_Using_mean - Percent_Using_sd, ymax = Percent_Using_mean + Percent_Using_sd)) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("Average % Using") +
  facet_wrap(~Taxa_lvl5)


##Percent Using
mm_df %>%
  filter(Project_Type != "Marine Mammal Surveys") %>%
  ggplot(aes(x = Study_Year, y = Percent_Giving, color = Community_Name, group = Community_Name)) +
  geom_point(aes(shape = Project_Type), size = 2) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("% Giving") +
  facet_wrap(~Taxa_lvl5, scale = "free")

mm_df %>%
  group_by(Taxa_lvl5, Study_Year) %>%
  filter(!is.na(Percent_Giving)) %>%
  summarise_at(vars(Percent_Giving), list(Percent_Giving_mean = mean, Percent_Giving_sd = sd)) %>%
  ggplot(aes(x = Study_Year, y = Percent_Giving_mean, group = Taxa_lvl5)) +
  geom_point(size = 2) +
  # geom_errorbar(aes(ymin = Percent_Giving_mean - Percent_Giving_sd, ymax = Percent_Giving_mean + Percent_Giving_sd)) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("Average % Giving") +
  facet_wrap(~Taxa_lvl5)


##Percent Receiving
mm_df %>%
  filter(Project_Type != "Marine Mammal Surveys") %>%
  ggplot(aes(x = Study_Year, y = Percent_Receiving, color = Community_Name, group = Community_Name)) +
  geom_point(aes(shape = Project_Type), size = 2) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("% Receiving") +
  facet_wrap(~Taxa_lvl5, scale = "free")

mm_df %>%
  group_by(Taxa_lvl5, Study_Year) %>%
  filter(!is.na(Percent_Receiving)) %>%
  summarise_at(vars(Percent_Receiving), list(Percent_Receiving_mean = mean, Percent_Receiving_sd = sd)) %>%
  ggplot(aes(x = Study_Year, y = Percent_Receiving_mean, group = Taxa_lvl5)) +
  geom_point(size = 2) +
  # geom_errorbar(aes(ymin = Percent_Receiving_mean - Percent_Receiving_sd, ymax = Percent_Receiving_mean + Percent_Receiving_sd)) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("Average % Giving") +
  facet_wrap(~Taxa_lvl5)


##HARBOUR SEAL ---------------------
mm_df_hs <- mm_df %>%
  filter(Taxa_lvl5 == "Harbor Seal")
##Estimated total number taken (this has values from both comprehensive and marine mammal surveys)
mm_df_hs %>%
  filter(!is.na(Estimated_Amount_Harvested)) %>% 
  ggplot(aes(x = Study_Year, y = Estimated_Amount_Harvested, color = Community_Name, group = Community_Name)) +
  geom_point(aes(shape = Project_Type), size = 2) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("Estimated Number Harvested (# individuals)") +
  facet_wrap(~Community_Name, scale = "free")

mm_df_hs %>%
  group_by(Taxa_lvl5, Study_Year) %>%
  summarise_at(vars(Estimated_Amount_Harvested), list(Estimated_Amount_Harvested = sum)) %>%
  ggplot(aes(x = Study_Year, y = Estimated_Amount_Harvested, group = Taxa_lvl5)) +
  geom_point(size = 2) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("Estimated Number Harvested (# individuals)") +
  facet_wrap(~Taxa_lvl5)


##Percent Harvesting
df1 <- mm_df_hs %>%
  filter(!is.na(Percent_Harvesting))
df2 <- mm_df_hs %>%
  filter(!is.na(Percent_Using))
df3 <- mm_df_hs %>%
  filter(!is.na(Percent_Receiving))
df4 <- mm_df_hs %>%
  filter(!is.na(Percent_Giving))

ggplot()+
  geom_line(data = df1, aes(x = Study_Year, y = Percent_Harvesting, group = Taxa_lvl5, color = "% Harvesting")) +
  geom_point(data = df1,aes(x = Study_Year, y = Percent_Harvesting, group = Taxa_lvl5, color = "% Harvesting"), size = 2) +
  geom_line(data = df2,aes(x = Study_Year, y = Percent_Using, group = Taxa_lvl5, color = "% Using")) +
  geom_point(data = df2,aes(x = Study_Year, y = Percent_Using, group = Taxa_lvl5, color = "% Using"), size = 2) +
  geom_line(data = df3, aes(x = Study_Year, y = Percent_Receiving, group = Taxa_lvl5, color = "% Receiving")) +
  geom_point(data = df3,aes(x = Study_Year, y = Percent_Receiving, group = Taxa_lvl5, color = "% Receiving"), size = 2) +
  geom_line(data = df4,aes(x = Study_Year, y = Percent_Giving, group = Taxa_lvl5, color = "% Giving")) +
  geom_point(data = df4,aes(x = Study_Year, y = Percent_Giving, group = Taxa_lvl5, color = "% Giving"),size = 2) +
  scale_color_manual(values = c("% Harvesting" = "darkred", "% Using" = "darkblue", "% Receiving" = "darkgreen", "% Giving" = "orange"), name = "Harvesting Type") +
  scale_x_continuous(breaks = seq(min(mm_df_hs_summary1$Study_Year), max(mm_df_hs_summary1$Study_Year), by = 2))  +
  theme_classic() +
  xlab("Year") +
  ylab("%") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~Community_Name)


##Average across all communities 
mm_df_hs_summary1 <- mm_df_hs %>%
  group_by(Taxa_lvl5, Study_Year) %>%
   filter(!is.na(Percent_Harvesting)) %>%
   filter(!is.na(Percent_Using)) %>%
  summarise_at(vars(Percent_Harvesting, Percent_Using), list(mean = mean, sd = sd))

mm_df_hs_summary2 <- mm_df_hs %>%
  group_by(Taxa_lvl5, Study_Year) %>%
  filter(!is.na(Percent_Giving)) %>%
  filter(!is.na(Percent_Receiving)) %>%
  summarise_at(vars(Percent_Giving, Percent_Receiving), list(mean = mean, sd = sd))

ggplot()+
    geom_line(data = mm_df_hs_summary1, aes(x = Study_Year, y = Percent_Harvesting_mean, group = Taxa_lvl5, color = "% Harvesting")) +
    geom_point(data = mm_df_hs_summary1,aes(x = Study_Year, y = Percent_Harvesting_mean, group = Taxa_lvl5, color = "% Harvesting"), size = 2) +
    geom_line(data = mm_df_hs_summary1,aes(x = Study_Year, y = Percent_Using_mean, group = Taxa_lvl5, color = "% Using")) +
    geom_point(data = mm_df_hs_summary1,aes(x = Study_Year, y = Percent_Using_mean, group = Taxa_lvl5, color = "% Using"), size = 2) +
    geom_line(data = mm_df_hs_summary2, aes(x = Study_Year, y = Percent_Receiving_mean, group = Taxa_lvl5, color = "% Receiving")) +
    geom_point(data = mm_df_hs_summary2,aes(x = Study_Year, y = Percent_Receiving_mean, group = Taxa_lvl5, color = "% Receiving"), size = 2) +
    geom_line(data = mm_df_hs_summary2,aes(x = Study_Year, y = Percent_Giving_mean, group = Taxa_lvl5, color = "% Giving")) +
    geom_point(data = mm_df_hs_summary2,aes(x = Study_Year, y = Percent_Giving_mean, group = Taxa_lvl5, color = "% Giving"),size = 2) +
    scale_color_manual(values = c("% Harvesting" = "darkred", "% Using" = "darkblue", "% Receiving" = "darkgreen", "% Giving" = "orange"), name = "Harvesting Type") +
  scale_x_continuous(breaks = seq(min(mm_df_hs_summary1$Study_Year), max(mm_df_hs_summary1$Study_Year), by = 2))  +
   theme_classic() +
  xlab("Year") +
  ylab("Average %") +
  facet_wrap(~Taxa_lvl5)


##SEA LION ---------------------
mm_df_hs <- mm_df %>%
  filter(Taxa_lvl5 == "Stellar Sea Lion")
##Estimated total number taken (this has values from both comprehensive and marine mammal surveys)
mm_df_hs %>%
  filter(!is.na(Estimated_Amount_Harvested)) %>% 
  ggplot(aes(x = Study_Year, y = Estimated_Amount_Harvested, color = Community_Name, group = Community_Name)) +
  geom_point(aes(shape = Project_Type), size = 2) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("Estimated Number Harvested (# individuals)") +
#  xlim(0, ymax) +
  facet_wrap(~Community_Name, scale = "free")

mm_df_hs %>%
  group_by(Taxa_lvl5, Study_Year) %>%
  summarise_at(vars(Estimated_Amount_Harvested), list(Estimated_Amount_Harvested = sum)) %>%
  ggplot(aes(x = Study_Year, y = Estimated_Amount_Harvested, group = Taxa_lvl5)) +
  geom_point(size = 2) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("Estimated Number Harvested (# individuals)") +
  facet_wrap(~Taxa_lvl5)


##Percent Harvesting
df1 <- mm_df_hs %>%
  filter(!is.na(Percent_Harvesting))
df2 <- mm_df_hs %>%
  filter(!is.na(Percent_Using))
df3 <- mm_df_hs %>%
  filter(!is.na(Percent_Receiving))
df4 <- mm_df_hs %>%
  filter(!is.na(Percent_Giving))

ggplot()+
  geom_line(data = df1, aes(x = Study_Year, y = Percent_Harvesting, group = Taxa_lvl5, color = "% Harvesting")) +
  geom_point(data = df1,aes(x = Study_Year, y = Percent_Harvesting, group = Taxa_lvl5, color = "% Harvesting"), size = 2) +
  geom_line(data = df2,aes(x = Study_Year, y = Percent_Using, group = Taxa_lvl5, color = "% Using")) +
  geom_point(data = df2,aes(x = Study_Year, y = Percent_Using, group = Taxa_lvl5, color = "% Using"), size = 2) +
  geom_line(data = df3, aes(x = Study_Year, y = Percent_Receiving, group = Taxa_lvl5, color = "% Receiving")) +
  geom_point(data = df3,aes(x = Study_Year, y = Percent_Receiving, group = Taxa_lvl5, color = "% Receiving"), size = 2) +
  geom_line(data = df4,aes(x = Study_Year, y = Percent_Giving, group = Taxa_lvl5, color = "% Giving")) +
  geom_point(data = df4,aes(x = Study_Year, y = Percent_Giving, group = Taxa_lvl5, color = "% Giving"),size = 2) +
  scale_color_manual(values = c("% Harvesting" = "darkred", "% Using" = "darkblue", "% Receiving" = "darkgreen", "% Giving" = "orange"), name = "Harvesting Type") +
  scale_x_continuous(breaks = seq(min(mm_df_hs_summary1$Study_Year), max(mm_df_hs_summary1$Study_Year), by = 2))  +
  theme_classic() +
  xlab("Year") +
  ylab("%") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~Community_Name, scale = "free")


##Average across all communities 
mm_df_hs_summary1 <- mm_df_hs %>%
  group_by(Taxa_lvl5, Study_Year) %>%
  filter(!is.na(Percent_Harvesting)) %>%
  filter(!is.na(Percent_Using)) %>%
  summarise_at(vars(Percent_Harvesting, Percent_Using), list(mean = mean, sd = sd))

mm_df_hs_summary2 <- mm_df_hs %>%
  group_by(Taxa_lvl5, Study_Year) %>%
  filter(!is.na(Percent_Giving)) %>%
  filter(!is.na(Percent_Receiving)) %>%
  summarise_at(vars(Percent_Giving, Percent_Receiving), list(mean = mean, sd = sd))

ggplot()+
  geom_line(data = mm_df_hs_summary1, aes(x = Study_Year, y = Percent_Harvesting_mean, group = Taxa_lvl5, color = "% Harvesting")) +
  geom_point(data = mm_df_hs_summary1,aes(x = Study_Year, y = Percent_Harvesting_mean, group = Taxa_lvl5, color = "% Harvesting"), size = 2) +
  geom_line(data = mm_df_hs_summary1,aes(x = Study_Year, y = Percent_Using_mean, group = Taxa_lvl5, color = "% Using")) +
  geom_point(data = mm_df_hs_summary1,aes(x = Study_Year, y = Percent_Using_mean, group = Taxa_lvl5, color = "% Using"), size = 2) +
  geom_line(data = mm_df_hs_summary2, aes(x = Study_Year, y = Percent_Receiving_mean, group = Taxa_lvl5, color = "% Receiving")) +
  geom_point(data = mm_df_hs_summary2,aes(x = Study_Year, y = Percent_Receiving_mean, group = Taxa_lvl5, color = "% Receiving"), size = 2) +
  geom_line(data = mm_df_hs_summary2,aes(x = Study_Year, y = Percent_Giving_mean, group = Taxa_lvl5, color = "% Giving")) +
  geom_point(data = mm_df_hs_summary2,aes(x = Study_Year, y = Percent_Giving_mean, group = Taxa_lvl5, color = "% Giving"),size = 2) +
  scale_color_manual(values = c("% Harvesting" = "darkred", "% Using" = "darkblue", "% Receiving" = "darkgreen", "% Giving" = "orange"), name = "Harvesting Type") +
  scale_x_continuous(breaks = seq(min(mm_df_hs_summary1$Study_Year), max(mm_df_hs_summary1$Study_Year), by = 2))  +
  theme_classic() +
  xlab("Year") +
  ylab("Average %") +
  facet_wrap(~Taxa_lvl5)


##SEA OTTER ---------------------
mm_df_hs <- mm_df %>%
  filter(Taxa_lvl5 == "Sea Otter")
##Estimated total number taken (this has values from both comprehensive and marine mammal surveys)
mm_df_hs %>%
  filter(!is.na(Estimated_Amount_Harvested)) %>% 
  ggplot(aes(x = Study_Year, y = Estimated_Amount_Harvested, color = Community_Name, group = Community_Name)) +
  geom_point(aes(shape = Project_Type), size = 2) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("Estimated Number Harvested (# individuals)") +
  #  xlim(0, ymax) +
  facet_wrap(~Community_Name, scale = "free")

mm_df_hs %>%
  group_by(Taxa_lvl5, Study_Year) %>%
  summarise_at(vars(Estimated_Amount_Harvested), list(Estimated_Amount_Harvested = sum)) %>%
  ggplot(aes(x = Study_Year, y = Estimated_Amount_Harvested, group = Taxa_lvl5)) +
  geom_point(size = 2) +
  geom_line() +
  theme_classic() +
  xlab("Year") +
  ylab("Estimated Number Harvested (# individuals)") +
  facet_wrap(~Taxa_lvl5)


##Percent Harvesting
df1 <- mm_df_hs %>%
  filter(!is.na(Percent_Harvesting))
df2 <- mm_df_hs %>%
  filter(!is.na(Percent_Using))
df3 <- mm_df_hs %>%
  filter(!is.na(Percent_Receiving))
df4 <- mm_df_hs %>%
  filter(!is.na(Percent_Giving))

ggplot()+
  geom_line(data = df1, aes(x = Study_Year, y = Percent_Harvesting, group = Taxa_lvl5, color = "% Harvesting")) +
  geom_point(data = df1,aes(x = Study_Year, y = Percent_Harvesting, group = Taxa_lvl5, color = "% Harvesting"), size = 2) +
  geom_line(data = df2,aes(x = Study_Year, y = Percent_Using, group = Taxa_lvl5, color = "% Using")) +
  geom_point(data = df2,aes(x = Study_Year, y = Percent_Using, group = Taxa_lvl5, color = "% Using"), size = 2) +
  geom_line(data = df3, aes(x = Study_Year, y = Percent_Receiving, group = Taxa_lvl5, color = "% Receiving")) +
  geom_point(data = df3,aes(x = Study_Year, y = Percent_Receiving, group = Taxa_lvl5, color = "% Receiving"), size = 2) +
  geom_line(data = df4,aes(x = Study_Year, y = Percent_Giving, group = Taxa_lvl5, color = "% Giving")) +
  geom_point(data = df4,aes(x = Study_Year, y = Percent_Giving, group = Taxa_lvl5, color = "% Giving"),size = 2) +
  scale_color_manual(values = c("% Harvesting" = "darkred", "% Using" = "darkblue", "% Receiving" = "darkgreen", "% Giving" = "orange"), name = "Harvesting Type") +
  scale_x_continuous(breaks = seq(min(mm_df_hs_summary1$Study_Year), max(mm_df_hs_summary1$Study_Year), by = 2))  +
  theme_classic() +
  xlab("Year") +
  ylab("%") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~Community_Name, scale = "free")


##Average across all communities 
mm_df_hs_summary1 <- mm_df_hs %>%
  group_by(Taxa_lvl5, Study_Year) %>%
  filter(!is.na(Percent_Harvesting)) %>%
  filter(!is.na(Percent_Using)) %>%
  summarise_at(vars(Percent_Harvesting, Percent_Using), list(mean = mean, sd = sd))

mm_df_hs_summary2 <- mm_df_hs %>%
  group_by(Taxa_lvl5, Study_Year) %>%
  filter(!is.na(Percent_Giving)) %>%
  filter(!is.na(Percent_Receiving)) %>%
  summarise_at(vars(Percent_Giving, Percent_Receiving), list(mean = mean, sd = sd))

ggplot()+
  geom_line(data = mm_df_hs_summary1, aes(x = Study_Year, y = Percent_Harvesting_mean, group = Taxa_lvl5, color = "% Harvesting")) +
  geom_point(data = mm_df_hs_summary1,aes(x = Study_Year, y = Percent_Harvesting_mean, group = Taxa_lvl5, color = "% Harvesting"), size = 2) +
  geom_line(data = mm_df_hs_summary1,aes(x = Study_Year, y = Percent_Using_mean, group = Taxa_lvl5, color = "% Using")) +
  geom_point(data = mm_df_hs_summary1,aes(x = Study_Year, y = Percent_Using_mean, group = Taxa_lvl5, color = "% Using"), size = 2) +
  geom_line(data = mm_df_hs_summary2, aes(x = Study_Year, y = Percent_Receiving_mean, group = Taxa_lvl5, color = "% Receiving")) +
  geom_point(data = mm_df_hs_summary2,aes(x = Study_Year, y = Percent_Receiving_mean, group = Taxa_lvl5, color = "% Receiving"), size = 2) +
  geom_line(data = mm_df_hs_summary2,aes(x = Study_Year, y = Percent_Giving_mean, group = Taxa_lvl5, color = "% Giving")) +
  geom_point(data = mm_df_hs_summary2,aes(x = Study_Year, y = Percent_Giving_mean, group = Taxa_lvl5, color = "% Giving"),size = 2) +
  scale_color_manual(values = c("% Harvesting" = "darkred", "% Using" = "darkblue", "% Receiving" = "darkgreen", "% Giving" = "orange"), name = "Harvesting Type") +
  scale_x_continuous(breaks = seq(min(mm_df_hs_summary1$Study_Year), max(mm_df_hs_summary1$Study_Year), by = 2))  +
  theme_classic() +
  xlab("Year") +
  ylab("Average %") +
  facet_wrap(~Taxa_lvl5)
