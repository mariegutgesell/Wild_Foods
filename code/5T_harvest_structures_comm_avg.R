##Generate Harvest Structures for Each Community (averaged across years) 

##source code that calculates average harvest across time for each community
source("code/3T_calculate_avg_community_harvest.R")
rm(list = ls()[!ls() %in% c("df_comm_avg")])
