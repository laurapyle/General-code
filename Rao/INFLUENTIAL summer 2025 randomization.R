library(readxl)
library(blockrand)
library(dplyr)

set.seed(3654)

# USUAL CARE SITES
sites_usual <- as.data.frame(c("WashU", "USF", "Arkansas", "Texas"))
colnames(sites_usual) <- "site"
# randomize 2 to intervention A and 2 to intervention B
sites_usual_temp <- blockrand(n=nrow(sites_usual), levels = c(0:1), block.sizes = 2)
sites_usual <- cbind(sites_usual, sites_usual_temp)
# code 0 as A and 1 as B
sites_usual$intervention <- ifelse(sites_usual$treatment == 0, "A", "B")
sites_usual$site_type <- "Usual Care"

# INTERVENTION - HIGH PERFORMING
sites_intervention_high <- as.data.frame(c("Hawaii", "WakeForest"))
colnames(sites_intervention_high) <- "site"
# randomize 1 to intervention A and 1 to intervention B
sites_intervention_high_temp <- blockrand(n=nrow(sites_intervention_high), levels = c(0:1), block.sizes = 1)
sites_intervention_high <- cbind(sites_intervention_high, sites_intervention_high_temp)
# code 0 as A and 1 as B
sites_intervention_high$intervention <- ifelse(sites_intervention_high$treatment == 0, "A", "B")
sites_intervention_high$site_type <- "Intervention - High Performing"

# INTERVENTION - LOW PERFORMING
sites_intervention_low <- as.data.frame(c("Emory", "OHSU", "MCW", "Iowa", "Nebraska"))
colnames(sites_intervention_low) <- "site"
# randomize 2 to intervention A and 3 to intervention B
sites_intervention_low_temp <- blockrand(n=nrow(sites_intervention_low), levels = c(0:1), block.sizes = c(3))
# remove 1 assignment in A
sites_intervention_low_temp <- sites_intervention_low_temp[-which(sites_intervention_low_temp$treatment == 0)[1], ]
sites_intervention_low <- cbind(sites_intervention_low, sites_intervention_low_temp)
# code 0 as A and 1 as B
sites_intervention_low$intervention <- ifelse(sites_intervention_low$treatment == 0, "A", "B")
sites_intervention_low$site_type <- "Intervention - Low Performing"

# combine
rand <- rbind(sites_usual, sites_intervention_high, sites_intervention_low)
rand <- rand %>% select(site, site_type, intervention)

# save
write.csv(rand,'/Users/laurapyle/Library/CloudStorage/OneDrive-UW/Rao/Randomization summer 2025/INFLUENTIAL trial randomization.csv', row.names = F)

