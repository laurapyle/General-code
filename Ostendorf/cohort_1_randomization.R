library(readxl)
library(blockrand)

set.seed(3654)

# read in participant info
participants <- read_xlsx("/Volumes/pylell/Ostendorf/K award/Randomization/Randomization cohort 1/Cohort 1 Randomization.xlsx")

# there are a total of 18 in the cohort, and 8 conditions
# want to randomize 2 people to each of the 8 conditions
# the remaining 2 should be randomized to conditions 5, 6, 7, 8

# first randomly select 2 participants who will be randomized separately to conditions 5, 6, 7, 8
participants.sampled <- participants[sample(1:nrow(participants),2, replace=FALSE),]
participants.notsampled <- participants[!(participants$`Record ID` %in% participants.sampled$`Record ID`),]

# randomize the 16
participants.notsampled.rand <- blockrand(n=16, levels = c(1:8), block.sizes = 2)
participants.notsampled <- cbind(participants.notsampled, participants.notsampled.rand$treatment)
colnames(participants.notsampled) <- c("Record ID", "Age", "Sex", "Condition")

# randomize the 2
participants.sampled.rand <- blockrand(n=2, levels = c(5:8), block.sizes = 1)
participants.sampled.rand <- participants.sampled.rand[sample(1:nrow(participants.sampled.rand),2, replace=FALSE),]
participants.sampled <- cbind(participants.sampled, participants.sampled.rand$treatment)
colnames(participants.sampled) <- c("Record ID", "Age", "Sex", "Condition")

# combine
participants.rand <- rbind(participants.sampled, participants.notsampled)
participants.rand$Age <- floor(participants.rand$Age)

# output
write.csv(participants.rand, 
          "/Volumes/pylell/Ostendorf/K award/Randomization/Randomization cohort 1/Cohort 1 Randomization Condition.csv",
          row.names = F)
