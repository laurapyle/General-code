library(readxl)
library(blockrand)

set.seed(3654)

# read in participant info
participants <- read.csv("/Volumes/pylell/Ostendorf/K award/Randomization/Randomization cohort 2/Cohort 2 condensed.csv")

# there are a total of 25 in the cohort, and 8 conditions
# want to randomize 3 people to each of the 8 conditions
# the remaining 1 should be randomized to conditions 5, 6, 7, 8

# first randomly select 1 participants who will be randomized separately to conditions 5, 6, 7, 8
participants.sampled <- participants[sample(1:nrow(participants),1, replace=FALSE),]
participants.notsampled <- participants[!(participants$Record.ID %in% participants.sampled$Record.ID),]

# randomize the 24
participants.notsampled.rand <- blockrand(n=24, levels = c(1:8), block.sizes = 3)
participants.notsampled <- cbind(participants.notsampled, participants.notsampled.rand$treatment)
colnames(participants.notsampled) <- c("Record ID", "Sex", "Race", "Ethnicity", "Condition")

# randomize the 1
participants.sampled.rand <- blockrand(n=1, levels = c(5:8), block.sizes = 1)
participants.sampled.rand <- participants.sampled.rand[sample(1:nrow(participants.sampled.rand),1, replace=FALSE),]
participants.sampled <- cbind(participants.sampled, participants.sampled.rand$treatment)
colnames(participants.sampled) <- c("Record ID", "Sex", "Race", "Ethnicity", "Condition")

# combine
participants.rand <- rbind(participants.sampled, participants.notsampled)
#participants.rand$Age <- floor(participants.rand$Age)

# check
table(participants.rand$Condition)
table(participants.rand$Sex, participants.rand$Condition)
table(participants.rand$Race, participants.rand$Condition)
table(participants.rand$Ethnicity, participants.rand$Condition)

# output
write.csv(participants.rand, 
          "/Volumes/pylell/Ostendorf/K award/Randomization/Randomization cohort 2/Cohort 2 Randomization Condition.csv",
          row.names = F)
