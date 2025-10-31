library(readxl)
library(blockrand)
library(dplyr)

set.seed(8877)

# read in participant info
participants <- read.csv('/Volumes/pylell/Hill/Randomization/23-2151 Cohort 4 Randomization.csv')

# stratify by biological sex and randomize to counseling or no counseling
males <- participants %>% filter(What.was.your.biological.sex.assigned.at.birth. == "Male")
females <- participants %>% filter(What.was.your.biological.sex.assigned.at.birth. == "Female")

# randomize males
males_rand <- blockrand(n=nrow(males), levels = c(0:1), block.sizes = 1)
males_rand <- males_rand[1:nrow(males),]
males <- cbind(males, males_rand$treatment)
males$treatment_char <- ifelse(males$`males_rand$treatment`==1, "Counseling", "No Counseling")
males$`males_rand$treatment` <- NULL

# randomize females
females_rand <- blockrand(n=nrow(females), levels = c(0:1), block.sizes = 1)
females_rand <- females_rand[1:nrow(females),]
females <- cbind(females, females_rand$treatment)
females$treatment_char <- ifelse(females$`females_rand$treatment`==1, "Counseling", "No Counseling")
females$`females_rand$treatment` <- NULL

# combine
rand <- rbind(males, females)

# check
table(rand$treatment_char)
table(rand$What.was.your.biological.sex.assigned.at.birth., rand$treatment_char)

# output
write.csv(rand, 
          "/Volumes/pylell/Hill/Randomization/23-2151 Cohort 4 Randomization with Assignments.csv",
          row.names = F)
