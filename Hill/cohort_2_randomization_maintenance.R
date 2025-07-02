library(readxl)
library(blockrand)
library(dplyr)

set.seed(1234)

# read in participant info
participants <- read.csv('/Volumes/pylell/Hill/Randomization/Maintenance phase randomization/23-2151 Cohort 2 Randomization with Assignments UPDATE.csv')

# stratify by biological sex and original randomization
males_counseling <- participants %>% filter(What.was.your.biological.sex.assigned.at.birth. == "Male" & treatment_char == "Counseling")
males_no_counseling <- participants %>% filter(What.was.your.biological.sex.assigned.at.birth. == "Male" & treatment_char == "No Counseling")
females_counseling <- participants %>% filter(What.was.your.biological.sex.assigned.at.birth. == "Female" & treatment_char == "Counseling")
females_no_counseling <- participants %>% filter(What.was.your.biological.sex.assigned.at.birth. == "Female" & treatment_char == "No Counseling")

# randomize males, counseling
males_counseling_rand <- blockrand(n=nrow(males_counseling), levels = c(0:1), block.sizes = 1)
males_counseling_rand <- males_counseling_rand[1:nrow(males_counseling),]
males_counseling <- cbind(males_counseling, males_counseling_rand$treatment)
males_counseling$treatment_char <- ifelse(males_counseling$`males_counseling_rand$treatment`==1, "ACT", "No ACT")
males_counseling$`males_counseling_rand$treatment` <- NULL
males_counseling$initial_intervention <- "Counseling"

# randomize males, no counseling
males_no_counseling_rand <- blockrand(n=nrow(males_no_counseling), levels = c(0:1), block.sizes = 1)
males_no_counseling_rand <- males_no_counseling_rand[1:nrow(males_no_counseling),]
males_no_counseling <- cbind(males_no_counseling, males_no_counseling_rand$treatment)
males_no_counseling$treatment_char <- ifelse(males_no_counseling$`males_no_counseling_rand$treatment`==1, "ACT", "No ACT")
males_no_counseling$`males_no_counseling_rand$treatment` <- NULL
males_no_counseling$initial_intervention <- "No Counseling"

# randomize females, counseling
females_counseling_rand <- blockrand(n=nrow(females_counseling), levels = c(0:1), block.sizes = 1)
females_counseling_rand <- females_counseling_rand[1:nrow(females_counseling),]
females_counseling <- cbind(females_counseling, females_counseling_rand$treatment)
females_counseling$treatment_char <- ifelse(females_counseling$`females_counseling_rand$treatment`==1, "ACT", "No ACT")
females_counseling$`females_counseling_rand$treatment` <- NULL
females_counseling$initial_intervention <- "Counseling"

# randomize females, no counseling
females_no_counseling_rand <- blockrand(n=nrow(females_no_counseling), levels = c(0:1), block.sizes = 1)
females_no_counseling_rand <- females_no_counseling_rand[1:nrow(females_no_counseling),]
females_no_counseling <- cbind(females_no_counseling, females_no_counseling_rand$treatment)
females_no_counseling$treatment_char <- ifelse(females_no_counseling$`females_no_counseling_rand$treatment`==1, "ACT", "No ACT")
females_no_counseling$`females_no_counseling_rand$treatment` <- NULL
females_no_counseling$initial_intervention <- "No Counseling"

# combine
rand <- rbind(males_counseling, males_no_counseling, females_counseling, females_no_counseling)

# check
table(rand$treatment_char)
table(rand$What.was.your.biological.sex.assigned.at.birth., rand$treatment_char)
table(rand$What.was.your.biological.sex.assigned.at.birth., rand$treatment_char)

# output
write.csv(rand, 
          "/Volumes/pylell/Hill/Randomization/Maintenance phase randomization/23-2151 Cohort 2 Randomization with Maintenance Assignments.csv",
          row.names = F)
