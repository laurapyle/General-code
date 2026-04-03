library(readxl)
library(blockrand)
library(dplyr)

set.seed(1234586444)

# goal for the entire study is 30 participants per group
# adding a new stratum of BMI >= 35 - 45
# there are two other strata (<25, 25 - <35), this randomization sequence was done previously
# will generate a sequence of 80 assignments just in case

# MALES
rand_males <- blockrand(n=80, levels = c(1:2), block.sizes = 2)
rand_males$treatment_char <- ifelse(rand_males$treatment == "1", "Placebo", "Semaglutide")
rand_males$sex <- "Male"

# FEMALES
rand_females <- blockrand(n=80, levels = c(1:2), block.sizes = 2)
rand_females$treatment_char <- ifelse(rand_females$treatment == "1", "Placebo", "Semaglutide")
rand_females$sex <- "Female"

rand <- rbind(rand_males, rand_females)
rand <- rand %>% select(id, treatment, treatment_char, sex)
colnames(rand) <- c("Participant Number", "Arm (num)", "Arm (char)", "Sex")

# output
write.csv(rand, 
          "/Volumes/pylell/T1-DISCO/rand_sequence_BMI_35-45.csv",
          row.names = F)



