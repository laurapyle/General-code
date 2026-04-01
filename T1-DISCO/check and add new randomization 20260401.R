library(blockrand)
library(dplyr)

# check balance of current randomizations
rand <- read.csv('/Users/pylell/Library/CloudStorage/OneDrive-UW/T1-DISCO randomization/checking randomization 20260401.csv')
table(rand$Group)
table(rand$Stratum, rand$Group)


set.seed(1234586444)

# create 53 new allocation pairs

# first pair
rand_first <- blockrand(n=53, levels = c(1:2), block.sizes = 2)
rand_first$treatment_char <- ifelse(rand_first$treatment == "1", "PLACEBO", "ACTIVE")

rand <- rand_first %>% select(id, treatment_char)
rand$treatment_char_second <- ifelse(rand$treatment_char == "ACTIVE", "PLACEBO", "ACTIIVE")

# check
table(rand$treatment_char)
table(rand$treatment_char_second)

# output
write.csv(rand, 
          "/Users/pylell/Library/CloudStorage/OneDrive-UW/T1-DISCO randomization/rand_sequence_BMI_25-35.csv",
          row.names = F)


