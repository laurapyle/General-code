library(readxl)
library(blockrand)
library(dplyr)

set.seed(1234586444)

# stratify randomization by cohort and A1c (>=8%, <8%)
# recruiting up to 230 to account for dropout (final N = 200)
# 10-16 per cohort
# will allow for 25 cohorts
# groups are BREATHE-T1D and HealthEd-T1D
# since we don't know how many people will have high or low A1c, for each cohort, generate a full randomization 
# sequence for both high and low A1c (i.e., up to 16)
rand1a <- blockrand(n=16, levels = c(1:2), block.sizes = 1, stratum = "Cohort 1, A1c <8%")
rand1b <- blockrand(n=16, levels = c(1:2), block.sizes = 1, stratum = "Cohort 1, A1c >=8%")



rand <- rbind(rand_males_lt45_bmilt30, rand_males_ge45_bmilt30, rand_males_lt45_bmige30, rand_males_ge45_bmige30,
              rand_females_lt45_bmilt30, rand_females_ge45_bmilt30, rand_females_lt45_bmige30, rand_females_ge45_bmige30)
rand <- rand %>% select(strata, treatment_char)
colnames(rand) <- c("Strata", "Arm (char)")

# output
write.csv(rand, 
          "/Users/pylell/Library/CloudStorage/OneDrive-SharedLibraries-UW/Nowak/ADPKD GLP1-Ra/Randomization/rand_sequence_ADPKD_GLP1Ra.csv",
          row.names = F)



