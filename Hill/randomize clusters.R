# randomize order of cohorts
# there are 4 cohorts, and at this point, we just want to randomize the order
# later we will randomize the individuals within the cohort to receive 1:1 support or not
a <- sample.int(4,4,replace = F)
a <- as.data.frame(a)
names(a) <- c("Cohort")

# write the file
write.csv(a,"/Volumes/pylell/Hill/Randomization/hill_cluster_randomization.csv", row.names = F)
