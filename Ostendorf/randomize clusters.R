# randomize order of clusters 1-4, without 1:1 support
a <- sample.int(4,4,replace = F)

# randomize order of clusters 5-8, with 1:1 support
b <- sample.int(4,4,replace = F)
b <- b + 4

final <- as.data.frame(cbind(a,b))
names(final) <- c("without_one_to_one","with_one_to_one")

# write the file
write.csv(final,"/Volumes/pylell/Ostendorf/K award/Randomization/cluster_randomization.csv", row.names = F)