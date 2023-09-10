# randomize order of clusters 1-4, without 1:1 support
a <- sample.int(4,4,replace = F)

# randomize order of clusters 5-8, with 1:1 support
b <- sample.int(4,4,replace = F)
b <- b + 4
