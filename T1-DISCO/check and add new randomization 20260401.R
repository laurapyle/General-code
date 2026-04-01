rand <- read.csv('/Users/pylell/Library/CloudStorage/OneDrive-UW/T1-DISCO randomization/checking randomization 20260401.csv')
table(rand$Group)
table(rand$Stratum, rand$Group)
