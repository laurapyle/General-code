library(dplyr)
library(sampling)
library(readxl)

data <- read_xlsx('/Users/lpyle/Library/CloudStorage/OneDrive-UW/Mackey/Fidelity checks/Fidelity Assessment Session List.xlsx')
data$stratum <- paste0(data$Intervention, data$Cohort)

temp <- strata(data=data, stratanames = "stratum", size=rep(1,length(table(data$stratum))), method="srswor")
res <- getdata(data, temp)

res <- res[,c("Intervention", "Cohort", "Session")]

write.csv(res,"/Users/lpyle/Library/CloudStorage/OneDrive-UW/Mackey/Fidelity checks/Fidelity Assessment Session List selected.csv",
          row.names = F)

