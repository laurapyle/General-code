# Library function tests
library(MESS)
library(pracma)
ogtt <- c(108,109,101,100,112,182,232,236,220,173)
tp <- c(-10,0,10,20,30,60,90,120,150,180)
dat <- data.frame(cbind(tp,ogtt))
thresh <- 180
dat$ogtt <- dat$ogtt - thresh
plot(dat$tp,dat$ogtt)
# pracma
trapz(dat$tp,dat$ogtt)
# MESS
auc(dat$tp,dat$ogtt,type = "linear")
auc(dat$tp,dat$ogtt,type = "spline")

# Dataframe function test
# GLUCOSE
ogttdat <- read.csv("H:\\Endocrinology\\Chan\\ADA abstract 2019\\CGMInHealthyControls_DATA_2018-12-21_1936.csv")
cols_gluc = c("min_minus_10_glucose","lab_ogtt_fasting",
         "min_10_glucose","min_20_glucose","min_30_glucose",
         "lab_ogtt_1_hour_glucose","min_90_glucose",
         "lab_ogtt_2_hour_glucose","min_150_glucose",
         "min_180_glucose")
#ogttdat <- ogttdat[,cols]
ogttdat <- ogttdat[which(rowSums(!is.na(ogttdat[,cols_gluc]))>=4),]

iAUC_glucose <- function(data,thresh,type = c("linear","spline"),
                 times = c(-10,0,10,20,30,60,90,120,150,180),
                 cols = c("min_minus_10_glucose","lab_ogtt_fasting",
                       "min_10_glucose","min_20_glucose","min_30_glucose",
                       "lab_ogtt_1_hour_glucose","min_90_glucose",
                       "lab_ogtt_2_hour_glucose","min_150_glucose",
                       "min_180_glucose")) {
  require(MESS)
  ogtt <- data[,cols] - thresh
  if (T %in% (rowSums(!is.na(ogtt)) < 2)) {
    stop("Too much missing data, all rows require at least two numeric values.")
  }
  aucs <- apply(ogtt,1, function(x) auc(times,x,type = type))
  return(aucs)
}

spline <- iAUC_glucose(ogttdat,180,type = "spline")
ogttdat$iauc180 <- iAUC_glucose(ogttdat,180,type = "linear")
ogttdat$iauc0 <- iAUC_glucose(ogttdat,0,type = "linear")
ogttdat$iaucbase <- iAUC_glucose(ogttdat,ogttdat$lab_ogtt_fasting,type = "linear")
View(ogttdat[c(cols,"iauc180","iauc0","iaucbase","ogtt_auc_3_hour")])
print(lin)
print(spline)
print(lin-spline)
mean((lin-spline),na.rm = T)

# INSULIN
ogttdat <- read.csv("H:\\Endocrinology\\Chan\\ADA abstract 2019\\CGMInHealthyControls_DATA_2018-12-21_1936.csv")
cols_ins = c("min_minus_10_insulin","min_0_insulin","min_10_insulin","min_20_insulin","min_30_insulin","min_60_insulin",
             "min_90_insulin","min_120_insulin","min_150_insulin","min_180_insulin")
#ogttdat <- ogttdat[,cols]
ogttdat <- ogttdat[which(rowSums(!is.na(ogttdat[,cols_ins]))>=4),]

iAUC_insulin <- function(data,thresh,type = c("linear","spline"),
                         times = c(-10,0,10,20,30,60,90,120,150,180),
                         cols = c("min_minus_10_insulin","min_0_insulin","min_10_insulin","min_20_insulin","min_30_insulin","min_60_insulin",
                                  "min_90_insulin","min_120_insulin","min_150_insulin","min_180_insulin")) {
  require(MESS)
  ogtt <- data[,cols] - thresh
  if (T %in% (rowSums(!is.na(ogtt)) < 2)) {
    stop("Too much missing data, all rows require at least two numeric values.")
  }
  aucs <- apply(ogtt,1, function(x) auc(times,x,type = type))
  return(aucs)
}
ogttdat$iauc180 <- iAUC_insulin(ogttdat,180,type = "linear")
ogttdat$iauc0 <- iAUC_insulin(ogttdat,0,type = "linear")
ogttdat$iaucbase <- iAUC_insulin(ogttdat,ogttdat$min_0_insulin,type = "linear")
View(ogttdat[c(cols_ins,"iauc180","iauc0","iaucbase")])

# C-PEP
ogttdat <- read.csv("H:\\Endocrinology\\Chan\\ADA abstract 2019\\CGMInHealthyControls_DATA_2018-12-21_1936.csv")
cols_cpep = c("min_minus_10_c_peptide","min_0_c_peptide","min_10_c_peptide","min_20_c_peptide","min_30_c_peptide","min_60_c_peptide",
             "min_90_c_peptide","min_120_c_peptide","min_150_c_peptide","min_180_c_peptide")
#ogttdat <- ogttdat[,cols]
ogttdat <- ogttdat[which(rowSums(!is.na(ogttdat[,cols_cpep]))>=4),]

iAUC_cpep <- function(data,thresh,type = c("linear","spline"),
                         times = c(-10,0,10,20,30,60,90,120,150,180),
                         cols = c("min_minus_10_c_peptide","min_0_c_peptide","min_10_c_peptide","min_20_c_peptide","min_30_c_peptide","min_60_c_peptide",
                                  "min_90_c_peptide","min_120_c_peptide","min_150_c_peptide","min_180_c_peptide")) {
  require(MESS)
  ogtt <- data[,cols] - thresh
  if (T %in% (rowSums(!is.na(ogtt)) < 2)) {
    stop("Too much missing data, all rows require at least two numeric values.")
  }
  aucs <- apply(ogtt,1, function(x) auc(times,x,type = type))
  return(aucs)
}
ogttdat$iauc180 <- iAUC_cpep(ogttdat,180,type = "linear")
ogttdat$iauc0 <- iAUC_cpep(ogttdat,0,type = "linear")
ogttdat$iaucbase <- iAUC_cpep(ogttdat,ogttdat$min_0_c_peptide,type = "linear")
View(ogttdat[c(cols_ins,"iauc180","iauc0","iaucbase")])

# GLUCAGON
ogttdat <- read.csv("H:\\Endocrinology\\Chan\\ADA abstract 2019\\CGMInHealthyControls_DATA_2018-12-21_1936.csv")
cols_glucagon = c("min_minus_10_glucagon","min_0_glucagon","min_10_glucagon","min_20_glucagon","min_30_glucagon","min_60_glucagon",
              "min_90_glucagon","min_120_glucagon","min_150_glucagon","min_180_glucagon")
#ogttdat <- ogttdat[,cols]
ogttdat <- ogttdat[which(rowSums(!is.na(ogttdat[,cols_glucagon]))>=4),]
ogttdat <- ogttdat[which(!is.na(ogttdat[,"min_0_glucagon"])),]

iAUC_glucagon <- function(data,thresh,type = c("linear","spline"),
                      times = c(-10,0,10,20,30,60,90,120,150,180),
                      cols = c("min_minus_10_glucagon","min_0_glucagon","min_10_glucagon","min_20_glucagon","min_30_glucagon","min_60_glucagon",
                               "min_90_glucagon","min_120_glucagon","min_150_glucagon","min_180_glucagon")) {
  require(MESS)
  ogtt <- data[,cols] - thresh
  if (T %in% (rowSums(!is.na(ogtt)) < 2)) {
    stop("Too much missing data, all rows require at least two numeric values.")
  }
  aucs <- apply(ogtt,1, function(x) auc(times,x,type = type))
  return(aucs)
}
ogttdat$iauc180 <- iAUC_glucagon(ogttdat,180,type = "linear")
ogttdat$iauc0 <- iAUC_glucagon(ogttdat,0,type = "linear")
ogttdat$iaucbase <- iAUC_glucagon(ogttdat,ogttdat$min_0_glucagon,type = "linear")

View(ogttdat[c(cols_glucagon,"iauc180","iauc0")])
