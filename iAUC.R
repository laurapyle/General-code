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
ogttdat <- read.csv("/Users/timvigers/Downloads/CGMInHealthyControls_DATA_2018-12-18_1244.csv")
ogttdat <- ogttdat[,1:12]
ogttdat <- ogttdat[which(rowSums(!is.na(ogttdat))>=4),]

iAUC <- function(data,thresh,type = c("linear","spline"),
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

spline <- iAUC(ogttdat,180,type = "spline")
lin <- iAUC(ogttdat,180,type = "linear")
print(lin)
print(spline)
print(lin-spline)
mean((lin-spline),na.rm = T)
