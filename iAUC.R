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

#spline <- iAUC(ogttdat,180,type = "spline")
#ogttdat$iauc180 <- iAUC(ogttdat,180,type = "linear")
#ogttdat$iauc0 <- iAUC(ogttdat,0,type = "linear")
#ogttdat$iaucbase <- iAUC(ogttdat,ogttdat$lab_ogtt_fasting,type = "linear")
