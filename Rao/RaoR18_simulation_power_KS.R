##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Project: Suchitra Rao's R18 (SMART design) 
## Created: January 16, 2021
## Author: Krithika Suresh
## Collaborators: Suchitra Rao, Tina Studts
## Description: Power calculation for CRT and SMART CRT
## Outputs: 
## Updates: 
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list=ls())
.libPaths("C:/Users/sureshk/Downloads/RLibrary")
require("lme4") #glmer

#From Mandy (email 1/9/2020):
# In the original trial I think the results were something like intervention group: baseline rate 39%, end rate 55% 
# so total change for intervention was 16 PP.  
# Control baseline rate was 39% and end rate was 41%, so total change in control was 6PP.  
# The difference in differences between intervention and control was 10PP
# So in the non inferiority trial I guess I would hope for something like this.
# Original intervention - baseline 39%, end rate 55% so total change 16 PP (assuming same baseline and same effect as in the first study)
# Revised intervention - baseline 39%, end rate at least 52%, so total change is 13pp.  So in this scenario a pp change of at least 13pp would be considered non=inferior if the original intervention change was 16pp


# Looking at 12 clusters (2:1)  -------------------------------------------
# Smaller ICC leads to increased power!
# Reported in grant Aim 2: >90% power to detect a difference-in-differences of 8%

power_dat <- NULL
# for(icc in c(0.02,0.03,0.05))
# for(n.ppl in c(100, 200, 500))
for(inc in c(0.08))
{
  ctrl.base <- 0.35
  ctrl.end <- ctrl.base
  tmt.base <- ctrl.base
  tmt.end <- tmt.base + inc
  n.ppl <- 500
  icc <- 0.01
  alpha <- 0.05
  
  ret_vec<-NULL
  icc_vec<-NULL
  for(i in 1:500) {
    set.seed(i)
    ctrl.diff<-ctrl.end-ctrl.base
    tmt.diff<-tmt.end-tmt.base
    diff.diff<-tmt.diff-ctrl.diff
    
    n.clust.tot <- 12 #number of clusters
    prop.clust.arm <- 4/12 #proportion of clusters in control arm
    # n.ppl <- 100 #number of people per cluster
    # prop.0 <- 0.3 #proportion in current group
    # # diff.abs <- 0.1 #absolute detectable difference of differences
    # icc<-0.01
    
    b0 <- log(ctrl.base/(1-ctrl.base)) #coefficient for intercept of regression model. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b1 <- log(tmt.base/(1-tmt.base))-b0 #coefficient for the exposure variable (e.g., intervention) of regression. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b2 <- log(ctrl.end/(1-ctrl.end))-b0 #coefficient for covariate variable (e.g., time) of regression. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b3 <- log(tmt.end/(1-tmt.end))-b0-b1-b2 #coefficient for the exposure-continuous interaction variable. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    sigma2<-pi^2/3 #ctrl.base*(1-ctrl.base)/n.ppl
    
    tau2<-icc/(1-icc)*sigma2
    
    ntim<-2 #number of time points (pre, post)
    
    # generate sample trial dataset
    time<-rep(seq(1,ntim)-1,n.clust.tot)
    cluster<-rep(seq(1,n.clust.tot),each=ntim)
    trt<-c(rep(0,n.clust.tot*prop.clust.arm*ntim),rep(1,n.clust.tot*(1-prop.clust.arm)*ntim))
    ctt1<-cbind(cluster,time,trt)
    
    # include random cluster effect 'b'
    b <- rnorm(n.clust.tot,mean=0,sd=sqrt(tau2))
    vecb<-rep(b,each=ntim)
    ctt2<-cbind(ctt1,"b"=vecb)
    
    # create muij (b0+b1*tmt+b2*time+b3*tmt*time+b)
    muij<-b0+b1*ctt2[,"trt"]+b2*ctt2[,"time"]+b3*ctt2[,"trt"]*ctt2[,"time"]+ctt2[,"b"]
    ctt3<-cbind(ctt2,muij)
    
    # expand table by sample size per cluster to get one row per participant
    ctt4<-ctt3[rep(1:nrow(ctt3), times = n.ppl), ]
    N<-n.ppl*n.clust.tot*ntim
    
    # create yijk - individual level responses
    yij<-ctt4[,"muij"]
    probij<-exp(yij)/(1+exp(yij))
    # runis <- runif(N,0,1)
    # resp <- ifelse(runis<probij,1,0)
    resp <- rbinom(N,1,probij)
    ctt5<-data.frame(cbind(ctt4,yij,probij,resp))
    
    # calculate by group means
    grpmeans<-aggregate(ctt5[, "resp"], list("trt"=ctt5$trt,"time"=ctt5$time), mean)
    # Check that expits are equivalent to the inputted rates
    # trt time          x
    # 1   0    0 0.10706019
    # 2   1    0 0.09797454
    # 3   0    1 0.10694444
    # 4   1    1 0.03032407
    
    # linear mixed model - treatment, time fixed effects, interaction tmt*time, cluster random effects
    savelme<-glmer(resp~trt+factor(time)+trt*factor(time)+(1|cluster),data=ctt5,family=binomial(link="logit"))
    vartrtbeta<-vcov(savelme)["trt:factor(time)1","trt:factor(time)1"]
    coefs<-fixef(savelme)
    obsinteff<-coefs["trt:factor(time)1"]
    
    varests<-unlist(VarCorr(savelme)) #use this to get estimated ICC
    esticc<-varests/(varests+pi^2/3)
    icc_vec<-c(icc_vec,esticc)
    # print(esticc)
    # # calculate (theoretical) power given estimated treatement effect variance
    # power <- pnorm((abs(b3)/sqrt(vartrtbeta))-qnorm(1-(alpha/2)))
    
    #Assess non-inferiority
    # lcl<-confint(savelme,parm="beta_",method="Wald",level=0.95)["trt:factor(time)1",][1]
    # ucl<-confint(savelme,parm="beta_",method="Wald",level=0.95)["trt:factor(time)1",][2]
    # if(lcl>0 | ucl<0) {ret<-1} else {ret<-0}
    pval<-summary(savelme)$coef["trt:factor(time)1","Pr(>|z|)"]
    if(pval<=0.05) {ret<-1} else {ret<-0}
    ret_vec<-c(ret_vec,ret)
    print(i)
  }
  
  power_dat<-rbind(power_dat,c("ctrl.base"=ctrl.base,"ctrl.diff"=ctrl.diff,"tmt.diff"=tmt.diff,"n.ppl"=n.ppl,"icc"=icc,"icc_est"=mean(icc_vec),"power"=sum(ret_vec)/length(ret_vec)*100))
  print(c("ctrl.base"=ctrl.base,"ctrl.diff"=ctrl.diff,"tmt.diff"=tmt.diff,"n.ppl"=n.ppl,"icc"=icc,"icc_est"=mean(icc_vec),"power"=sum(ret_vec)/length(ret_vec)*100))
}

names(power_dat) <- c("ctrl.diff","tmt.diff","n.ppl","icc","icc_est","power")
# ctrl.diff tmt.diff n.ppl  icc     icc_est power
# 1         0      0.1   100 0.01 0.007038996  60.5
# 2         0      0.1   200 0.01 0.007359575  87.0
# 3         0      0.1   500 0.01 0.007525162 100.0

# ctrl.base    ctrl.diff     tmt.diff        n.ppl          icc      icc_est    power
# 0.50000000   0.00000000   0.03000000 500.00000000   0.02000000   0.01497958   31.00000000  
# ctrl.base     ctrl.diff      tmt.diff         n.ppl           icc       icc_est     power
# 0.500000000   0.000000000   0.050000000 500.000000000   0.010000000   0.007393723   73.000000000
# ctrl.base     ctrl.diff      tmt.diff         n.ppl           icc       icc_est 
# 0.500000000   0.000000000   0.070000000 500.000000000   0.010000000   0.007426107 
# power 
# 96.000000000  
# 
# ctrl.base     ctrl.diff      tmt.diff         n.ppl           icc       icc_est 
# 0.500000000   0.000000000   0.080000000 500.000000000   0.010000000   0.007865017 
# power 
# 98.200000000 

# Looking at 10 clusters (3:2) --------------------------------------------
ctrl.base<-0.5
tmt.base<-0.5
ctrl.end<-0.5
alpha <- 0.05
n.ppl <- 500
icc <- 0.01
tmt.end <- 0.57

# for(icc in c(0.02,0.03,0.05))
for(n.ppl in c(500))
{
  ret_vec<-NULL
  icc_vec<-NULL
  for(i in 1:100) {
    set.seed(i)
    ctrl.diff<-ctrl.end-ctrl.base
    tmt.diff<-tmt.end-tmt.base
    diff.diff<-tmt.diff-ctrl.diff
    
    n.clust.tot <- 10 #number of clusters
    prop.clust.arm <- 4/10 #proportion of clusters in control arm
    # n.ppl <- 100 #number of people per cluster
    # prop.0 <- 0.3 #proportion in current group
    # # diff.abs <- 0.1 #absolute detectable difference of differences
    # icc<-0.01
    
    b0 <- log(ctrl.base/(1-ctrl.base)) #coefficient for intercept of regression model. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b1 <- log(tmt.base/(1-tmt.base))-b0 #coefficient for the exposure variable (e.g., intervention) of regression. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b2 <- log(ctrl.end/(1-ctrl.end))-b0 #coefficient for covariate variable (e.g., time) of regression. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b3 <- log(tmt.end/(1-tmt.end))-b0-b1-b2 #coefficient for the exposure-continuous interaction variable. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    sigma2<-pi^2/3 #ctrl.base*(1-ctrl.base)/n.ppl
    
    tau2<-icc/(1-icc)*sigma2
    
    ntim<-2 #number of time points (pre, post)
    
    # generate sample trial dataset
    time<-rep(seq(1,ntim)-1,n.clust.tot)
    cluster<-rep(seq(1,n.clust.tot),each=ntim)
    trt<-c(rep(0,n.clust.tot*prop.clust.arm*ntim),rep(1,n.clust.tot*(1-prop.clust.arm)*ntim))
    ctt1<-cbind(cluster,time,trt)
    
    # include random cluster effect 'b'
    b <- rnorm(n.clust.tot,mean=0,sd=sqrt(tau2))
    vecb<-rep(b,each=ntim)
    ctt2<-cbind(ctt1,"b"=vecb)
    
    # create muij (b0+b1*tmt+b2*time+b3*tmt*time+b)
    muij<-b0+b1*ctt2[,"trt"]+b2*ctt2[,"time"]+b3*ctt2[,"trt"]*ctt2[,"time"]+ctt2[,"b"]
    ctt3<-cbind(ctt2,muij)
    
    # expand table by sample size per cluster to get one row per participant
    ctt4<-ctt3[rep(1:nrow(ctt3), times = n.ppl), ]
    N<-n.ppl*n.clust.tot*ntim
    
    # create yijk - individual level responses
    yij<-ctt4[,"muij"]
    probij<-exp(yij)/(1+exp(yij))
    # runis <- runif(N,0,1)
    # resp <- ifelse(runis<probij,1,0)
    resp <- rbinom(N,1,probij)
    ctt5<-data.frame(cbind(ctt4,yij,probij,resp))
    
    # calculate by group means
    grpmeans<-aggregate(ctt5[, "resp"], list("trt"=ctt5$trt,"time"=ctt5$time), mean)
    # Check that expits are equivalent to the inputted rates
    # trt time          x
    # 1   0    0 0.10706019
    # 2   1    0 0.09797454
    # 3   0    1 0.10694444
    # 4   1    1 0.03032407
    
    # linear mixed model - treatment, time fixed effects, interaction tmt*time, cluster random effects
    savelme<-glmer(resp~trt+factor(time)+trt*factor(time)+(1|cluster),data=ctt5,family=binomial(link="logit"))
    vartrtbeta<-vcov(savelme)["trt:factor(time)1","trt:factor(time)1"]
    coefs<-fixef(savelme)
    obsinteff<-coefs["trt:factor(time)1"]
    
    varests<-unlist(VarCorr(savelme)) #use this to get estimated ICC
    esticc<-varests/(varests+pi^2/3)
    icc_vec<-c(icc_vec,esticc)
    # print(esticc)
    # # calculate (theoretical) power given estimated treatement effect variance
    # power <- pnorm((abs(b3)/sqrt(vartrtbeta))-qnorm(1-(alpha/2)))
    
    #Assess non-inferiority
    # lcl<-confint(savelme,parm="beta_",method="Wald",level=0.95)["trt:factor(time)1",][1]
    # ucl<-confint(savelme,parm="beta_",method="Wald",level=0.95)["trt:factor(time)1",][2]
    # if(lcl>0 | ucl<0) {ret<-1} else {ret<-0}
    pval<-summary(savelme)$coef["trt:factor(time)1","Pr(>|z|)"]
    if(pval<=0.05) {ret<-1} else {ret<-0}
    ret_vec<-c(ret_vec,ret)
    print(i)
  }
  
  # power_dat<-rbind(power_dat,c("ctrl.diff"=ctrl.diff,"tmt.diff"=tmt.diff,"n.ppl"=n.ppl,"icc"=icc,"icc_est"=mean(icc_vec),"power"=sum(ret_vec)/length(ret_vec)*100))
  print(c("ctrl.diff"=ctrl.diff,"tmt.diff"=tmt.diff,"n.ppl"=n.ppl,"icc"=icc,"icc_est"=mean(icc_vec),"power"=sum(ret_vec)/length(ret_vec)*100))
}

# ctrl.diff      tmt.diff         n.ppl           icc       icc_est         power 
# 0.000000000   0.100000000 500.000000000   0.010000000   0.008115881  98.600000000

# ctrl.diff     tmt.diff        n.ppl          icc      icc_est        power 
# 0.00000000   0.10000000 500.00000000   0.02000000   0.01672608  99.40000000
# ctrl.diff      tmt.diff         n.ppl           icc       icc_est         power 
# 0.000000000   0.100000000 500.000000000   0.010000000   0.007456601 100.000000000 

# Surveys -----------------------------------------------------------------
# 100 surveys per site 
power_dat <- NULL
for(inc in c(0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2))
{
  ctrl.base <- 0.5
  ctrl.end <- ctrl.base
  tmt.base <- ctrl.base
  tmt.end <- tmt.base + inc
  #n.ppl <- 100
  n.ppl <- 60
  icc <- 0.01
  alpha <- 0.05
  ret_vec<-NULL
  icc_vec<-NULL
  for(i in 1:200) {
    set.seed(i)
    ctrl.diff<-ctrl.end-ctrl.base
    tmt.diff<-tmt.end-tmt.base
    diff.diff<-tmt.diff-ctrl.diff
    
    n.clust.tot <- 12 #number of clusters
    prop.clust.arm <- 4/12 #proportion of clusters in control arm
    # n.ppl <- 100 #number of people per cluster
    # prop.0 <- 0.3 #proportion in current group
    # # diff.abs <- 0.1 #absolute detectable difference of differences
    # icc<-0.01
    
    b0 <- log(ctrl.base/(1-ctrl.base)) #coefficient for intercept of regression model. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b1 <- log(tmt.base/(1-tmt.base))-b0 #coefficient for the exposure variable (e.g., intervention) of regression. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b2 <- log(ctrl.end/(1-ctrl.end))-b0 #coefficient for covariate variable (e.g., time) of regression. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b3 <- log(tmt.end/(1-tmt.end))-b0-b1-b2 #coefficient for the exposure-continuous interaction variable. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    sigma2<-pi^2/3 #ctrl.base*(1-ctrl.base)/n.ppl
    
    tau2<-icc/(1-icc)*sigma2
    
    ntim<-2 #number of time points (pre, post)
    
    # generate sample trial dataset
    time<-rep(seq(1,ntim)-1,n.clust.tot)
    cluster<-rep(seq(1,n.clust.tot),each=ntim)
    trt<-c(rep(0,n.clust.tot*prop.clust.arm*ntim),rep(1,n.clust.tot*(1-prop.clust.arm)*ntim))
    ctt1<-cbind(cluster,time,trt)
    
    # include random cluster effect 'b'
    b <- rnorm(n.clust.tot,mean=0,sd=sqrt(tau2))
    vecb<-rep(b,each=ntim)
    ctt2<-cbind(ctt1,"b"=vecb)
    
    # create muij (b0+b1*tmt+b2*time+b3*tmt*time+b)
    muij<-b0+b1*ctt2[,"trt"]+b2*ctt2[,"time"]+b3*ctt2[,"trt"]*ctt2[,"time"]+ctt2[,"b"]
    ctt3<-cbind(ctt2,muij)
    
    # expand table by sample size per cluster to get one row per participant
    ctt4<-ctt3[rep(1:nrow(ctt3), times = n.ppl), ]
    N<-n.ppl*n.clust.tot*ntim
    
    # create yijk - individual level responses
    yij<-ctt4[,"muij"]
    probij<-exp(yij)/(1+exp(yij))
    # runis <- runif(N,0,1)
    # resp <- ifelse(runis<probij,1,0)
    resp <- rbinom(N,1,probij)
    ctt5<-data.frame(cbind(ctt4,yij,probij,resp))
    
    # calculate by group means
    grpmeans<-aggregate(ctt5[, "resp"], list("trt"=ctt5$trt,"time"=ctt5$time), mean)
    # Check that expits are equivalent to the inputted rates
    # trt time          x
    # 1   0    0 0.10706019
    # 2   1    0 0.09797454
    # 3   0    1 0.10694444
    # 4   1    1 0.03032407
    
    # linear mixed model - treatment, time fixed effects, interaction tmt*time, cluster random effects
    savelme<-glmer(resp~trt+factor(time)+trt*factor(time)+(1|cluster),data=ctt5,family=binomial(link="logit"))
    vartrtbeta<-vcov(savelme)["trt:factor(time)1","trt:factor(time)1"]
    coefs<-fixef(savelme)
    obsinteff<-coefs["trt:factor(time)1"]
    
    varests<-unlist(VarCorr(savelme)) #use this to get estimated ICC
    esticc<-varests/(varests+pi^2/3)
    icc_vec<-c(icc_vec,esticc)
    # print(esticc)
    # # calculate (theoretical) power given estimated treatement effect variance
    # power <- pnorm((abs(b3)/sqrt(vartrtbeta))-qnorm(1-(alpha/2)))
    
    #Assess non-inferiority
    # lcl<-confint(savelme,parm="beta_",method="Wald",level=0.95)["trt:factor(time)1",][1]
    # ucl<-confint(savelme,parm="beta_",method="Wald",level=0.95)["trt:factor(time)1",][2]
    # if(lcl>0 | ucl<0) {ret<-1} else {ret<-0}
    pval<-summary(savelme)$coef["trt:factor(time)1","Pr(>|z|)"]
    if(pval<=0.05) {ret<-1} else {ret<-0}
    ret_vec<-c(ret_vec,ret)
    print(i)
  }
  
  power_dat<-rbind(power_dat,c("ctrl.base"=ctrl.base,"ctrl.diff"=ctrl.diff,"tmt.diff"=tmt.diff,"n.ppl"=n.ppl,"icc"=icc,"icc_est"=mean(icc_vec),"power"=sum(ret_vec)/length(ret_vec)*100))
  print(c("ctrl.base"=ctrl.base,"ctrl.diff"=ctrl.diff,"tmt.diff"=tmt.diff,"n.ppl"=n.ppl,"icc"=icc,"icc_est"=mean(icc_vec),"power"=sum(ret_vec)/length(ret_vec)*100))
}

#       ctrl.base ctrl.diff tmt.diff n.ppl  icc     icc_est power
# [1,]       0.5         0     0.10   100 0.01 0.007038996  60.5
# [2,]       0.5         0     0.30   100 0.01 0.006789666 100.0
# [3,]       0.5         0     0.20   100 0.01 0.006961621  99.5
# [4,]       0.5         0     0.15   100 0.01 0.006884889  94.0
# [5,]       0.5         0     0.13   100 0.01 0.006955886  82.5
# [6,]       0.5         0     0.14   100 0.01 0.006917825  88.5


# Looking at 12 clusters (2:1)  -------------------------------------------
# Smaller ICC leads to increased power!
# Reported in grant Aim 2: >90% power to detect a difference-in-differences of 8%
# Second stage power calculation A vs. B (A=7 sites, B=5 sites)
power_dat <- NULL
# for(icc in c(0.02,0.03,0.05))
# for(n.ppl in c(100, 200, 500))
for(inc in c(0.06))
{
  ctrl.base <- 0.5
  ctrl.end <- ctrl.base + 0.08
  tmt.base <- ctrl.base
  tmt.end <- tmt.base + (ctrl.end - ctrl.base) + inc #tmt.base + inc
  n.ppl <- 500
  icc <- 0.01
  alpha <- 0.05
  
  ret_vec<-NULL
  icc_vec<-NULL
  for(i in 1:100) {
    set.seed(i)
    ctrl.diff<-ctrl.end-ctrl.base
    tmt.diff<-tmt.end-tmt.base
    diff.diff<-tmt.diff-ctrl.diff
    
    n.clust.tot <- 12 #number of clusters
    prop.clust.arm <- 7/12 #proportion of clusters in control arm
    # n.ppl <- 100 #number of people per cluster
    # prop.0 <- 0.3 #proportion in current group
    # # diff.abs <- 0.1 #absolute detectable difference of differences
    # icc<-0.01
    
    b0 <- log(ctrl.base/(1-ctrl.base)) #coefficient for intercept of regression model. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b1 <- log(tmt.base/(1-tmt.base))-b0 #coefficient for the exposure variable (e.g., intervention) of regression. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b2 <- log(ctrl.end/(1-ctrl.end))-b0 #coefficient for covariate variable (e.g., time) of regression. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b3 <- log(tmt.end/(1-tmt.end))-b0-b1-b2 #coefficient for the exposure-continuous interaction variable. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    sigma2<-pi^2/3 #ctrl.base*(1-ctrl.base)/n.ppl
    
    tau2<-icc/(1-icc)*sigma2
    
    ntim<-2 #number of time points (pre, post)
    
    # generate sample trial dataset
    time<-rep(seq(1,ntim)-1,n.clust.tot)
    cluster<-rep(seq(1,n.clust.tot),each=ntim)
    trt<-c(rep(0,n.clust.tot*prop.clust.arm*ntim),rep(1,n.clust.tot*(1-prop.clust.arm)*ntim))
    ctt1<-cbind(cluster,time,trt)
    
    # include random cluster effect 'b'
    b <- rnorm(n.clust.tot,mean=0,sd=sqrt(tau2))
    vecb<-rep(b,each=ntim)
    ctt2<-cbind(ctt1,"b"=vecb)
    
    # create muij (b0+b1*tmt+b2*time+b3*tmt*time+b)
    muij<-b0+b1*ctt2[,"trt"]+b2*ctt2[,"time"]+b3*ctt2[,"trt"]*ctt2[,"time"]+ctt2[,"b"]
    ctt3<-cbind(ctt2,muij)
    
    # expand table by sample size per cluster to get one row per participant
    ctt4<-ctt3[rep(1:nrow(ctt3), times = n.ppl), ]
    N<-n.ppl*n.clust.tot*ntim
    
    # create yijk - individual level responses
    yij<-ctt4[,"muij"]
    probij<-exp(yij)/(1+exp(yij))
    # runis <- runif(N,0,1)
    # resp <- ifelse(runis<probij,1,0)
    resp <- rbinom(N,1,probij)
    ctt5<-data.frame(cbind(ctt4,yij,probij,resp))
    
    # calculate by group means
    grpmeans<-aggregate(ctt5[, "resp"], list("trt"=ctt5$trt,"time"=ctt5$time), mean)
    # Check that expits are equivalent to the inputted rates
    # trt time          x
    # 1   0    0 0.10706019
    # 2   1    0 0.09797454
    # 3   0    1 0.10694444
    # 4   1    1 0.03032407
    
    # linear mixed model - treatment, time fixed effects, interaction tmt*time, cluster random effects
    savelme<-glmer(resp~trt+factor(time)+trt*factor(time)+(1|cluster),data=ctt5,family=binomial(link="logit"))
    vartrtbeta<-vcov(savelme)["trt:factor(time)1","trt:factor(time)1"]
    coefs<-fixef(savelme)
    obsinteff<-coefs["trt:factor(time)1"]
    
    varests<-unlist(VarCorr(savelme)) #use this to get estimated ICC
    esticc<-varests/(varests+pi^2/3)
    icc_vec<-c(icc_vec,esticc)
    # print(esticc)
    # # calculate (theoretical) power given estimated treatement effect variance
    # power <- pnorm((abs(b3)/sqrt(vartrtbeta))-qnorm(1-(alpha/2)))
    
    #Assess non-inferiority
    # lcl<-confint(savelme,parm="beta_",method="Wald",level=0.95)["trt:factor(time)1",][1]
    # ucl<-confint(savelme,parm="beta_",method="Wald",level=0.95)["trt:factor(time)1",][2]
    # if(lcl>0 | ucl<0) {ret<-1} else {ret<-0}
    pval<-summary(savelme)$coef["trt:factor(time)1","Pr(>|z|)"]
    if(pval<=0.05) {ret<-1} else {ret<-0}
    ret_vec<-c(ret_vec,ret)
    print(i)
  }
  
  power_dat<-rbind(power_dat,c("ctrl.base"=ctrl.base,"ctrl.diff"=ctrl.diff,"tmt.diff"=tmt.diff,"n.ppl"=n.ppl,"icc"=icc,"icc_est"=mean(icc_vec),"power"=sum(ret_vec)/length(ret_vec)*100))
  print(c("ctrl.base"=ctrl.base,"ctrl.diff"=ctrl.diff,"tmt.diff"=tmt.diff,"n.ppl"=n.ppl,"icc"=icc,"icc_est"=mean(icc_vec),"power"=sum(ret_vec)/length(ret_vec)*100))
}

#       ctrl.base ctrl.diff tmt.diff n.ppl  icc     icc_est power
# [1,]       0.5      0.08     0.11   500 0.01 0.007972678    38
# ctrl.base ctrl.diff tmt.diff n.ppl  icc     icc_est power
# [1,]       0.5      0.08     0.14   500 0.01 0.007986041    91


# Second stage randomization: 10 sites ------------------------------------
#6 control vs. 4 intervention
power_dat <- NULL
# for(icc in c(0.02,0.03,0.05))
# for(n.ppl in c(100, 200, 500))
for(inc in c(0.06))
{
  ctrl.base <- 0.5
  ctrl.end <- ctrl.base + 0.08
  tmt.base <- ctrl.base
  tmt.end <- tmt.base + (ctrl.end - ctrl.base) + inc #tmt.base + inc
  n.ppl <- 500
  icc <- 0.01
  alpha <- 0.05
  
  ret_vec<-NULL
  icc_vec<-NULL
  for(i in 1:500) {
    set.seed(i)
    ctrl.diff<-ctrl.end-ctrl.base
    tmt.diff<-tmt.end-tmt.base
    diff.diff<-tmt.diff-ctrl.diff
    
    n.clust.tot <- 10 #number of clusters
    prop.clust.arm <- 6/10 #proportion of clusters in control arm
    # n.ppl <- 100 #number of people per cluster
    # prop.0 <- 0.3 #proportion in current group
    # # diff.abs <- 0.1 #absolute detectable difference of differences
    # icc<-0.01
    
    b0 <- log(ctrl.base/(1-ctrl.base)) #coefficient for intercept of regression model. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b1 <- log(tmt.base/(1-tmt.base))-b0 #coefficient for the exposure variable (e.g., intervention) of regression. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b2 <- log(ctrl.end/(1-ctrl.end))-b0 #coefficient for covariate variable (e.g., time) of regression. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b3 <- log(tmt.end/(1-tmt.end))-b0-b1-b2 #coefficient for the exposure-continuous interaction variable. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    sigma2<-pi^2/3 #ctrl.base*(1-ctrl.base)/n.ppl
    
    tau2<-icc/(1-icc)*sigma2
    
    ntim<-2 #number of time points (pre, post)
    
    # generate sample trial dataset
    time<-rep(seq(1,ntim)-1,n.clust.tot)
    cluster<-rep(seq(1,n.clust.tot),each=ntim)
    trt<-c(rep(0,n.clust.tot*prop.clust.arm*ntim),rep(1,n.clust.tot*(1-prop.clust.arm)*ntim))
    ctt1<-cbind(cluster,time,trt)
    
    # include random cluster effect 'b'
    b <- rnorm(n.clust.tot,mean=0,sd=sqrt(tau2))
    vecb<-rep(b,each=ntim)
    ctt2<-cbind(ctt1,"b"=vecb)
    
    # create muij (b0+b1*tmt+b2*time+b3*tmt*time+b)
    muij<-b0+b1*ctt2[,"trt"]+b2*ctt2[,"time"]+b3*ctt2[,"trt"]*ctt2[,"time"]+ctt2[,"b"]
    ctt3<-cbind(ctt2,muij)
    
    # expand table by sample size per cluster to get one row per participant
    ctt4<-ctt3[rep(1:nrow(ctt3), times = n.ppl), ]
    N<-n.ppl*n.clust.tot*ntim
    
    # create yijk - individual level responses
    yij<-ctt4[,"muij"]
    probij<-exp(yij)/(1+exp(yij))
    # runis <- runif(N,0,1)
    # resp <- ifelse(runis<probij,1,0)
    resp <- rbinom(N,1,probij)
    ctt5<-data.frame(cbind(ctt4,yij,probij,resp))
    
    # calculate by group means
    grpmeans<-aggregate(ctt5[, "resp"], list("trt"=ctt5$trt,"time"=ctt5$time), mean)
    # Check that expits are equivalent to the inputted rates
    # trt time          x
    # 1   0    0 0.10706019
    # 2   1    0 0.09797454
    # 3   0    1 0.10694444
    # 4   1    1 0.03032407
    
    # linear mixed model - treatment, time fixed effects, interaction tmt*time, cluster random effects
    savelme<-glmer(resp~trt+factor(time)+trt*factor(time)+(1|cluster),data=ctt5,family=binomial(link="logit"))
    vartrtbeta<-vcov(savelme)["trt:factor(time)1","trt:factor(time)1"]
    coefs<-fixef(savelme)
    obsinteff<-coefs["trt:factor(time)1"]
    
    varests<-unlist(VarCorr(savelme)) #use this to get estimated ICC
    esticc<-varests/(varests+pi^2/3)
    icc_vec<-c(icc_vec,esticc)
    # print(esticc)
    # # calculate (theoretical) power given estimated treatement effect variance
    # power <- pnorm((abs(b3)/sqrt(vartrtbeta))-qnorm(1-(alpha/2)))
    
    #Assess non-inferiority
    # lcl<-confint(savelme,parm="beta_",method="Wald",level=0.95)["trt:factor(time)1",][1]
    # ucl<-confint(savelme,parm="beta_",method="Wald",level=0.95)["trt:factor(time)1",][2]
    # if(lcl>0 | ucl<0) {ret<-1} else {ret<-0}
    pval<-summary(savelme)$coef["trt:factor(time)1","Pr(>|z|)"]
    if(pval<=0.05) {ret<-1} else {ret<-0}
    ret_vec<-c(ret_vec,ret)
    print(i)
  }
  
  power_dat<-rbind(power_dat,c("ctrl.base"=ctrl.base,"ctrl.diff"=ctrl.diff,"tmt.diff"=tmt.diff,"n.ppl"=n.ppl,"icc"=icc,"icc_est"=mean(icc_vec),"power"=sum(ret_vec)/length(ret_vec)*100))
  print(c("ctrl.base"=ctrl.base,"ctrl.diff"=ctrl.diff,"tmt.diff"=tmt.diff,"n.ppl"=n.ppl,"icc"=icc,"icc_est"=mean(icc_vec),"power"=sum(ret_vec)/length(ret_vec)*100))
}
# ctrl.base ctrl.diff tmt.diff n.ppl  icc     icc_est power
# [1,]       0.5      0.08     0.14   500 0.01 0.007472417    87


# Low performing sites  ---------------------------------------------------
# 3 vs. 3 clusters 
# Second stage power calculation in low-performing sites A vs. B (A=3 sites, B=3 sites)

power_dat <- NULL
# for(icc in c(0.02,0.03,0.05))
# for(n.ppl in c(100, 200, 500))
for(inc in c(0.08))
{
  ctrl.base <- 0.5
  ctrl.end <- ctrl.base + 0.08
  tmt.base <- ctrl.base
  tmt.end <- tmt.base + (ctrl.end - ctrl.base) + inc #tmt.base + inc
  n.ppl <- 500
  icc <- 0.01
  alpha <- 0.05
  
  ret_vec<-NULL
  icc_vec<-NULL
  for(i in 1:500) {
    set.seed(i)
    ctrl.diff<-ctrl.end-ctrl.base
    tmt.diff<-tmt.end-tmt.base
    diff.diff<-tmt.diff-ctrl.diff
    
    n.clust.tot <- 6 #number of clusters
    prop.clust.arm <- 3/6 #proportion of clusters in control arm
    # n.ppl <- 100 #number of people per cluster
    # prop.0 <- 0.3 #proportion in current group
    # # diff.abs <- 0.1 #absolute detectable difference of differences
    # icc<-0.01
    
    b0 <- log(ctrl.base/(1-ctrl.base)) #coefficient for intercept of regression model. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b1 <- log(tmt.base/(1-tmt.base))-b0 #coefficient for the exposure variable (e.g., intervention) of regression. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b2 <- log(ctrl.end/(1-ctrl.end))-b0 #coefficient for covariate variable (e.g., time) of regression. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    b3 <- log(tmt.end/(1-tmt.end))-b0-b1-b2 #coefficient for the exposure-continuous interaction variable. For logistic and Poisson log odds ratios and log incidence rate ratios. Can be 0
    
    sigma2<-pi^2/3 #ctrl.base*(1-ctrl.base)/n.ppl
    
    tau2<-icc/(1-icc)*sigma2
    
    ntim<-2 #number of time points (pre, post)
    
    # generate sample trial dataset
    time<-rep(seq(1,ntim)-1,n.clust.tot)
    cluster<-rep(seq(1,n.clust.tot),each=ntim)
    trt<-c(rep(0,n.clust.tot*prop.clust.arm*ntim),rep(1,n.clust.tot*(1-prop.clust.arm)*ntim))
    ctt1<-cbind(cluster,time,trt)
    
    # include random cluster effect 'b'
    b <- rnorm(n.clust.tot,mean=0,sd=sqrt(tau2))
    vecb<-rep(b,each=ntim)
    ctt2<-cbind(ctt1,"b"=vecb)
    
    # create muij (b0+b1*tmt+b2*time+b3*tmt*time+b)
    muij<-b0+b1*ctt2[,"trt"]+b2*ctt2[,"time"]+b3*ctt2[,"trt"]*ctt2[,"time"]+ctt2[,"b"]
    ctt3<-cbind(ctt2,muij)
    
    # expand table by sample size per cluster to get one row per participant
    ctt4<-ctt3[rep(1:nrow(ctt3), times = n.ppl), ]
    N<-n.ppl*n.clust.tot*ntim
    
    # create yijk - individual level responses
    yij<-ctt4[,"muij"]
    probij<-exp(yij)/(1+exp(yij))
    # runis <- runif(N,0,1)
    # resp <- ifelse(runis<probij,1,0)
    resp <- rbinom(N,1,probij)
    ctt5<-data.frame(cbind(ctt4,yij,probij,resp))
    
    # calculate by group means
    grpmeans<-aggregate(ctt5[, "resp"], list("trt"=ctt5$trt,"time"=ctt5$time), mean)
    # Check that expits are equivalent to the inputted rates
    # trt time          x
    # 1   0    0 0.10706019
    # 2   1    0 0.09797454
    # 3   0    1 0.10694444
    # 4   1    1 0.03032407
    
    # linear mixed model - treatment, time fixed effects, interaction tmt*time, cluster random effects
    savelme<-glmer(resp~trt+factor(time)+trt*factor(time)+(1|cluster),data=ctt5,family=binomial(link="logit"))
    vartrtbeta<-vcov(savelme)["trt:factor(time)1","trt:factor(time)1"]
    coefs<-fixef(savelme)
    obsinteff<-coefs["trt:factor(time)1"]
    
    varests<-unlist(VarCorr(savelme)) #use this to get estimated ICC
    esticc<-varests/(varests+pi^2/3)
    icc_vec<-c(icc_vec,esticc)
    # print(esticc)
    # # calculate (theoretical) power given estimated treatement effect variance
    # power <- pnorm((abs(b3)/sqrt(vartrtbeta))-qnorm(1-(alpha/2)))
    
    #Assess non-inferiority
    # lcl<-confint(savelme,parm="beta_",method="Wald",level=0.95)["trt:factor(time)1",][1]
    # ucl<-confint(savelme,parm="beta_",method="Wald",level=0.95)["trt:factor(time)1",][2]
    # if(lcl>0 | ucl<0) {ret<-1} else {ret<-0}
    pval<-summary(savelme)$coef["trt:factor(time)1","Pr(>|z|)"]
    if(pval<=0.05) {ret<-1} else {ret<-0}
    ret_vec<-c(ret_vec,ret)
    print(i)
  }
  
  power_dat<-rbind(power_dat,c("ctrl.base"=ctrl.base,"ctrl.diff"=ctrl.diff,"tmt.diff"=tmt.diff,"n.ppl"=n.ppl,"icc"=icc,"icc_est"=mean(icc_vec),"power"=sum(ret_vec)/length(ret_vec)*100))
  print(c("ctrl.base"=ctrl.base,"ctrl.diff"=ctrl.diff,"tmt.diff"=tmt.diff,"n.ppl"=n.ppl,"icc"=icc,"icc_est"=mean(icc_vec),"power"=sum(ret_vec)/length(ret_vec)*100))
}

#       ctrl.base ctrl.diff tmt.diff n.ppl  icc     icc_est power
# [1,]       0.5      0.08     0.11   500 0.01 0.007972678    38
# ctrl.base ctrl.diff tmt.diff n.ppl  icc     icc_est power
# [1,]       0.5      0.08     0.14   500 0.01 0.005935093    65
# ctrl.base ctrl.diff tmt.diff n.ppl  icc     icc_est power
# [1,]       0.5      0.08     0.15   500 0.01 0.005916359    82
# ctrl.base ctrl.diff tmt.diff n.ppl  icc     icc_est power
# [1,]       0.5      0.08     0.16   500 0.01 0.006232302    90


