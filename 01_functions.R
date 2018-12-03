
dist_check<-function(var1){
  par(mfrow=c(2,2))
  hist(var1)
  hist(log(var1))
  avg<-mean(var1,na.rm=T)
  avg_exp<-exp(mean(log(var1),na.rm=T))
  med<-median(var1,na.rm=T)
  
  return(list(avg,avg_exp,med))
}

##################################################################################################################
mean_table<-function(y,x){
  
  mean.x<-t.test(y~x)
  sd<-round(tapply(y,x,function(x) sqrt(var(x,na.rm=T))),1)
  
#table for x
  tab.x<-data.frame(Variable="",
    one=paste0(round(mean.x$estimate[2],1),"±",sd[2]),
    two=paste0(round(mean.x$estimate[1],1), "±",sd[1]),
    Pval=round(mean.x$p.value,4))
  
  tab.x$Pval[tab.x$Pval==0]<-"<0.0001"
  
  return(tab.x)
}
#########################################################################################################
prop_table<- function(var1,var2) {
  y<-table(var1, var2)
  y.prop<-prop.table(y,2)
  y_p<-fisher.test(y)

  #table
  tab.1<-data.frame(Variable=c("",levels(as.factor(var1))),
                    one=c("",paste(y[,2],paste0("(",round(y.prop[,2]*100,0),"%",")"))),
                    two=c("",paste(y[,1],paste0("(",round(y.prop[,1]*100,0),"%",")"))),
                    Pval=c(round(y_p$p.value,4),rep("",length(levels(as.factor(var1))))),row.names=NULL)
  
  tab.1$Pval<-as.character(tab.1$Pval)
  tab.1$Pval[tab.1$Pval==0]<-"<0.001"
  
  return(tab.1)
}
#############################################################################################################
#summarize logistic model in table format
mod_tablog<-function(model){
  ret<-data.frame(Independent_variable=names(coef(model))[-1],
                  OR=paste0(round(exp(coef(model)[-c(1)]),1)," (",
                            round(exp(confint(model)[,1]),1)[-c(1)],", ",
                            round(exp(confint(model)[,2]),1)[-c(1)],")"),
                  Pvalue=round(summary(model)$coef[,4],4)[-c(1)],
                  row.names=NULL)
  colnames(ret)<-c("Predictor","OR (95% CI)","PValue")
  return(ret)}
###########################################################################################################
#summarize linear model in table format
mod_tab<-function(model){
  ret<-data.frame(Independent_variable=names(coef(model))[-1],
                  Estimate=paste0(round(coef(model)[-c(1)],3)," (",
                                  round(confint(model)[,1],3)[-c(1)],", ",
                                  round(confint(model)[,2],3)[-c(1)],")"),
                  Pvalue=round(summary(model)$coef[,4],4)[-c(1)],
                  row.names=NULL)
  colnames(ret)<-c("Predictor","Estimate (95% CI)","PValue")
  return(ret)}
