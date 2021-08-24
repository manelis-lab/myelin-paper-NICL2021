library(dplyr)
library(EnvStats)




Brain<-read.csv("/MethodX_data/data/other_input/data_360parcels_Glasser32K.csv")
Brain<-Brain[,-1]



Brain3<-as.data.frame(apply(X = Brain,MARGIN = 2,FUN = function(x){
  cv = sd(x)/abs(mean(x))
  #cv<-EnvStats::cv(x=J,method = "l.moments")
  return(cv)
  
}))
colnames(Brain3)<-c("CV")

outliers<-rosnerTest(Brain3$CV,k = 5)
outlier.col1<-outliers$all.stats$Obs.Num[outliers$all.stats$Outlier]

Brain3<-Brain3[-outlier.col1,]

outliers<-rosnerTest(Brain3,k = 5)
outlier.col2<-outliers$all.stats$Obs.Num[outliers$all.stats$Outlier]


Brain3<-Brain3[-outlier.col2]

outliers<-rosnerTest(Brain3,k = 5)
outlier.col3<-outliers$all.stats$Obs.Num[outliers$all.stats$Outlier]

length(c(outlier.col1,outlier.col2,outlier.col3))
#11 outliers
#


Brain3<-as.data.frame(apply(X = Brain,MARGIN = 2,FUN = function(x){
  cv = sd(x)/abs(mean(x))
  #cv<-EnvStats::cv(x=J,method = "l.moments")
  return(cv)
  
}))
colnames(Brain3)<-c("CV")

outliers<-rosnerTest(Brain3$CV,k = 20)
#outlier.col1<-outliers$all.stats$Obs.Num[outliers$all.stats$Outlier]

outliers<-dplyr::filter(outliers$all.stats, Outlier == TRUE)



#Brain[,outlier.col]
write.csv(x = outliers,file = "MethodX_data/outputs/preprocessing/outlier.results.csv")