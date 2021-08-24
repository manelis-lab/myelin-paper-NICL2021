library(dplyr)
library(readxl)
library(tidyr)
library(stringr)

study_data<-read.csv(file = "MethodX_data/data/other_input/data_clinical_and_parcels_all.csv",stringsAsFactors = F)
frequency_compare<-read.csv(file = "MethodX_data/outputs/glmnet/glmnet_variable_selection.csv",stringsAsFactors = F)


brain_select<- frequency_compare %>% 
  filter(`Variable Retained?` == TRUE) %>% 
  filter(Variable != "IQ")

#Brain<-study_data[,match(c("ID",brain_select$Variable),colnames(study_data) )]
study_data$UDGroup<-(study_data$Group == "UD")*1

for(i in 1:(dim(brain_select)[1])){
  

  
  analysis_data<-study_data %>% 
                dplyr::select(c(brain_select$Variable[i],"UDGroup","Age_Visit2","Sex","IQ"))
  colnames(analysis_data)[1]<-"Region"
  
  analysis_data<-analysis_data %>% 
    mutate_all( scale,center=T,scale=T)
  

  fit1<-lm(Region ~   UDGroup +   Age_Visit2+Sex + IQ,data = analysis_data )
  
  
  J<-summary(fit1)$coefficients[-1,] 
  colnames(J)<-c("Est","SE","t","p")
  out<-t(as.data.frame(as.vector(t(J))))
  
  for(k in 1: length(row.names(J))){
    
    name_temp<-paste(row.names(J)[k],colnames(J),sep="."  )
    if(k==1){out2<-name_temp}
    if(k>1){out2<-c(out2,name_temp)}
    
  }
  colnames(out)<-out2
  row.names(out)<-colnames(Brain)[i]
  out<-as.data.frame(out)
  
  #out$AnovaCompare.p<-model_compare$`Pr(>F)`[2]
  
  if(i==1){out_final<-out}
  if(i>1){out_final<-rbind(out_final,out)}
  
}
out_final<-as.data.frame(out_final)

out_final<-out_final[order(out_final$UDGroup.p),]
out_final$p_fdr<-p.adjust(out_final$UDGroup.p,method = "fdr")



write.csv(x = out_final,file = "MethodX_data/outputs/regressions/regression_dd_control_myelin.csv",row.names = F,quote = F)
