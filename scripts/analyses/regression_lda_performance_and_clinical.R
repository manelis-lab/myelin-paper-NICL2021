library(dplyr)
library(readxl)
library(tidyr)
library(stringr)



# location of Study Data
study_data<-read.csv(file = "MethodX_data/data/other_input/data_clinical_and_parcels_all.csv",stringsAsFactors = F)
performance_data = read.csv(file = "MethodX_data/data/other_input/glmnet_performance.csv",stringsAsFactors = F)

study_data = merge(study_data,performance_data,by="ID")

all_subj_vars<-c("IQ","Age_Visit2",	"Sex",	"HRSD25_baseline"	,"MOODStotal_baseline_lifetime")
UD_only_vars<-c("AD_1","BASELINE_Mood_Dx","Depression_age_onset","Depression_years","BASELINE_number_MDE_lifetime","NComorbid")

study_data$UDGroup<-(study_data$Group == "UD")*1

for(i in 1:length(all_subj_vars)){
  
  
  
  analysis_data<-study_data %>% 
    dplyr::select(c(all_subj_vars[i],"UDGroup","perc_cor"))
  colnames(analysis_data)[1]<-"variable"
  
  fit_base<-lm(perc_cor ~  1,data = analysis_data )
  fit1<-lm(perc_cor ~   UDGroup +  variable + UDGroup:variable ,data = analysis_data )
  
  compare<-anova(fit_base,fit1)
  
  out<-c("All",all_subj_vars[i],compare$F[2],compare$`Pr(>F)`[2])
  
  if(i ==1){out_final<-out}
  if(i>1)  {out_final <- rbind(out_final,out)}
  
}
colnames(out_final)<-c("Sample","Variable","F","P")
all_final<-out_final


####
#UD
study_data<-dplyr::filter(study_data,Group == "UD")
for(i in 1:length(UD_only_vars)){
  
  
  
  analysis_data<-study_data %>% 
    dplyr::select(c(UD_only_vars[i],"perc_cor")) %>% na.omit()
  colnames(analysis_data)[1]<-"variable"
  
  fit_base<-lm(perc_cor ~  1,data = analysis_data )
  fit1<-lm(perc_cor ~  variable  ,data = analysis_data )
  
  compare<-anova(fit_base,fit1)
  
  out<-c("UD only",UD_only_vars[i],compare$F[2],compare$`Pr(>F)`[2])
  
  if(i ==1){out_final<-out}
  if(i>1)  {out_final <- rbind(out_final,out)}
  
}
colnames(out_final)<-c("Sample","Variable","F","P")
UD_final<-out_final

final<-rbind(all_final,UD_final)

write.csv(x = final,file = "MethodX_data/outputs/regressions/regression_dd_control_myelin.csv",row.names = F,quote = F)
