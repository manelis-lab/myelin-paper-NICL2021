library(dplyr)
library(readxl)
library(tidyr)
library(stringr)


# location of Study Data
study_data<-read.csv(file = "MethodX_data/data/other_input/data_clinical_and_parcels_all.csv",stringsAsFactors = F)


all_subj_vars<-c("IQ","Age_Visit2",	"Sex",	"HRSD25_baseline"	,"MOODStotal_baseline_lifetime")
UD_only_vars<-c("AD_1","BASELINE_Mood_Dx","Depression_age_onset","Depression_years","BASELINE_number_MDE_lifetime","NComorbid")

study_data$UDGroup<-(study_data$Group == "UD")*1

# location of Supplemental Table 2

frequency_compare<-read.csv(file = "MethodX_data/outputs/glmnet/glmnet_variable_selection.csv",stringsAsFactors = F)


brain_select<- frequency_compare %>% 
  filter(`Variable Retained?` == TRUE) %>% 
  filter(Variable != "IQ")


for(i in 1:length(all_subj_vars)){
  
  
  for(b in 1:(dim(brain_select)[1])){
  
  col_keep<-unique(c(brain_select$Variable[b],all_subj_vars[i],"UDGroup","IQ","Sex","Age_Visit2"))
    
  analysis_data<-study_data %>% 
    dplyr::select(col_keep) %>% na.omit()
  
  
  colnames(analysis_data)[c(1,2)]<-c("Region","variable")

  fit_base<-lm(Region ~  as.matrix(analysis_data[,c(4:dim(analysis_data)[2])]),data = analysis_data )
  fit1<-lm(Region ~   UDGroup +  variable + UDGroup:variable +  as.matrix(analysis_data[,c(4:dim(analysis_data)[2])]),data = analysis_data )

 
  
  compare<-anova(fit_base,fit1)
  
  out<-c("All",all_subj_vars[i],brain_select$Variable[b],compare$F[2],compare$`Pr(>F)`[2])
  
  
  
  
  
  
  
  if(b ==1){out_brain<-out}
  if(b>1)  {out_brain <- rbind(out_brain,out)}
  
  }
  out_brain<-as.data.frame(out_brain)
  colnames(out_brain)<-c("Sample","Variable","Region","F","P")
  out_brain$P_fdr<-p.adjust(p = out_brain$P,method = "fdr")
  
  
  if(i ==1){out_final<-out_brain}
  if(i>1)  {out_final <- rbind(out_final,out_brain)}
  
  
}

all_final<-out_final


###
study_data<-filter(study_data, Group == "UD")
for(i in 1:length(UD_only_vars)){
  
  
  for(b in 1:(dim(brain_select)[1])){
    
    col_keep<-unique(c(brain_select$Variable[b],UD_only_vars[i],"IQ","Sex","Age_Visit2"))
    
    analysis_data<-study_data %>% 
      dplyr::select(col_keep) %>% na.omit()
    
    
    colnames(analysis_data)[c(1,2)]<-c("Region","variable")
    
    fit_base<-lm(Region ~  as.matrix(analysis_data[,c(4:dim(analysis_data)[2])]),data = analysis_data )
    fit1<-lm(Region ~    variable +   as.matrix(analysis_data[,c(4:dim(analysis_data)[2])]),data = analysis_data )

    
    compare<-anova(fit_base,fit1)
    
    out<-c("UD Only",UD_only_vars[i],brain_select$Variable[b],compare$F[2],compare$`Pr(>F)`[2])
    
    
    
    
    
    
    
    if(b ==1){out_brain<-out}
    if(b>1)  {out_brain <- rbind(out_brain,out)}
    
  }
  out_brain<-as.data.frame(out_brain)
  colnames(out_brain)<-c("Sample","Variable","Region","F","P")
  out_brain$P_fdr<-p.adjust(out_brain$P,method = "fdr")
  
  
  if(i ==1){out_final<-out_brain}
  if(i>1)  {out_final <- rbind(out_final,out_brain)}
  
  
}

UD_final<-out_final



final<-rbind(all_final,UD_final)

write.csv(x = final,file = "MethodX_data/outputs/regressions/regression_demo_clin_myelin.csv",row.names = F,quote = F)




