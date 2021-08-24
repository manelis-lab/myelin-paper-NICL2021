# predict case/control status for 719313
# # David Baranger - 1/7/2021
# 
# 


library(dplyr)

library(readxl)
library(stringr)
library(tidyr)

library(EnvStats)

library(doParallel)
library(tictoc)
library(MASS)


#converted subject
data_719313<-read_xlsx("MethodX_data/data/other_input/converted_participant_parcels_bothsessions.xlsx")
# location of Supplemental Table 2
plot_data<-read.csv("MethodX_data/outputs/glmnet/glmnet_variable_selection.csv",header = T)

Variables<-read.csv("MethodX_data/data/other_input/data_clinical_and_parcels_all.csv",header = T)



Variables$UD_Group<-(Variables$Group == "UD")*1


brain_select<- plot_data %>% 
  filter(`Variable Retained?` == TRUE) %>% 
  filter(Variable != "IQ")

var_keep<-c("UD_Group",brain_select$Variable)

All<-Variables[,match(var_keep,colnames(Variables) )]

fit.lda <-MASS::lda(UD_Group ~., data=All, CV=F)

# Predicted control/case in converted subject
predict(object = fit.lda,newdata =data_719313)$class

# [1] 1 1
# Levels: 0 1

sink(file = "MethodX_data/outputs/glmnet/predict.followup.txt")
print("Predict class (control/dd) of participant who converted from control to dd")
print("0 = control; 1 = depressive disorder")
print(predict(object = fit.lda,newdata =data_719313)$class)
sink()
