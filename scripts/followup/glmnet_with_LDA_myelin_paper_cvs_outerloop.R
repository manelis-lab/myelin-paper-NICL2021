# glmnet analysis with different cv folds, inner & outer loops
# david baranger, 2021

# ==================
rm(list=ls())         #Remove all variables from list
options(scipen=999) 
# ==================

library(glmnet)
library(doParallel)
library(foreach)
library(MASS)
library(e1071)
library(dplyr)
library(gtools)
library(varhandle)    #To convert factors/levels to numeric/character variables
# ==================


top_dir="/MethodX_data"
dir=paste0(top_dir, "/other_input/")
file="ElasticNet_variables.csv"

date<-Sys.Date()
# ==================
x=read.csv(paste0(dir, "/", file), header=TRUE)
x$Group=ifelse(x$UD_Group=="1", "UD", "HC")


alpha=0.5       # 0.5 is for elassic net

set.seed(3456)     #For replicability



hc_id<-subset(x, Group=="HC", select=c(ID))
ud_id<-subset(x, Group=="UD", select=c(ID))

int.cv = c(5,10,20,30,84) # number of cv folds for the internal elastic net loop 
out.cv = c(2,3,4,5,6,7,8) # outer loop

reps = 1833

cl =7 # 1 per outer loop (35 models totals)
clus <- parallel::makeCluster(cl)
doParallel::registerDoParallel(clus)



 foreach( ocv = 1:length(out.cv),.inorder = F,.packages = c('dplyr','MASS','glmnet','e1071','varhandle') ) %dopar% { 
 
  
  #for( ocv in 1:length(out.cv)){
    for( icv in 1:length(int.cv)){
    
  if(icv == length(int.cv)){
    int.cv[icv] = 86 - (2*out.cv[ocv]) # so that last is always LOOCV
  }
  
  sel_regions<-NULL
  lambdas<-NULL
  rpair<-NULL
  
for (repetition in 1:reps ){
  print(c(ocv,icv,repetition))

  
  
  hc_list = sample(x = hc_id$ID,size = out.cv[ocv],replace = F)
  ud_list = sample(x = ud_id$ID,size = out.cv[ocv],replace = F)
  
  ID_train = c(hc_list,ud_list)
  
  data = x[-match(ID_train,x$ID),-1]
  
  #data = x %>% dplyr::filter(ID != ID_train) 
  
    #data=subset(x, ID!=pairs[subset,1] & ID!=pairs[subset,2], select=-c(ID, UD_Group, Sex, IQ, Age_Visit2))
   # data=subset(x, ID!=ID_train, select=-c(ID))
   # print(paste0("=== Subset ", subset, " - removed HC: ", pairs[subset,1], " UD: ", pairs[subset,2]))

    gr=as.factor(data$Group)
    y=gr

    ff <- as.formula( ~ .)  ### Models no interaction
    xx<-model.matrix(ff, subset(data, select=-c(Group)))[, -1] #[, -1] - to remove intercept 
    xx_df<-cbind(as.data.frame(model.matrix(ff, data)[, -1]), y)
    
    fit <- cv.glmnet(xx, y, alpha = alpha,
                     nfold = int.cv[icv], parallel = F,
                     family = "binomial", 
                     type.measure = "class", grouped=FALSE)
    
    errors = fit$cvm[which(fit$lambda==fit$lambda.1se)][[1]]   #Use 1se
    best <- cbind(subset, fit$lambda.1se,errors)
    lambdas<-as.data.frame(rbind(lambdas, best))
    results<-predict(fit, s=fit$lambda.1se, type="coefficients", alpha = alpha)

    rn <- rownames(results)[which(results !=0)]
    nfeatures<-length(rn)-1

    if (length(rn)>1) {
    keep=rn[-1]   #Remove intercept
   # keep<-keep %>% dplyr::select(-c("Sex","IQ","Age_Visit2"))
   # keep<-keep[is.na(match(keep , c("Sex","IQ","Age_Visit2")))]
    
    df=as.data.frame(cbind(y, subset(xx_df, select=keep)))
    names(df)[1]="Group"
    fit.lda <-lda(Group~., data=df, CV=TRUE)
    ct <- table(df$Group, fit.lda$class)
    #diag(prop.table(ct, 1))
    lda_cv_accuracy=sum(diag(prop.table(ct)))
    #df.test=subset(x, ID==pairs[subset,1] | ID==pairs[subset,2], select=c("Group", keep))
   
     df.test = x[match(ID_train,x$ID),-1] %>% dplyr::select(c("Group"),all_of(keep) ) 
    
    
    lda <- lda(Group~., data=df)
    pred.train <- predict(lda,df)$class
    pred.test <- predict(lda,df.test)$class
#accuracy on training/testingdata
    lda_train_acc=mean(pred.train == df$Group)
    lda_test_accuracy=mean(pred.test == df.test$Group)
    df.test$predicted<-unfactor(pred.test)
    predictedHC<-subset(df.test, Group=="HC", select=c(predicted))
    predictedUD<-subset(df.test, Group=="UD", select=c(predicted))
    coefficient <- round((results)[which(results !=0)], 3)
    
    #paste("removed_HC",c(1:3),sep="_")
    
    pred_results=as.data.frame(t(c(hc_list,ud_list,predictedHC$predicted,predictedUD$predicted)))
    colnames(pred_results) = c(  paste("removed_HC",c(1:out.cv[ocv]),sep="_"),
                                 paste("removed_UD",c(1:out.cv[ocv]),sep="_"),
                                 paste("predicted_HC",c(1:out.cv[ocv]),sep="_"),
                                 paste("predicted_UD",c(1:out.cv[ocv]),sep="_")
    )
    
    
    all=data.frame(cbind(repetition, 
                            rn, 
                            coefficient,
                            fit$lambda.1se, 
                            errors, 
                            lda_cv_accuracy, 
                            lda_train_acc, 
                            lda_test_accuracy,
                            nfeatures))
    names(all)[2] <- "Region"
    names(all)[4] <- "fit.lambda.1se"
    names(all)[6] <- "fit.cvm.1SE"
    all = cbind(all,pred_results)
    
    sel_regions<-rbind(sel_regions,all)

    } else {
      coefficient <- round((results)[which(results !=0)], 3)
      lda_cv_accuracy<-"NA"
      lda_train_acc<-"NA"
      lda_test_accuracy<-"NA"
      predictedHC<-rep("NA",out.cv[ocv])
      predictedUD<-rep("NA",out.cv[ocv])
     
       pred_results=as.data.frame(t(c(hc_list,ud_list,predictedHC,predictedUD)))
     
       colnames(pred_results) = c(  paste("removed_HC",c(1:out.cv[ocv]),sep="_"),
                                   paste("removed_UD",c(1:out.cv[ocv]),sep="_"),
                                   paste("predicted_HC",c(1:out.cv[ocv]),sep="_"),
                                   paste("predicted_UD",c(1:out.cv[ocv]),sep="_")
      )
      
      
      all=data.frame(cbind(repetition, 
                           rn, 
                           coefficient,
                           fit$lambda.1se, 
                           errors, 
                           lda_cv_accuracy, 
                           lda_train_acc, 
                           lda_test_accuracy,
                           nfeatures))
      names(all)[2] <- "Region"
      names(all)[4] <- "fit.lambda.1se"
      names(all)[6] <- "fit.cvm.1SE"
      all = cbind(all,pred_results)
      
      sel_regions<-rbind(sel_regions,all)
    }
}

  res<-unfactor(sel_regions)
#res$pair<-paste0(res$removedHC, "_", res$removedUD)

# ================== Save results

#out=paste0(top_dir, "/myelinmapping/results/glmnet_lambda.min_selected_variables_HC_vs_UD_86subj_11202020_v1.txt")

  
  
out=paste0("MethodX_data/outputs/cvs/glmnet_leave-one-out_nested_1uniquepair_removed_HC_UD_",out.cv[ocv],"_outer_",int.cv[icv],"_internalfolds_", date, ".txt")


write.table(res, out, col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

}
  
}
 parallel::stopCluster(clus)
 foreach::registerDoSEQ()
 