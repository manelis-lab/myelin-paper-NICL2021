# glmnet analysis with different cv folds
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

lambdas<-NULL
rpair<-NULL

hc_id<-subset(x, Group=="HC", select=c(ID))
ud_id<-subset(x, Group=="UD", select=c(ID))

# === Build an exhaustive set of UD-HC pairs to remove
pairs=NULL
for (hc in 1:dim(hc_id)[1]) {
   for (ud in 1:dim(ud_id)[1]) {
       b=cbind(hc_id[hc,], ud_id[ud,])
       pairs=rbind(pairs, b)
    }
}

cl =5
clus <- parallel::makeCluster(cl)
doParallel::registerDoParallel(clus)



int.cv = c(5,10,20,30,84) # number of cv folds for the internal elastic net loop 
sel_regions<-NULL

#for( icv in 1:length(int.cv)){
  foreach( icv = 1:length(int.cv),.inorder = F,
           .packages = c('dplyr','MASS','glmnet','e1071','varhandle') ) %dopar% { 
    
  sel_regions<-NULL
 # all = NULL
  
  
for (subset in 1:dim(pairs)[1] ){
print(subset)
    #data=subset(x, ID!=pairs[subset,1] & ID!=pairs[subset,2], select=-c(ID, UD_Group, Sex, IQ, Age_Visit2))
    data=subset(x, ID!=pairs[subset,1] & ID!=pairs[subset,2], select=-c(ID))
    print(paste0("=== Subset ", subset, " - removed HC: ", pairs[subset,1], " UD: ", pairs[subset,2]))

    gr=as.factor(data$Group)
    y=gr

    ff <- as.formula( ~ .)  ### Models no interaction
    xx<-model.matrix(ff, subset(data, select=-c(Group)))[, -1] #[, -1] - to remove intercept 
    xx_df<-cbind(as.data.frame(model.matrix(ff, data)[, -1]), y)
    
    fit <- cv.glmnet(xx, y, alpha = alpha, nfold = int.cv[icv], parallel = F, family = "binomial", type.measure = "class", grouped=FALSE)
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
    df.test=subset(x, ID==pairs[subset,1] | ID==pairs[subset,2], select=c("Group", keep))
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
    all=as.data.frame(cbind(subset, pairs[subset,1], pairs[subset,2], rn, coefficient, fit$lambda.1se, errors, lda_cv_accuracy, lda_train_acc, lda_test_accuracy, predictedHC[,1], predictedUD[,1], nfeatures))
    names(all)[2] <- "removedHC"
    names(all)[3] <- "removedUD"
    names(all)[4] <- "Region"
    names(all)[6] <- "fit.lambda.1se"
    names(all)[7] <- "fit.cvm.1SE"
    names(all)[11] <- "predictedHC"
    names(all)[12] <- "predictedUD"
    
    sel_regions<-rbind(sel_regions,all)

    } else {
      coefficient <- round((results)[which(results !=0)], 3)
      lda_cv_accuracy<-"NA"
      lda_train_acc<-"NA"
      lda_test_accuracy<-"NA"
      predictedHC<-"NA"
      predictedUD<-"NA"
      all=as.data.frame(cbind(subset, pairs[subset,1], pairs[subset,2], rn, coefficient, fit$lambda.1se, errors, lda_cv_accuracy, lda_train_acc, lda_test_accuracy, predictedHC, predictedUD, nfeatures))
      names(all)[2] <- "removedHC"
      names(all)[3] <- "removedUD"
      names(all)[4] <- "Region"
      names(all)[6] <- "fit.lambda.1se"
      names(all)[7] <- "fit.cvm.1SE"
      names(all)[11] <- "predictedHC"
      names(all)[12] <- "predictedUD"
    
    sel_regions<-rbind(sel_regions,all)
    }
}


res<-unfactor(sel_regions)

res$pair<-paste0(res$removedHC, "_", res$removedUD)

# ================== Save results

#out=paste0(top_dir, "/myelinmapping/results/glmnet_lambda.min_selected_variables_HC_vs_UD_86subj_11202020_v1.txt")
#out=paste0("C:/Users/barangerda/ACE Lab/Paper_Draft/revisions/glmnet_leave-one-out_nested_1uniquepair_removed_HC_UD_",out.cv[ocv],"_outer_",int.cv[icv],"_internalfolds_", date, ".txt")

out=paste0( "MethodX_data/outputs/cvs/loocv_inneronly/glmnet_leave-one-out_nested_1uniquepair_removed_HC_UD_",int.cv[icv],"_internalfolds_", date, ".txt")


write.table(res, out, col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

}
  parallel::stopCluster(clus)
  foreach::registerDoSEQ()
  