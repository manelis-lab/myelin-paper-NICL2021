#!/usr/bin/env Rscript

#title               glmnet_nback_multinomial_with_selection.R
#author              Anna Manelis
#version date	     11/20/2020	

### For more information check
### https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html#qs

# ==================
rm(list=ls())         #Remove all variables from list
options(scipen=999) 
# ==================
library(glmnet)
pkgs <- list("glmnet", "doParallel")
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 8)
library(MASS)
library(e1071)
library(tidyverse)
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
sel_regions<-NULL

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


for (subset in 1:dim(pairs)[1] ){

    data=subset(x, ID!=pairs[subset,1] & ID!=pairs[subset,2], select=-c(ID, UD_Group))
    print(paste0("=== Subset ", subset, " - removed HC: ", pairs[subset,1], " UD: ", pairs[subset,2]))

    gr=as.factor(data$Group)
    y=gr

    ff <- as.formula( ~ .)  ### Models no interaction
    xx<-model.matrix(ff, subset(data, select=-c(Group)))[, -1] #[, -1] - to remove intercept 
    xx_df<-cbind(as.data.frame(model.matrix(ff, data)[, -1]), y)
    
    fit <- cv.glmnet(xx, y, alpha = alpha, nfold = dim(data)[1], parallel = TRUE, family = "binomial", type.measure = "class", grouped=FALSE)
    errors = fit$cvm[which(fit$lambda==fit$lambda.1se)][[1]]   #Use 1se
    best <- cbind(subset, fit$lambda.1se,errors)
    lambdas<-as.data.frame(rbind(lambdas, best))
    results<-predict(fit, s=fit$lambda.1se, type="coefficients", alpha = alpha)

    rn <- rownames(results)[which(results !=0)]
    nfeatures<-length(rn)-1

    if (length(rn)>1) {
    keep=rn[-1]   #Remove intercept
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

# === Post-processing

res<-unfactor(sel_regions)

res$pair<-paste0(res$removedHC, "_", res$removedUD)

# ================== Save results

out=paste0(top_dir, "/outputs/glmnet/glmnet_without_age_sex_iq_leave-one-out_nested_1uniquepair_removed_HC_UD_", date, ".txt")


write.table(res, out, col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

# ====== END OF GLMNET
# ========= END OF NESTED CROSS-VALIDATED FEATURE SELECTION ========= #
#================ END OF SCRIPT ================#

