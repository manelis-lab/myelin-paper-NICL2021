#!/usr/bin/env Rscript

#title               myelin_paper_permuted_labels_with_selection.R
#author              Anna Manelis
#version date	     02/22/2021	

# ==================
rm(list=ls())         #Remove all variables from list
options(scipen=999) 
# ==================
library(glmnet)
pkgs <- list("glmnet", "doParallel")
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 8)

library(MASS)
library(lattice)
library(e1071)
library(tidyverse)
library(gtools)
library(varhandle)    #To convert factors/levels to numeric/character variables

# ================== FIXME: The paths should be changed according to the git folder structure

top_dir="/MethodX_data"
dir=paste0(top_dir, "/other_input/")
file="ElasticNet_variables.csv"

date<-Sys.Date()

# ================== Read input data

x=read.csv(paste0(dir, "/", file), header=TRUE)
x$Group=ifelse(x$UD_Group=="1", "UD", "HC")

alpha=0.5       # 0.5 is for elassic net

set.seed(3456)     #For replicability

lambdas<-NULL
rpair<-NULL
sel_regions<-NULL
sets<-NULL

# === Creat exhaustive list of HC and UD pairs

hc_id<-subset(x, Group=="HC", select=c(ID))
ud_id<-subset(x, Group=="UD", select=c(ID))

pairs=NULL
for (hc in 1:dim(hc_id)[1]) {
   for (ud in 1:dim(ud_id)[1]) {
       b=cbind(hc_id[hc,], ud_id[ud,])
       pairs=rbind(pairs, b)
    }
}


# === 'rands' variable is the random seed to start the loop

rands<-c(1, 20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000, 180000)

for (i in 1:length(rands)) { 
seeds=c(rands[i]:400000)

    for (subset in 1:dim(pairs)[1] ){
    
        data=subset(x, ID!=pairs[subset,1] & ID!=pairs[subset,2], select=-c(ID, UD_Group))
        data_id=subset(x, ID!=pairs[subset,1] & ID!=pairs[subset,2], select=-c(UD_Group))
   
        gr<-as.factor(data$Group)
        id<-data_id[, c("Group","ID", "Sex", "IQ", "Age_Visit2")]
        for (permutation in 1:10) {
    
            print(paste0("=== Subset ", subset, " - removed HC: ", pairs[subset,1], " UD: ", pairs[subset,2], " permutation ", seeds[permutation]))
            set.seed(seeds[permutation])
            y_perm<-sample(gr)
            y=y_perm
            id_rand<-cbind(id,y)
            id_rand$overlap<-ifelse(id_rand$Group==id_rand$y, 1, 0)
            HC_overlap<- table(id_rand$Group, id_rand$overlap)["HC", "1"]/sum(table(id_rand$Group, id_rand$overlap)["HC",])*100
            UD_overlap<- table(id_rand$Group, id_rand$overlap)["UD", "1"]/sum(table(id_rand$Group, id_rand$overlap)["UD",])*100
            total_overlap<-(HC_overlap+UD_overlap)/2   # Computed as the mean overlap for UD and HC

            pvals<-as.data.frame(t(c(
               t.test(id_rand$Age_Visit2[y=="UD"],id_rand$Age_Visit2[gr=="UD"])$p.value,
               t.test(id_rand$Age_Visit2[y=="HC"],id_rand$Age_Visit2[gr=="HC"])$p.value,
               t.test(id_rand$IQ[y=="UD"],id_rand$IQ[gr=="UD"])$p.value,
               t.test(id_rand$IQ[y=="HC"],id_rand$IQ[gr=="HC"])$p.value,
               chisq.test(id_rand$Sex[y=="UD"],id_rand$Sex[gr=="UD"])$p.value,
               chisq.test(id_rand$Sex[y=="HC"],id_rand$Sex[gr=="HC"])$p.value
            )))
  
            colnames(pvals)<- apply(expand.grid(c("UD","HC"),c("Age","IQ","Sex"),"pval"), 1, paste, collapse=".")

            set<-as.data.frame(cbind(unfactor(id), unfactor(y), 
              HC_overlap, UD_overlap, total_overlap, pvals, subset, seeds[permutation]))
            names(set)[1]<-"Group" 
            names(set)[6]<-"permutedGroup" 
            names(set)[17]<-"permutation_seed"
            sets<-rbind(sets, set)
        
            ff <- as.formula( ~ .)  ### Model without interactions

            xx<-model.matrix(ff, subset(data, select=-c(Group)))[, -1] ###[, -1] - to remove intercept 
            xx_df<-cbind(as.data.frame(xx), y)

            fit <- cv.glmnet(xx, y, alpha = alpha, nfold = dim(data)[1], parallel = TRUE, family = "binomial", type.measure = "class", grouped=FALSE)
            errors = fit$cvm[which(fit$lambda==fit$lambda.1se)][[1]]   ###Use 1se
            best <- cbind(subset, fit$lambda.1se,errors)
            lambdas<-as.data.frame(rbind(lambdas, best))
            results<-predict(fit, s=fit$lambda.1se, type="coefficients", alpha = alpha)

            rn <- rownames(results)[which(results !=0)]
            print(paste0(rn))
            nfeatures<-length(rn)-1
 
            if (length(rn)>1) {
                keep=rn[-1]   #Remove intercept from the list of selected variables
                df=as.data.frame(cbind(y, subset(xx_df, select=keep)))
                names(df)[1]="Group"
	        # XXX: LDA with leave-one-out cross-validation on all data: not included into the final results and manuscripts
                fit.lda <-lda(Group~., data=df, CV=TRUE)
                ct <- table(df$Group, fit.lda$class)
                lda_cv_accuracy=sum(diag(prop.table(ct)))
         
            # === Train LDA on 84 subjects
	        lda <- lda(Group~., data=df)
                pred.train <- predict(lda,df)$class
            # === Test LDA on 2 'held-out' subjects
	        df.test=subset(x, ID==pairs[subset,1] | ID==pairs[subset,2], select=c("Group", keep))
                pred.test <- predict(lda,df.test)$class
            # === accuracy on training/testingdata
                lda_train_acc=mean(pred.train == df$Group)
                lda_test_accuracy=mean(pred.test == df.test$Group)
                df.test$predicted<-unfactor(pred.test)
                predictedHC<-subset(df.test, Group=="HC", select=c(predicted))
                predictedUD<-subset(df.test, Group=="UD", select=c(predicted))
    
                coefficient <- round((results)[which(results !=0)], 3)
	    # === Put together all calculated variables to include into the output tables
                all=as.data.frame(cbind(subset, pairs[subset,1], pairs[subset,2], rn, coefficient, fit$lambda.1se, errors, lda_cv_accuracy, lda_train_acc, lda_test_accuracy, predictedHC[,1], predictedUD[,1], seeds[permutation], nfeatures, pvals, HC_overlap, UD_overlap, total_overlap))
                names(all)[2] <- "removedHC"
                names(all)[3] <- "removedUD"
                names(all)[4] <- "Region"
                names(all)[6] <- "fit.lambda.1se"
                names(all)[7] <- "fit.cvm.1SE"
                names(all)[11] <- "predictedHC"
                names(all)[12] <- "predictedUD"
                names(all)[13] <- "permutation_seed"

                sel_regions<-rbind(sel_regions,all)
            } else {
                coefficient <- round((results)[which(results !=0)], 3)
                lda_cv_accuracy<-NA
                lda_train_acc<-NA
                lda_test_accuracy<-NA
                predictedHC<-"NA"
                predictedUD<-"NA"
                all=as.data.frame(cbind(subset, pairs[subset,1], pairs[subset,2], rn, coefficient, fit$lambda.1se, errors, lda_cv_accuracy, lda_train_acc, lda_test_accuracy, predictedHC, predictedUD, seeds[permutation], nfeatures, pvals, HC_overlap, UD_overlap, total_overlap))
                names(all)[2] <- "removedHC"
                names(all)[3] <- "removedUD"
                names(all)[4] <- "Region"
                names(all)[6] <- "fit.lambda.1se"
                names(all)[7] <- "fit.cvm.1SE"
                names(all)[11] <- "predictedHC"
                names(all)[12] <- "predictedUD"
                names(all)[13] <- "permutation_seed"
                sel_regions<-rbind(sel_regions,all)
            }
        }

        seeds=seeds[-(1:100)]
    }

# === Post-processing

res<-unfactor(sel_regions)

res$pair<-paste0(res$removedHC, "_", res$removedUD)

# ================== Save results

out=paste0(top_dir, "/outputs/permutations/split", i, "_glmnet_permuted_labels_10times_leave-one-out_nested_with_age_sex_iq_1uniquepair_removed_HC_UD_", date, ".txt")

write.table(res, out, col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

# === Save permuted sets

out2=paste0(top_dir, "/outputs/permutations/split", i, "_glmnet_permuted_sets_10times_for_leave-one-out_nested_with_age_sex_iq_1uniquepair_removed_HC_UD_", date, ".txt")

write.table(sets, out2, col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

}

# ========= END OF NESTED CROSS-VALIDATED FEATURE SELECTION


# === Merge all splits into one dataset

perm_base<-paste0("glmnet_permuted_labels_10times_leave-one-out_nested_with_age_sex_iq_1uniquepair_removed_HC_UD_", date, ".txt")

set_base<-paste0("glmnet_permuted_sets_10times_for_leave-one-out_nested_with_age_sex_iq_1uniquepair_removed_HC_UD_", date, ".txt")

# ===
perm_list <- as.list(list.files(paste0(top_dir, "/outputs/permutations"), pattern = perm_base, full.names = TRUE))

perm_bind <- do.call("rbind", lapply(perm_list, FUN = function(file) {
  read.table(file, header=TRUE, sep="\t")
}))

# ===
set_list <- as.list(list.files(paste0(top_dir, "/outputs/permutations"), pattern = set_base, full.names = TRUE))

set_bind <- do.call("rbind", lapply(set_list, FUN = function(file) {
  read.table(file, header=TRUE, sep="\t")
}))

# ===
perm_out<-paste0(top_dir, "/outputs/permutations/glmnet_permuted_labels_100times_for_leave-one-out_nested_with_age_sex_iq_1uniquepair_removed_HC_UD_", date, ".txt")

set_out<-paste0(top_dir, "/outputs/permutations/glmnet_permuted_sets_100times_for_leave-one-out_nested_with_age_sex_iq_1uniquepair_removed_HC_UD_", date, ".txt")

# ===
write.table(perm_bind, perm_out, col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
write.table(set_bind, set_out, col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

#================ END OF SCRIPT ================#

