# Process glmnet output
# David Baranger

library(data.table)
library(dplyr)
library(ggplot2)
################
# Location of 'Code' folder


#output of true analysis
glmnet_truefile<-fread(input = "MethodX_data/outputs/glmnet/elasticnet_output/glmnet_with_age_sex_iq_leave-one-out_nested_1uniquepair_removed_HC_UD_2020-11-26.txt",header = T,stringsAsFactors = F,data.table = F,fill=T)

#pull in permutation data

for(i in 1:10){
 # print(i)
 # # Permutation analysis output files
  split<-paste("MethodX_data/outputs/permutations/split",i,"_glmnet_permuted_labels_10times_leave-one-out_nested_with_age_sex_iq_1uniquepair_removed_HC_UD_2020-12-09.txt",sep="")
  glmnet_file_temp<-fread(input = split,header = T,stringsAsFactors = F,data.table = F,fill=T)
  
  glmnet_file_temp<-glmnet_file_temp[-(dim(glmnet_file_temp)[1]-1),] # last line is "====="
  glmnet_file_temp$subset<-as.numeric(glmnet_file_temp$subset) # this is originally a character because of the "===="
  
  if (i ==1){glmnet_file<-glmnet_file_temp
  }else{glmnet_file<-rbind(glmnet_file,glmnet_file_temp)}
  
}
rm(glmnet_file_temp)

glmnet_truefile<-glmnet_truefile[-(dim(glmnet_truefile)[1]-1),] # last line is "====="
glmnet_truefile$subset<-as.numeric(glmnet_truefile$subset) # this is originally a character because of the "===="


# look only at per-permutation info, remove region information and then remove duplicate rows
 glmnet_file2<-glmnet_file[,-match( c("Region","coefficient"),colnames(glmnet_file))]
 glmnet_file2<-unique(glmnet_file2)
 glmnet_file2<-filter(glmnet_file2, !is.na(subset))
# 
glmnet_truefile2<-glmnet_truefile[,-match( c("Region","coefficient"),colnames(glmnet_truefile))]
glmnet_truefile2<-unique(glmnet_truefile2)
glmnet_truefile2<-filter(glmnet_truefile2, !is.na(subset))

# only models where variables were retained
glmnet_file3<-na.omit(glmnet_file2)
glmnet_truefile3<-na.omit(glmnet_truefile2)


print("Proportion of true models where any variables were retained")
print(dim(glmnet_truefile3)[1]/dim(glmnet_truefile2)[1]*100)

print("Proportion of permutation models where any variables were retained")
print(dim(glmnet_file3)[1]/dim(glmnet_file2)[1]*100)


#hist(glmnet_file3$nfeatures,breaks = 100)
print("Distribution of number of retained features with true data")
print(quantile(glmnet_truefile3$nfeatures))
print("Distribution of number of retained features in permutation")
print(quantile(glmnet_file3$nfeatures))

glmnet_file3$total_overlap <- glmnet_file3$total_overlap/2



# percent permuations where groups differ by covariates

sig_diff<-table(apply(glmnet_file2 %>% dplyr::select(ends_with(match = c("pval"))),MARGIN = 1,
                FUN = function(X){
                  sum(sum(p.adjust(p =X,method = "fdr")<0.05)>0)                  }
  
))
print("Proportion of models where age,sex,IQ of permuted groups signicicantly differ from true groups")
perc_diff<-(sig_diff[2]/(sig_diff[1]+sig_diff[2]))*100
print(perc_diff)
#glmnet_file[which(glmnet_file$Region==""),]


#True accuracy



#table(glmnet_truefile$removedHC)
glmnet_truefile$HC_cor<-(glmnet_truefile$predictedHC == "HC")*1
glmnet_truefile$UD_cor<-(glmnet_truefile$predictedUD == "UD")*1

HC_predicted<-glmnet_truefile %>% 
  dplyr::group_by_at("removedHC") %>% 
  dplyr::summarise(.groups = "keep",
                   perc_cor= mean(HC_cor))

UD_predicted<-glmnet_truefile %>% 
  dplyr::group_by_at("removedUD") %>% 
  dplyr::summarise(.groups = "keep",
                   perc_cor= mean(UD_cor))

#hist(HC_predicted$perc_cor)
#hist(UD_predicted$perc_cor)

print("Total Accuracy")
print(mean(c(HC_predicted$perc_cor,UD_predicted$perc_cor)))

print("UD Accuracy")
print(mean(UD_predicted$perc_cor))
print("HC accuracy")
print(mean(HC_predicted$perc_cor))
print("Accuracy Distribution")
quantile(c(UD_predicted$perc_cor,HC_predicted$perc_cor))
print("Binomial Test accuracy > 50%")
binom.test(sum(c(na.omit(UD_predicted$perc_cor) > .5, na.omit(HC_predicted$perc_cor)>.5)),86)



UD_predicted<-data.frame(ID = UD_predicted$removedUD,
                         perc_cor = UD_predicted$perc_cor,
                         Group = "UD") %>% na.omit()
HC_predicted<-data.frame(ID = HC_predicted$removedHC,
                         perc_cor = HC_predicted$perc_cor,
                         Group = "HC")%>% na.omit()
predicted<-rbind(UD_predicted,HC_predicted)
print("Prediction accuracy and subject labels saved")
write.csv(x = predicted,file = "MethodX_data/data/other_input/glmnet_performance.csv",row.names = F)



## permutation per-subject accuracy
for( s in 1: dim(table(glmnet_file2$subset)) ){
#  print(s)
  
#for(p in 1:100){
  pair_data<-glmnet_file2[glmnet_file2$subset==s,]
  
 pair_info<-data.frame(
  HC_ID = pair_data$removedHC[1],
  HC_acc = sum(na.omit(pair_data$predictedHC == "HC"))/100,
  HC_totacc = sum(na.omit(pair_data$predictedHC == "HC"))/(100 -sum(is.na(pair_data$predictedHC)) ),
  HC_NA = sum(is.na(pair_data$predictedHC)),
  HC_overlap = mean(pair_data$HC_overlap),
  UD_ID = pair_data$removedUD[1],
  UD_acc = sum(na.omit(pair_data$predictedUD == "UD"))/100,
  UD_totacc = sum(na.omit(pair_data$predictedUD == "UD"))/(100 -sum(is.na(pair_data$predictedUD)) ),
  UD_NA = sum(is.na(pair_data$predictedUD)),
  UD_overlap = mean(pair_data$UD_overlap)
 )
  if(s==1){pair_out<-pair_info
  }else{pair_out<-rbind(pair_out,pair_info)}
    
}

UD_acc<-pair_out %>%
  group_by(UD_ID) %>%
  summarise_at( vars(UD_acc,UD_totacc,UD_NA,UD_overlap ),list(mean = function(x){base::mean(x=x, na.rm = T) } )) %>% 
  as.data.frame()

HC_acc<-pair_out %>%
  group_by(HC_ID) %>%
  summarise_at( vars(HC_acc,HC_totacc,HC_NA,HC_overlap ),list(mean = function(x){base::mean(x=x, na.rm = T)})) %>% 
  as.data.frame()

print("Distribution of permutation accuracy including models where no variables were selected")
quantile(c(UD_acc$UD_acc,HC_acc$HC_acc ))
print("Distribution of permutation accuracy excluding models where no variables were selected")
quantile(c(UD_acc$UD_totacc,HC_acc$HC_totacc ))
print("Distribution of permutation accuracy in UD excluding models where no variables were selected")
quantile(c(UD_acc$UD_totacc ))
print("Distribution of permutation accuracy in HC excluding models where no variables were selected")
quantile(c(HC_acc$HC_totacc ))
print("Distribution of number of models (per subject) where no variables were selected")
quantile(c(UD_acc$UD_NA,HC_acc$HC_NA ))

# quantile(c(pair_out$HC_NA ))
# sum(c(pair_out$HC_NA ))/dim(glmnet_file2)[1]
print("Mean permutation accuracy excluding models where no variables were selected")
mean(c(UD_acc$UD_totacc,HC_acc$HC_totacc ))

#########################
# feature frequency

feature_freq<-table(glmnet_file$Region)/dim(glmnet_file2)[1]*100
feature_freq<-feature_freq[-c(1,2)]
#hist(feature_freq)

feature_freq<-as.data.frame(t(feature_freq),stringsAsFactors = F)
feature_freq<-feature_freq[,-1]
colnames(feature_freq)<-c("Variable","PermutationFrequency")

#(quantile(feature_freq$PermutationFrequency)[5] - median(feature_freq$PermutationFrequency))/IQR(feature_freq$PermutationFrequency)

m<-3.5

upper_bound<- median(feature_freq$PermutationFrequency) + (m* IQR(feature_freq$PermutationFrequency))
lower_bound<- median(feature_freq$PermutationFrequency) - (m* IQR(feature_freq$PermutationFrequency))

true_freq<-table(glmnet_truefile$Region)/dim(glmnet_truefile2)[1]*100
true_freq<-true_freq[-c(1,2)]

true_freq<-as.data.frame(t(true_freq),stringsAsFactors = F)
true_freq<-true_freq[,-1]
colnames(true_freq)<-c("Variable","TrueFrequency")

frequency_compare<-merge(true_freq,feature_freq,by="Variable",all.x = T)
frequency_compare$TrueFrequency[is.na(frequency_compare$TrueFrequency)]<-0


frequency_compare<-frequency_compare[order(frequency_compare$TrueFrequency,decreasing = T),]

frequency_compare$Variable2<-factor(frequency_compare$Variable, levels=frequency_compare$Variable)


write.csv(x = frequency_compare,file = "MethodX_data/outputs/glmnet/glmnet_variable_selection.csv")

print("Variable selection saved")
sink()
