# process glmnet with varying cvs

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(readxl)


loocv_outer = list.files("MethodX_data/outputs/cvs/loocv_inneronly/")

original_retained = read.csv("MethodX_data/outputs/regressions/regression_dd_control_myelin.csv",stringsAsFactors = F)
original_retained=original_retained$Y

for(i in 1:length(loocv_outer)){
  print(i)
  cv = as.numeric(str_split(string = loocv_outer[i],pattern = "_",simplify = T)[8])
  
  file_in = fread(input =paste("MethodX_data/outputs/cvs/loocv_inneronly/",loocv_outer[i],sep="") ,header = T,stringsAsFactors = F,data.table = F,fill=T)
  
  
  file_in<-file_in[-(dim(file_in)[1]-1),] # last line is "====="
  file_in$subset<-as.numeric(file_in$subset) # this is originally a character because of the "===="
  file_in<-filter(file_in, !is.na(subset))
  file_in<-na.omit(file_in)
  region_num =  t(as.matrix(quantile(file_in$nfeatures,probs = c(seq(0,1,.125)))))
  colnames(region_num) = paste("region num", colnames(region_num))
  
  file_in = file_in %>% dplyr::filter(Region != "(Intercept)")
  
  file_in<-file_in[,match( c("Region"),colnames(file_in))]
  
  region_count = table(file_in)
  
  total_selected = dim(region_count)
  region_count_perc = region_count/1833*100
  region_dist = t(as.matrix(quantile(region_count_perc,probs = c(seq(0,1,.125)))))
  
  
  region_count_perc = region_count_perc[region_count_perc>3.77]
  
  
  total_retained = dim(region_count_perc)
  
 orig_overlap = sum(!is.na(match(names(region_count_perc),original_retained)))/33*100
  
 file_final = data.frame(Outer = 1,
                         Inner = cv,
                         total_selected = total_selected,
                         total_retained = total_retained,
                         orig_overlap = orig_overlap)
 
 file_final = cbind(file_final,region_dist,  region_num )
 
 
  if(i ==1){cv_out = file_final}else{cv_out = rbind(cv_out,file_final)}
}

#cv_out$total_retained[5] = 33

cv_out_1 = cv_out

##########################

loocv_inner = list.files("MethodX_data/outputs/cvs/")
loocv_inner = loocv_inner[grep(pattern = "*.txt",loocv_inner)]

for(i in 1:length(loocv_inner)){
  print(i)
  
  outer_cv = as.numeric(str_split(string = loocv_inner[i],pattern = "_",simplify = T)[8])
  inner_cv = as.numeric(str_split(string = loocv_inner[i],pattern = "_",simplify = T)[10])
  
  file_in = fread(input =loocv_inner[i] ,header = T,stringsAsFactors = F,data.table = F,fill=T)
  
  
  file_in<-file_in[-(dim(file_in)[1]-1),] # last line is "====="

  modnum = file_in %>% dplyr::filter(Region == "(Intercept)")
  
  file_in = file_in %>% dplyr::filter(Region != "(Intercept)")
  region_num =  t(as.matrix(quantile(file_in$nfeatures,probs = c(seq(0,1,.125)))))
  
  file_in<-file_in[,match( c("Region"),colnames(file_in))]
  
  region_count = table(file_in)
  
  total_selected = dim(region_count)
  region_count_perc = region_count/1833*100
  region_dist = t(as.matrix(quantile(region_count_perc,probs = c(seq(0,1,.125)))))
  
  colnames(region_num) = paste("region num", colnames(region_num))
  
  region_count_perc = region_count_perc[region_count_perc>3.77]
  
  total_retained = dim(region_count_perc)
  
  orig_overlap = sum(!is.na(match(names(region_count_perc),original_retained)))/34*100
  
  file_final = data.frame(Outer = outer_cv,
                          Inner = inner_cv,
                          total_selected = total_selected,
                          total_retained = total_retained,
                          orig_overlap = orig_overlap)
  file_final = cbind(file_final,region_dist,region_num)
  
  if(i ==1){cv_out = file_final}else{cv_out = rbind(cv_out,file_final)}
}

cv_final = as.data.frame(rbind(cv_out_1,cv_out))

cv_final$Inner[cv_final$Inner > 30] <- "LOO"
cv_final$Inner2 = cv_final$Inner
cv_final$Inner = as.factor(cv_final$Inner)
cv_final$Inner = factor(cv_final$Inner,levels(cv_final$Inner)[c(5,3,2,1,4)])



cv_final$Outer2 = as.factor(cv_final$Outer)
cv_final$Inner2 = paste("Training CV folds = ",cv_final$Inner)


cv_final$Inner2 = as.factor(cv_final$Inner2)
cv_final$Inner2 = factor(cv_final$Inner2,levels(cv_final$Inner2)[c(5,3,2,1,4)])



region_plot_total = ggplot(data = cv_final,
                          aes(y = total_retained, x = Inner,fill = Outer2))+
  scale_fill_viridis_d()+
  #scale_y_continuous(limits = c(0,100))+
  geom_bar(stat = "identity", position=position_dodge(),color="black")+
  geom_hline(yintercept = 34,color="black",linetype = "dotted",size=1)+
  ylab(label = "Total number of variables retained")+
  xlab(label = "Number of CV folds used in elastic net training")+
  theme_classic()+
  labs(fill = "Pairs held-out ")
region_plot_total




ggsave("MethodX_data/outputs/cvs/CV_region_selection_results.png",width = 5,height = 4,units = "in",dpi = 300)
