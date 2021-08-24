library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(ggseg)
library(ggsegExtra)
library(ggsci)
library(ggsegGlasser)
library(stringr)
library(ggbeeswarm)
library(gridExtra)


# location of Supplemental Table 4
plot_data<-read.csv(file = "/data/davidhome/MethodX_data/outputs/regressions/regression_demo_clin_myelin.csv",stringsAsFactors = F)

# location of Study Data
input_data<-read.csv(file = "MethodX_data/data/other_input/data_clinical_and_parcels_all.csv",stringsAsFactors = F)

input_data <- input_data %>% dplyr::filter(Group == "UD")




dat<-dplyr::filter(.data = plot_data,Sample == "UD-Only",X == "Antidepressants",P<0.05)

input_data<-dplyr::filter(input_data, !is.na(AD_1))
input_data$Antidepressants <- as.factor(input_data$AD_1>0)
input_data$Antidepressants<-factor(input_data$Antidepressants,labels = c("No","Yes"))

AD_plots<-list()

for(i in 1: dim(dat)[1]){
  
  y_var<-which(colnames(input_data) == dat$Y[i])
  y_data<- input_data[,y_var]
  y_name<-stringr::str_replace(string = dat$Y[i],pattern = "_ROI",replacement = "")
  y_name<-stringr::str_replace(y_name,pattern = "R_",replacement = "Right ")
  y_name<-stringr::str_replace(y_name,pattern = "L_",replacement = "Left ")
 # data_plot<-cbind(input_data$Antidepressants,y_data)
  data_plot<-data.frame(Antidepressants=input_data$Antidepressants,y_data=y_data)
  
  AD_plots[[i]]<-ggplot(data = data_plot,aes(x = Antidepressants,y =y_data,fill=Antidepressants))+
   
    scale_fill_viridis_d( option = "D",begin = .25,end = .75)+
    geom_violin(alpha=0.4,position = position_dodge(width = .75), size=1, color="black",scale = "width") +
    
    geom_boxplot(notch = TRUE,  outlier.size =-1, color="black",lwd=1, alpha = 0.5,show.legend = F)+
     geom_quasirandom(shape = 21,size=2,dodge.width = .75,
                      width = .25,show.legend = F)+
    #ggtitle(label = "",paste(strwrap(iqm_t2$iqm_definition[i],50),collapse = "\n"))+
    #geom_point( shape = 21,size=2, position = position_jitterdodge(jitter.width = .25), color="black",alpha=0.25,show.legend = F)+
    theme_pubr()+
    ggtitle(label=y_name)+
    
    xlab(label =""   )+
    ylab(label ="T1w/T2w")+
    # ggtitle(label = "PMCP")+
    #scale_y_continuous(breaks = seq(0,1,by = 0.1),limits = c(0,.6))+
   # geom_hline(yintercept = api_upper,color="black",linetype="dashed",size=1)+
    #geom_hline(yintercept = api_lower,color="black",linetype="dashed",size=1)+
    
    theme(axis.line = element_line(size = 1),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          #panel.border = element_rect(colour = "black", fill=NA, size=2),
          axis.ticks = element_line(size=1,color="black"),
          axis.ticks.length=unit(0.2,"cm"),
          legend.position ="top")
 


}


plots = arrangeGrob(grobs = AD_plots,ncol = 2 )
ggsave("MethodX_data/scripts/figures/AD_plots.png",dpi=300, plots, width = 8, height = 8, units = "in")

