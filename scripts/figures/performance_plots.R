library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readxl)


# location of Study Data
predicted<-read.csv(file = "MethodX_data/data/other_input/glmnet_performance.csv",stringsAsFactors = F)

# location of Supplemental Table 2
frequency_compare<-read.csv(file = "MethodX_data/outputs/glmnet/glmnet_variable_selection.csv",stringsAsFactors = F)



######
predicted$perc_cor <- predicted$perc_cor*100

dist_acc<- 
  ggplot(data = predicted,aes(x = perc_cor,fill=Group  ))+
  #scale_fill_viridis_d(option = "C",end = 0.60,direction = -1,alpha = 1)+  
  #ggsci::scale_fill_gsea()+
  scale_fill_manual(values = ggsci::rgb_gsea(n = 2,reverse = T))+
  
  geom_density(alpha = 0.5, aes(x=perc_cor, y=..density..*750,fill=Group),
               size=.5,bw=7.5) +
  geom_histogram(colour="black", binwidth=5,show.legend = TRUE,size=.5,
                 position = position_dodge(width = 2.5))+
  theme_classic()+
  font("legend.text",size = 10)+
  ggtitle(label = "Classification accuracy")+
 # ggtitle(label = "Distribution of classification accuracy across nested CVs")+
  #theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=2),
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(color="black"),
    axis.ticks = element_line(size=1,color="black"),
    axis.ticks.length=unit(0.2,"cm") )+
  ylab("Count")+
  xlab ("Subject-wise classification accuracy (%)")+
  scale_x_continuous(breaks = seq(0,100,by = 10))+
  #scale_x_continuous(breaks = c(0,2,4,6,8,10,12) )+
  scale_y_continuous(breaks = seq(0,30,by = 5),limits=c(0,25),
                     sec.axis = sec_axis(~./750,
                                        # breaks = seq(0,10,by=.2),
                                         name = "Density" ))
dist_acc

######

colnames(frequency_compare)<-c("Variable","TrueFrequency","PermutationFrequency","Retained")

m=3.5
upper_bound<- median(frequency_compare$PermutationFrequency) + (m* IQR(frequency_compare$PermutationFrequency))
lower_bound<- median(frequency_compare$PermutationFrequency) - (m* IQR(frequency_compare$PermutationFrequency))

frequency_compare$Variable2<-as.factor(frequency_compare$Variable)
frequency_compare$Variable2<-factor(frequency_compare$Variable2,
                                    levels = frequency_compare$Variable2[order(frequency_compare$TrueFrequency,decreasing = T)])


frequency_compare2<-pivot_longer(data = frequency_compare,
                                 cols = ends_with("Frequency"),
                                 names_to = "Type",
                                 values_to = "Frequency")
library(stringr)
library(ggrepel)
frequency_compare2$Type<-str_replace(string = frequency_compare2$Type,pattern = "Frequency",replacement = "")
frequency_compare2$Type<-str_replace(string = frequency_compare2$Type,pattern = "True",replacement = "True data")

frequency_compare2$Type<-as.factor(frequency_compare2$Type)
frequency_compare2$Type<-factor(frequency_compare2$Type,levels = c("True data","Permutation"))

freq_plot<-
  
  ggplot(frequency_compare2,aes(x=Variable2,y=Frequency,fill=Type))+
  geom_hline(yintercept = seq(0,100,by=10),color="lightgrey")+
  scale_y_continuous(breaks = seq(0,100,by=10))+
 
  scale_fill_viridis_d(begin = .2,end = .8)+
  
  geom_rect(aes(ymin = lower_bound,
                ymax=upper_bound,
                xmin=Variable2[1],
                xmax=Variable2[dim(frequency_compare2)[1]]),fill="darkgrey")+
  geom_point(color="black",shape=21,size=2,alpha=.75 )+
  font("legend.text",size = 12)+
  ggtitle(label = "Variable selection")+
  
  geom_hline(yintercept = upper_bound,color="black",linetype="dashed")+
  geom_hline(yintercept = lower_bound,color="black",linetype="dashed")+
  # geom_point(aes(y = PermutationFrequency),color="black",shape=21,
  #            fill = viridis::plasma(n = 1,begin = .9))+
  #annotate(geom = "text",label="IQ",x = 17,y = 87)+
  ylab(label = "Percent of models where variable is retained")+
  xlab(label = "")+
  labs(tag = "dashed line = median of permuted frequency +/- 3.5*IQR")+
  # ggtitle(label = "",
  #         subtitle =" " )+
  theme_classic()+ 
  theme(legend.title = element_blank(),
        axis.text = element_text(color="black"),
        legend.position = "top",
        plot.tag.position = c(.7,.02),
        plot.tag = element_text(size=8,color="#606060"))+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank())+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  geom_text_repel(
    data = dplyr::filter(.data = frequency_compare2, Type=="True data",Variable=="IQ" ),
    aes(label = Variable),
    size = 3,nudge_x = 5,
    box.padding = unit(0.45, "lines"),
    point.padding = unit(0.4, "lines")
  )


freq_plot
#ggsave(freq_plot,filename = "FrequencyComparison.png",width = 4,height = 4)

perf_plots<-ggarrange(dist_acc,freq_plot,labels = c("A","B"),ncol = 2,common.legend = F,widths = c(1.2,1))
ggsave(plot = perf_plots,filename = "MethodX_data/scripts/figures/Performance_plots.png",width = 10,height = 4,dpi=300)
ggsave(plot = perf_plots,filename = "MethodX_data/scripts/figures/Performance_plots_500.png",width = 10,height = 4,dpi=500)
