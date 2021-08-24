library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(ggseg)
library(ggsegExtra)
library(ggsci)
library(ggsegGlasser)
library(stringr)

# location of Supplemental Table 3
plot_data = read.csv(file = "MethodX_data/outputs/regressions/regression_dd_control_myelin.csv",stringsAsFactors = F)

plot_data$label<-plot_data$Y

plot_data$label<-str_replace(string = plot_data$label,pattern = "[.]",replacement = "-")
plot_data$label<-str_replace(string = plot_data$label,pattern = "_ROI",replacement = "")
plot_data$label<-str_replace(string = plot_data$label,pattern = "R_",replacement = "rh_R_")
plot_data$label<-str_replace(string = plot_data$label,pattern = "L_",replacement = "lh_L_")


plot_data2<-plot_data %>% dplyr::select(c("label","T" ))
colnames(plot_data2)[2]<-"Stat"
row.names(plot_data2)<-NULL
#plot_data2<-plot_data2[-which(plot_data2$label == "lh_L_10pp"),]


J<-colorRampPalette(colors = c(rgb_gsea(n = 2)[2], "#FFFFFF", rgb_gsea(n = 2)[1]),
                    space = "rgb",interpolate = "linear")


plot1<-ggseg(.data = plot_data2,mapping=aes(fill=Stat),atlas = glasser,
                   position="stacked",color="black",size=.25)+
  scale_fill_gradientn(colours = J(n = 512),na.value = "lightgrey", 
                       limits = c(max(abs(range(plot_data2$Stat)))*-1,max(abs(range(plot_data2$Stat)))),breaks=seq(-4,4,by=1),
                       guide = guide_colorbar( frame.colour = "black",
                                               barwidth = 11,
                                               barheight =1,
                                               draw.ulim = T,
                                               title.position = "top",
                                               ticks.colour = "black",
                                               frame.linewidth = 1,
                                               label.theme = element_text(size=10),
                                               ticks.linewidth = 1))+
  labs(fill =   "T-statistic UD vs HC"    )+
  #ggtitle("Whole Brain")+
  theme_custombrain(
    plot.background = "white",
    text.colour = "black",
    text.size = 12,
    text.family = "sans"
  )+
  # theme_pubr()+
  theme(legend.text=element_text(size=12),
        legend.position = "top",
        legend.title = element_text(size=12,vjust = .8))  #ggtitle(label = Description)+
plot1


#####
#####
#####
# plot_data$label<-plot_data$Y
# 
# plot_data$label<-str_replace(string = plot_data$label,pattern = "[.]",replacement = "-")
# plot_data$label<-str_replace(string = plot_data$label,pattern = "_ROI",replacement = "")
# plot_data$label<-str_replace(string = plot_data$label,pattern = "R_",replacement = "rh_R_")
# plot_data$label<-str_replace(string = plot_data$label,pattern = "L_",replacement = "lh_L_")
# 
plot_data$Stat2<-plot_data$`Variable Selection Frequency`
plot_data2<-plot_data %>% dplyr::select(c("label","Stat2" ))
row.names(plot_data2)<-NULL
#plot_data2<-plot_data2[-which(plot_data2$label == "lh_L_10pp"),]


# J<-colorRampPalette(colors = c(rgb_gsea(n = 2)[2], "#FFFFFF", rgb_gsea(n = 2)[1]),
#                     space = "rgb",interpolate = "linear")

J<- ggsci::rgb_material(palette = "red",n = 512)
plot2<-ggseg(.data = plot_data2,mapping=aes(fill=Stat2),atlas = glasser,
             position="stacked",color="black",size=.25)+
  scale_fill_gradientn(colours =  ggsci::rgb_material(palette = "purple",n = 512),
                       na.value = "lightgrey", 
                       limits = c(0,100),
                       breaks=seq(0,100,by=20),
                       guide = guide_colorbar( frame.colour = "black",
                                               barwidth = 11,
                                               barheight =1,
                                               draw.ulim = T,
                                               title.position = "top",
                                               ticks.colour = "black",
                                               frame.linewidth = 1,
                                               label.theme = element_text(size=10),
                                               ticks.linewidth = 1))+
  labs(fill =   "Variable selection frequency (%)"    )+
  #ggtitle("Whole Brain")+
  theme_custombrain(
    plot.background = "white",
    text.colour = "black",
    text.size = 12,
    text.family = "sans"
  )+
  # theme_pubr()+
  theme(legend.text=element_text(size=12),
        legend.position = "top",
        legend.title = element_text(size=12,vjust = .8))  #ggtitle(label = Description)+
plot2


brain_plot<-ggarrange(plot1,plot2,ncol = 2,labels = c("A","B"),common.legend = F)
ggsave(filename = "MethodX_data/scripts/figures/brain_plots.png",plot = brain_plot,width = 10,height = 5,dpi=300)
ggsave(filename = "MethodX_data/scripts/figures/brain_plots_600.pdf",plot = brain_plot,width = 10,height = 5,dpi=600)
############################
