# Script to parcellate workbench cifti files.

# open rstudio from command line ["rstudio"] for wb_command system commands to work

# David Baranger 10/8/2020



library(stringr)
library(data.table)
library(dplyr)
# path for analysis directory/output name
out_path<-"MethodX_data/outputs/preprocessing"
out_dir<-"myelin_parcellation_All_Glasser"


inputdata<-read.csv("MethodX_data/data/other_input/data_clinical_and_parcels_all.csv")


inputdata<-inputdata %>% 
  dplyr::select(c("ID"))

inputdata$ID<-paste("sub-",inputdata$ID,sep="")

parcellation_folder<-"MethodX_data/" # wherever parcellation is downloaded to
parcellation_name<-"GlasserAtlas.32k.dlabel.nii"
atlaslabelfile<-"MethodX_data/GlasserRegions.txt"


# data location: 
data.dir<-     "/MethodX_data/data/mri_derivatives/primary" #Path to directory with subject folders
path.to.data<- "/ses-01/"      #Path to directory that the data being analyzed is in
data.filename<-"SmoothedMyelinMap_BC.32k_fs_LR.dscalar.nii" # data file name, assumed to start with subject ID
thickness.filename<-"midthickness.32k_fs_LR.surf.gii"        # thickness filename, L&R both exist, not specified here. 

stat_of_interest <- "MEAN" # MEAN, MIN, MAX, MEDIAN - comment out code in wb_command for anything but MEAN

########################################################################################################



out_dir<-paste(out_dir,"N",dim(inputdata)[1],format(Sys.time(), "%Y-%m-%d_%H.%M"),sep="_")


setwd(out_path)


## Make file structure

system(command = paste("mkdir ",out_dir,sep = ""))
 system(command = paste("mkdir ",out_dir,"/inputfiles",sep = ""))
 system(command=paste("mkdir ",out_dir,"/inputfiles/area_temp",sep = "")) 
system(command = paste("mkdir ",out_dir,"/outputfiles",sep = "")) 


for(subj in 1:dim(inputdata)[1]   ) {
 

system(command = paste("wb_command -cifti-parcellate ",
                       data.dir,"/",inputdata[subj,1],
                       path.to.data,inputdata[subj,1],".",
            data.filename," ",parcellation_folder,parcellation_name,
            " COLUMN " ,
            out_dir,"/outputfiles/",inputdata[subj,1], "parcellated.pscalar.nii",
            " -spatial-weights -left-area-surf ",
            data.dir,"/",inputdata[subj,1],path.to.data,inputdata[subj,1],".L.",thickness.filename,
            " -right-area-surf ",
            data.dir,"/",inputdata[subj,1],path.to.data,inputdata[subj,1],".R.",thickness.filename,
           " -method ",stat_of_interest,
            sep=""))           

system(command = paste("wb_command -cifti-convert -to-text ",
                       out_dir,"/outputfiles/",
                       inputdata[subj,1],"parcellated.pscalar.nii ",
                       out_dir,"/outputfiles/",
                       inputdata[subj,1],".txt",sep=""))

data<-read.table(file = paste(out_dir,"/outputfiles/",inputdata[subj,1],".txt",sep="" ),header = F)
colnames(data)<-inputdata[subj,1]

if(subj == 1){data_out<-data}
if(subj>1){data_out<-cbind(data_out,data)}
print(subj)

print(dim(data_out))
}



atlas<-fread(file =atlaslabelfile,data.table = F,fill = T,header = F)
ROIs<-atlas[rep(c(TRUE,FALSE),dim(atlas)[1]/2),]


row.names(data_out)<-ROIs
subjid<-colnames(data_out)
data_out<-as.data.frame(t(data_out))
data_out$SUBJ<-subjid

out_file<-paste(out_path,"/",out_dir,"/",out_dir,".csv",sep="")
write.table(x = data_out,file = out_file,sep = ",",row.names = F,col.names = T)
