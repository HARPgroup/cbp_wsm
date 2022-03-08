args <- commandArgs(trailingOnly = TRUE)

#Variables
scenario<-args[1]

#Directories and files

dir<-"./../../../../output/"

wdir<-paste(dir,"input/",scenario,"/",sep="")
dir.create(file.path(wdir,"loads"))








