args <- commandArgs(trailingOnly = TRUE)

#Variables
scenario<-args[1]
basin<-args[2]

#Directories and files

dir<-"./../../../../output/"

wdir<-paste(dir,"input/",scenario,"/",sep="")
dir.create(file.path(wdir,"summary"))

ndir<-paste(wdir,"summary/",sep="")
dir.create(file.path(ndir,basin))




