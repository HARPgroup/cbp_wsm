# This code summarize and aggregate input loads by basin 
# using files in ./output/input/scenario/summary/
# USAGE : Rscript summary_input_load_basin.r scenario basin startyear endyear

args <- commandArgs(trailingOnly = TRUE)

#Variables
inputtype<-c("atdep","manure","fert","legume","uptake")
scenario<-args[1]
basin<-args[2]
startyear<-as.numeric(args[3])
endyear<-as.numeric(args[4])

#Directories
dir<-"./../../../../output/"
wdir<-paste(dir,"input/",scenario,"/",sep="")

source("./../vars/p6landuses.r")

for (i in 1:length(inputtype)) {
  inptype<-paste(inputtype[i])

  cat("Summarizing ",scenario,basin,inptype,"\n")

  tmonth=(endyear-startyear+1)*12
  vector1 <- seq(1,tmonth,by=1)
  vector0 <- vector1*0
  monthn <- seq(1,12,by=1)
  year <-  rep(startyear:endyear, each=12)
  month <- vector0 + monthn
  no3nload <- vector0
  nh3nload <- vector0
  orgnload <- vector0
  po4pload <- vector0
  orgpload <- vector0

  outfile<-paste(wdir,"summary/",basin,"/",basin,"_monthly_load_",inptype,".csv",sep="")
  cat("year","month","no3n","nh3n","orgn","po4p","orgp",file=outfile,sep=",","\n")
  cat("Creating ",outfile,"\n")

  for (ii in 1:length(p6lu)) {
    lu <- p6lu[ii]

    loadfile<-paste(wdir,"summary/",basin,"/",basin,"_monthly_load_",inptype,"_",lu,".csv",sep="")
    exist <- file.exists(loadfile)
    cat("File ",loadfile," exist = ",exist,"\n")

    if (exist==TRUE) {
        loaddata <- read.csv(loadfile)
        attach(loaddata)
        no3nload <- no3nload + no3n
        nh3nload <- nh3nload + nh3n
        orgnload <- orgnload + orgn
        po4pload <- po4pload + po4p
        orgpload <- orgpload + orgp
        detach(loaddata)
      }
  }
  allload<-paste(year,month,no3nload,nh3nload,orgnload,po4pload,orgpload,sep=",")
  cat(allload,file=outfile,sep=",",append=TRUE,fill=1)
}



