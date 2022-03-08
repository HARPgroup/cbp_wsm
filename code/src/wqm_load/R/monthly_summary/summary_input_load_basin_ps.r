# This code summarize and aggregate PS input loads by basin
# using files in ./output/eos/monthly/scenario/
# USAGE : Rscript summary_input_load_basin_ps.r scenario basin startyear endyear

args <- commandArgs(trailingOnly = TRUE)

#Variables
inputtype<-c("wwtp","indus","cso","septic")
scenario<-args[1]
basin<-args[2]
startyear<-as.numeric(args[3])
endyear<-as.numeric(args[4])

#Directories and files
dir<-"./../../../../output/"
wdir<-paste(dir,"input/",scenario,"/",sep="")
seglist<-paste("./../seglists/",basin,".lrseg.r",sep="")
source(seglist)

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
  tssxload <- vector0
  tsedload <- vector0

  outfile<-paste(wdir,"summary/",basin,"/",basin,"_monthly_load_",inptype,".csv",sep="")
  cat("year","month","no3n","nh3n","orgn","po4p","orgp","tssx","tsed",file=outfile,sep=",","\n")
  cat("Creating ",outfile,"\n")
  cat(" ","\n")
  for (iii in 1:length(segments)){
      lrseg<-segments[iii]
      land<-paste(substr(lrseg, 1, 6))
      river<-paste(substr(lrseg, 7, 19))
      loadfile<-paste(dir,"eor/monthly/",scenario,"/",land,"_to_",river,"_",inptype,".mon",sep="")
      exist <- file.exists(loadfile)

      if (exist==TRUE) {
        loaddata <- read.csv(loadfile)
        attach(loaddata)
        no3nload <- no3nload + NO23
        nh3nload <- nh3nload + NH3X
        orgnload <- orgnload + ORGN
        po4pload <- po4pload + PO4X
        orgpload <- orgpload + ORGP
        tssxload <- tssxload + TSSX
        tsedload <- tsedload + TSED
        detach(loaddata)
      }
  }
  allload<-paste(year,month,no3nload,nh3nload,orgnload,po4pload,orgpload,tssxload,tsedload,sep=",")
  cat(allload,file=outfile,sep=",",append=TRUE,fill=1)
}


