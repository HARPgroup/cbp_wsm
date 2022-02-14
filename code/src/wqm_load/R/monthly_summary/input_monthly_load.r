#This code calculate monthly input loads
#USAGE
#Rscript input_monthly_load_rscript.r landriverseg
args <- commandArgs(trailingOnly = TRUE)

#Variables
#source("./../vars/p532landuses_noafo.r")
inputtype<-c("atdep","manure","fert","legume","uptake")

scenario<-args[1]
land<-paste(substr(args[2], 1, 6))
river<-paste(substr(args[2], 7, 19))
lu<-args[3]
dir<-"./../../../../output/"
wdir<-paste(dir,"input/",scenario,"/",sep="")

cat(land,river,lu,sep=",","\n")

for (x in 1:length(inputtype)) {
  inptype<-paste(inputtype[x])

#  for (ii in 1:length(p532lu)) {
#    lu <- p532lu[ii]
    acrefile<-paste(dir,"eor/monthly/",scenario,"/",land,"_to_",river,"_",lu,".mon",sep="")
    ratefile<-paste(wdir,"monthly_",inptype,"_",land,"_",lu,".csv",sep="")
    exist1 <- file.exists(acrefile)
    exist2 <- file.exists(ratefile)

    if (exist1==TRUE & exist2==TRUE) {
      acredata <- read.table(file=acrefile,header=TRUE,sep=",")
      attach(acredata)
      nodata<-names(acredata)[1]
      ratedata <- read.csv(ratefile)
      attach(ratedata)

      if (nodata=="NO.LOADS"){
        cat("NO LOADS","\n")
      }else{
        outfile<-paste(wdir,"/loads/monthly_load_",inptype,"_",land,"_",river,"_",lu,".csv",sep="")
        cat("year","month","no3n","nh3n","orgn","po4p","orgp",file=outfile,sep=",","\n")
        no3nload<-ACRE*no3n
        nh3nload<-ACRE*nh3n
        orgnload<-ACRE*orgn
        po4pload<-ACRE*po4p
        orgpload<-ACRE*orgp
        allload<-paste(year,mm,no3nload,nh3nload,orgnload,po4pload,orgpload,sep=",")
        cat(allload,file=outfile,sep=",",append=TRUE,fill=1)
      }
    } else {
      cat("FILE NOT FOUND ","\n")
    }
}


