#This code calculate monthly input loads from feeding space
args <- commandArgs(trailingOnly = TRUE)

scenario<-args[1]
land<-paste(substr(args[2], 1, 6))
river<-paste(substr(args[2], 7, 19))
feedlu<-c("fsp","fnp")
cat(land,river,sep=",","\n")

#Directories
dir<-"./../../../../output/"
wdir<-paste(dir,"input/",scenario,"/",sep="")

for (x in 1:length(feedlu)) {
  lu<-paste(feedlu[x])

# AFO manure
  acrefile<-paste(dir,"eor/monthly/",scenario,"/",land,"_to_",river,"_",lu,".mon",sep="")
  exist1 <- file.exists(acrefile)
  if (exist1==TRUE) {
    acredata <- read.table(file=acrefile,header=TRUE,sep=",")
    attach(acredata)
    nodata<-names(acredata)[1]

    if (nodata=="NO.LOADS"){
        cat("NO LOADS","\n")
    }else{
        outfile<-paste(wdir,"loads/monthly_load_manure_",land,"_",river,"_",lu,".csv",sep="")
        cat("year","month","no3n","nh3n","orgn","po4p","orgp",file=outfile,sep=",","\n")
        allload<-paste(YEAR,MONTH,NO23,NH3X,ORGN,PO4X,ORGP,sep=",")
        cat(allload,file=outfile,sep=",",append=TRUE,fill=1)
    }
  }

# AFO Atdep
  acrefile<-paste(dir,"eor/monthly/",scenario,"/",land,"_to_",river,"_",lu,".mon",sep="")
  ratefile<-paste(wdir,"monthly_atdep_",land,"_",lu,".csv",sep="")
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
        outfile<-paste(wdir,"loads/monthly_load_atdep_",land,"_",river,"_",lu,".csv",sep="")
        cat("year","month","no3n","nh3n","orgn","po4p","orgp",file=outfile,sep=",","\n")
        no3nload<-ACRE*no3n
        nh3nload<-ACRE*nh3n
        orgnload<-ACRE*orgn
        po4pload<-ACRE*po4p
        orgpload<-ACRE*orgp
        allload<-paste(year,month,no3nload,nh3nload,orgnload,po4pload,orgpload,sep=",")
        cat(allload,file=outfile,sep=",",append=TRUE,fill=1)
    }
  }
}







