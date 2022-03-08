#Reads precip file and label quantiles classes and
#calculates statistics within those classes
#quantiles are points taken at regular intervals from the cumulative distribution function
args <- commandArgs(trailingOnly = TRUE)
#Variables
dataset<-args[1]
land<-args[2]
qyear1<-as.numeric(args[3])
qyear2<-as.numeric(args[4])
year1<-as.numeric(args[5])
year2<-as.numeric(args[6])

years<-seq(year1,year2,by=1)
Qbreaks<-c("Q0","Q20","Q30","Q40","Q50","Q60","Q70","Q80","Q90","Q100")

#Directories and files
wdir<-paste("./../../../../input/scenario/climate/prad/",dataset,"/txt/",sep="")
wdir2<-paste(wdir,"qstats_seasonal_",year1,"_",year2,"/",sep="")

precipfile<-paste(wdir,"daily_",land,".csv",sep="")
exist <- file.exists(precipfile)

if (exist==TRUE) {
  cat(land," ")

###########################################################################
  precipdata<-read.csv(file=precipfile)
  attach(precipdata)
  winterdata <- subset(precipdata,year >= qyear1 & year <= qyear2 & month <= 3)
  attach (winterdata)

#quantile classes
  data <- data.frame(year1=year,month1=month,day1=day,precip1=precip.in.,quantile1=rep(NA))
  data[data<0.0003937008] <- NA #remove data < 0.01mm (0 inches)
  qbreak <- with(data, quantile(precip1, probs=c(0,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), na.rm=TRUE))

  statfile<-paste(wdir2,"qbreaks_winter_",land,"_",qyear1,"_",qyear2,".csv",sep="")
  cat("STAT",Qbreaks,file=statfile,sep=",","\n")
  cat("qbreaks",qbreak,file=statfile,append=TRUE,sep=",","\n")
  
  detach(precipdata)
  detach(winterdata)

###########################################################################
  precipdata<-read.csv(file=precipfile)
  attach(precipdata)
  springdata <- subset(precipdata,year >= qyear1 & year <= qyear2 & month >3 & month<=6)
  attach (springdata)
#quantile classes
  data <- data.frame(year1=year,month1=month,day1=day,precip1=precip.in.,quantile1=rep(NA))
  data[data<0.0003937008] <- NA #remove data < 0.01mm (0 inches)
  qbreak <- with(data, quantile(precip1, probs=c(0,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), na.rm=TRUE))

  statfile<-paste(wdir2,"qbreaks_spring_",land,"_",qyear1,"_",qyear2,".csv",sep="")
  cat("STAT",Qbreaks,file=statfile,sep=",","\n")
  cat("qbreaks",qbreak,file=statfile,append=TRUE,sep=",","\n")
  
  detach(precipdata)
  detach(springdata)


###########################################################################
  precipdata<-read.csv(file=precipfile)
  attach(precipdata)
  summerdata <- subset(precipdata,year >= qyear1 & year <= qyear2 & month >6 & month<=9)
  attach (summerdata)
#quantile classes
  data <- data.frame(year1=year,month1=month,day1=day,precip1=precip.in.,quantile1=rep(NA))
  data[data<0.0003937008] <- NA #remove data < 0.01mm (0 inches)
  qbreak <- with(data, quantile(precip1, probs=c(0,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), na.rm=TRUE))
 
  statfile<-paste(wdir2,"qbreaks_summer_",land,"_",qyear1,"_",qyear2,".csv",sep="")
  cat("STAT",Qbreaks,file=statfile,sep=",","\n")
  cat("qbreaks",qbreak,file=statfile,append=TRUE,sep=",","\n")

  detach(precipdata)
  detach(summerdata)


###########################################################################
  precipdata<-read.csv(file=precipfile)
  attach(precipdata)
  falldata <- subset(precipdata,year >= qyear1 & year <= qyear2 & month >9 & month <=12)
  attach (falldata)
#quantile classes
  data <- data.frame(year1=year,month1=month,day1=day,precip1=precip.in.,quantile1=rep(NA))
  data[data<0.0003937008] <- NA #remove data < 0.01mm (0 inches)
  qbreak <- with(data, quantile(precip1, probs=c(0,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), na.rm=TRUE))

  statfile<-paste(wdir2,"qbreaks_fall_",land,"_",qyear1,"_",qyear2,".csv",sep="")
  cat("STAT",Qbreaks,file=statfile,sep=",","\n")
  cat("qbreaks",qbreak,file=statfile,append=TRUE,sep=",","\n")
  
  detach(precipdata)
  detach(falldata)

}



