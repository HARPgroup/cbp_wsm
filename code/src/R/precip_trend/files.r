#Create directories and regression files
args <- commandArgs(trailingOnly = TRUE)

dataset<-args[1]
year1<-as.numeric(args[2])
year2<-as.numeric(args[3])

seasons<-c("fall","spring","summer","winter")
statvars<-c("frequency","intensity","total")
regresvars<-c("RSQ","INTERCEPT","SLOPE","PVALUE","TAU","KPVALUE")
Qs<-c("Q20","Q30","Q40","Q50","Q60","Q70","Q80","Q90","Q100")

wdir<-paste("./../../../../input/scenario/climate/prad/",dataset,"/txt/qstats_seasonal_",year1,"_",year2,"/",sep="")

for (i in 1:length(statvars)){
  stat  <-paste(statvars[i])

  for(ii in 1:length(seasons)){
    season  <-paste(seasons[ii])

    for (iii in 1:length(Qs)){
        breaks  <-paste(Qs[iii])

        outfile<-paste(wdir,"regressions_",breaks,"_",stat,"_",season,"_",year1,"_",year2,".csv",sep="")
        cat("SEG","STAT",regresvars,file=outfile,sep=",","\n")

    }
  }
}

