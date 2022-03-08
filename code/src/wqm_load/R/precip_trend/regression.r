args <- commandArgs(trailingOnly = TRUE)
library(Kendall)

#Variables
dataset<-args[1]
land<-args[2]
year1<-as.numeric(args[3])
year2<-as.numeric(args[4])
seasons<-c("fall","spring","summer","winter")
statvars<-c("frequency","intensity","total")

#Directories and files
wdir<-paste("./../../../../input/scenario/climate/prad/",dataset,"/txt/qstats_seasonal_",year1,"_",year2,"/",sep="")

for (i in 1:length(statvars)){
  stat  <-paste(statvars[i])

  for(ii in 1:length(seasons)){
    season  <-paste(seasons[ii])
    statfile<-paste(wdir,"qstats_",land,"_",year1,"_",year2,"_",season,"_",stat,".csv",sep="")
    exist <- file.exists(statfile)

    if (exist==TRUE) {
      qdata<-read.csv(statfile)
      attach(qdata)
      cat(land," ")

      outland<-paste(wdir,"regressions_",land,"_",stat,"_",season,"_",year1,"_",year2,".csv",sep="")
      cat("QUANTILE","STAT","RSQ","INTERCEPT","SLOPE","PVALUE","TAU","KPVALUE",file=outland,sep=",","\n")
 
      outall<-paste(wdir,"regressions_Q20_",stat,"_",season,"_",year1,"_",year2,".csv",sep="")
      rsq<-summary(lm(formula=Q20~YEARN))$r.squared
      intercept<-summary(lm(formula=Q20~YEARN))$coefficients[1,1]
      slope<-summary(lm(formula=Q20~YEARN))$coefficients[2,1]
      pvalue<-summary(lm(formula=Q20~YEARN))$coefficients[2,4]
      kend    <- Kendall(Q20,YEARN)
      tau     <- as.numeric(kend[1])
      kpvalue  <- as.numeric(kend[2])
      cat("Q20",stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outland,append=TRUE,sep=",","\n")
      cat(land,stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outall,append=TRUE,sep=",","\n")

      outall<-paste(wdir,"regressions_Q30_",stat,"_",season,"_",year1,"_",year2,".csv",sep="")
      rsq<-summary(lm(formula=Q30~YEARN))$r.squared
      intercept<-summary(lm(formula=Q30~YEARN))$coefficients[1,1]
      slope<-summary(lm(formula=Q30~YEARN))$coefficients[2,1]
      pvalue<-summary(lm(formula=Q30~YEARN))$coefficients[2,4]
      kend    <- Kendall(Q30,YEARN)
      tau     <- as.numeric(kend[1])
      kpvalue  <- as.numeric(kend[2])
      cat("Q30",stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outland,append=TRUE,sep=",","\n")
      cat(land,stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outall,append=TRUE,sep=",","\n")

      outall<-paste(wdir,"regressions_Q40_",stat,"_",season,"_",year1,"_",year2,".csv",sep="")
      rsq<-summary(lm(formula=Q40~YEARN))$r.squared
      intercept<-summary(lm(formula=Q40~YEARN))$coefficients[1,1]
      slope<-summary(lm(formula=Q40~YEARN))$coefficients[2,1]
      pvalue<-summary(lm(formula=Q40~YEARN))$coefficients[2,4]
      kend    <- Kendall(Q40,YEARN)
      tau     <- as.numeric(kend[1])
      kpvalue  <- as.numeric(kend[2])
      cat("Q40",stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outland,append=TRUE,sep=",","\n")
      cat(land,stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outall,append=TRUE,sep=",","\n")

      outall<-paste(wdir,"regressions_Q50_",stat,"_",season,"_",year1,"_",year2,".csv",sep="")
      rsq<-summary(lm(formula=Q50~YEARN))$r.squared
      intercept<-summary(lm(formula=Q50~YEARN))$coefficients[1,1]
      slope<-summary(lm(formula=Q50~YEARN))$coefficients[2,1]
      pvalue<-summary(lm(formula=Q50~YEARN))$coefficients[2,4]
      kend    <- Kendall(Q50,YEARN)
      tau     <- as.numeric(kend[1])
      kpvalue  <- as.numeric(kend[2])
      cat("Q50",stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outland,append=TRUE,sep=",","\n")
      cat(land,stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outall,append=TRUE,sep=",","\n")

      outall<-paste(wdir,"regressions_Q60_",stat,"_",season,"_",year1,"_",year2,".csv",sep="")
      rsq<-summary(lm(formula=Q60~YEARN))$r.squared
      intercept<-summary(lm(formula=Q60~YEARN))$coefficients[1,1]
      slope<-summary(lm(formula=Q60~YEARN))$coefficients[2,1]
      pvalue<-summary(lm(formula=Q60~YEARN))$coefficients[2,4]
      kend    <- Kendall(Q60,YEARN)
      tau     <- as.numeric(kend[1])
      kpvalue  <- as.numeric(kend[2])
      cat("Q60",stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outland,append=TRUE,sep=",","\n")
      cat(land,stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outall,append=TRUE,sep=",","\n")

      outall<-paste(wdir,"regressions_Q70_",stat,"_",season,"_",year1,"_",year2,".csv",sep="")
      rsq<-summary(lm(formula=Q70~YEARN))$r.squared
      intercept<-summary(lm(formula=Q70~YEARN))$coefficients[1,1]
      slope<-summary(lm(formula=Q70~YEARN))$coefficients[2,1]
      pvalue<-summary(lm(formula=Q70~YEARN))$coefficients[2,4]
      kend    <- Kendall(Q70,YEARN)
      tau     <- as.numeric(kend[1])
      kpvalue  <- as.numeric(kend[2])
      cat("Q70",stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outland,append=TRUE,sep=",","\n")
      cat(land,stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outall,append=TRUE,sep=",","\n")

      outall<-paste(wdir,"regressions_Q80_",stat,"_",season,"_",year1,"_",year2,".csv",sep="")
      rsq<-summary(lm(formula=Q80~YEARN))$r.squared
      intercept<-summary(lm(formula=Q80~YEARN))$coefficients[1,1]
      slope<-summary(lm(formula=Q80~YEARN))$coefficients[2,1]
      pvalue<-summary(lm(formula=Q80~YEARN))$coefficients[2,4]
      kend    <- Kendall(Q80,YEARN)
      tau     <- as.numeric(kend[1])
      kpvalue  <- as.numeric(kend[2])
      cat("Q80",stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outland,append=TRUE,sep=",","\n")
      cat(land,stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outall,append=TRUE,sep=",","\n")

      outall<-paste(wdir,"regressions_Q90_",stat,"_",season,"_",year1,"_",year2,".csv",sep="")
      rsq<-summary(lm(formula=Q90~YEARN))$r.squared
      intercept<-summary(lm(formula=Q90~YEARN))$coefficients[1,1]
      slope<-summary(lm(formula=Q90~YEARN))$coefficients[2,1]
      pvalue<-summary(lm(formula=Q90~YEARN))$coefficients[2,4]
      kend    <- Kendall(Q90,YEARN)
      tau     <- as.numeric(kend[1])
      kpvalue  <- as.numeric(kend[2])
      cat("Q90",stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outland,append=TRUE,sep=",","\n")
      cat(land,stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outall,append=TRUE,sep=",","\n")

      outall<-paste(wdir,"regressions_Q100_",stat,"_",season,"_",year1,"_",year2,".csv",sep="")
      rsq<-summary(lm(formula=Q100~YEARN))$r.squared
      intercept<-summary(lm(formula=Q100~YEARN))$coefficients[1,1]
      slope<-summary(lm(formula=Q100~YEARN))$coefficients[2,1]
      pvalue<-summary(lm(formula=Q100~YEARN))$coefficients[2,4]
      kend    <- Kendall(Q100,YEARN)
      tau     <- as.numeric(kend[1])
      kpvalue  <- as.numeric(kend[2])
      cat("Q100",stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outland,append=TRUE,sep=",","\n")
      cat(land,stat,rsq,intercept,slope,pvalue,tau,kpvalue,file=outall,append=TRUE,sep=",","\n")

      detach(qdata)

    }
  }
}

