require(ggplot2)
dir<-"./output/river/summary/"

scen<-c("p532cal_102413","NLDc8505HydBa","NLDc8505HydBc","NLDc8505HydBd","NLDc8505HydBe")
tempfile<-(paste(dir,"Calibration_summary_021214.csv",sep=""))

cat("CALIB","CALIBSHORT","Riverseg",
	"Tbias","Wstat","Sstat","Qstat","Bstat","Total_E","Total_LE","Mon_eff",
	"QaveRI","BaveRI","Pbias","VPbias","WBaveRI","SBaveRI","lo10bias","lo05bias",
	"SIZEINDEX","MAJBAS",file=tempfile,sep=",","\n")
 
for (i in 1:length(scen)) {
	scenario <- scen[i]
        scenshort <- substr(paste(scen[i]),9,14)
	summaryfile<-paste(dir,scenario,"_sum_stats.csv",sep="")
	data<-read.csv(summaryfile)
	attach(data)
 
	for (ii in 1:length(Tbias)) {
 
		dat <- c(Tbias[ii],Wstat[ii],Sstat[ii],Qstat[ii],Bstat[ii],Total_E[ii],Total_LE[ii],Mon_eff[ii],QaveRI[ii],BaveRI[ii],Pbias[ii],VPbias[ii],WBaveRI[ii],SBaveRI[ii],lo10bias[ii],lo05bias[ii])
		cat(scenario,scenshort,paste(SEGMENT[ii]),dat,substr(paste(SEGMENT[ii]),3,3),substr(paste(SEGMENT[ii]),1,1),
		file=tempfile,sep=",",append=TRUE,"\n")
 
         }
}

newdata<-read.csv(tempfile)
attach(newdata)

A<-matrix(c("E","Eastern Shore","J","James","X","Patuxent","P","Potomac","R","Rappahannock",
"S","Susquehanna","W","Western Shore","Y","York"),ncol=2,nrow=8,byrow=TRUE)
colnames(A) <- c("MAJBAS","BASINNAME")
A<-as.data.frame(A)

dataplot<-merge(newdata,A, by = "MAJBAS", all = TRUE)
attach(dataplot)

##############################
## BASIN NAME

graph<-paste(dir,"Tbias_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Tbias)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Wstat_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Wstat)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Sstat_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Sstat)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Qstat_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Qstat)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Bstat_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Bstat)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Total_E_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Total_E)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Total_LE_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Total_LE)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Mon_eff_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Mon_eff)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"QaveRI_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,QaveRI)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"BaveRI_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,BaveRI)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Pbias_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Pbias)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"VPbias_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,VPbias)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"WBaveRI_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,WBaveRI)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"SBaveRI_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,SBaveRI)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"lo10bias_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,lo10bias)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"lo05bias_BASINNAME.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,lo05bias)) + geom_boxplot(notch=TRUE) + facet_wrap(~ BASINNAME)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

##############################
## SIZE INDEX

graph<-paste(dir,"Tbias_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Tbias)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Wstat_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Wstat)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Sstat_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Sstat)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Qstat_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Qstat)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Bstat_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Bstat)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Total_E_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Total_E)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Total_LE_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Total_LE)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Mon_eff_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Mon_eff)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"QaveRI_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,QaveRI)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"BaveRI_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,BaveRI)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"Pbias_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,Pbias)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"VPbias_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,VPbias)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"WBaveRI_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,WBaveRI)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"SBaveRI_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,SBaveRI)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"lo10bias_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,lo10bias)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)

graph<-paste(dir,"lo05bias_SIZEINDEX.pdf",sep="")
ggplot(dataplot,aes(CALIBSHORT,lo05bias)) + geom_boxplot(notch=TRUE) + facet_wrap(~ SIZEINDEX)+ggtitle("Calibration Statistics")
ggsave(file=graph,scale=1.5)



