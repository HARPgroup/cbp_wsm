library(rgeos)
library(rgdal)
library(raster)

my.filepath <- 'C:\\Users\\connorb5\\Desktop\\GitHub\\cbp_wsm\\evap\\'

m<-c('01','02','03','04','05','06','07','08','09','10','11','12')
VA<-readOGR(paste(my.filepath,'EvapInputs.gdb',sep=""),layer="VA")
BB<-readOGR(paste(my.filepath,'EvapInputs.gdb',sep=""),layer="BoundingBox")
BB<-spTransform(BB,CRS="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
#crs(VA)

#--------------------------------------------------------------------------------------------------
# Alternativley The bounding box can also be creted within R
# change the code below to coordinates of interest
#library(sp)
#x_min<--74
#x_max<--85
#y_min<-35
#y_max<-42
#coords = matrix(c(x_min,y_min,x_max,y_min,x_max,y_max,x_min,y_max,x_min,y_min),ncol = 2, byrow = TRUE)
#BB = Polygon(coords)
#BB = SpatialPolygons(list(Polygons(list(BB), ID = "a")), proj4string=CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
#--------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
# View Bounding Box and Virginia as plot image 
#plot(BB,axes=T)
#plot(VA,add=T)
#--------------------------------------------------------------------------------------------------


########Mean monthly raster creation using PRISM data (pre-downloaded)########
#You MUST delete output TIFF files before writing new ones!
#Can download ECHO as to get all monthly data for 1970 max temperature (can also call tmin, ppt, tdmean, and tmean). Works for 1895-1980
yr<-as.character(seq(1955,1970))
setwd(my.filepath)
pathTMean<- paste(my.filepath,'PRISM\\TMean',sep="")
pathTMax <- paste(my.filepath,'PRISM\\TMax',sep="")
pathTMin <- paste(my.filepath,'PRISM\\TMin',sep="")
pathTDew <- paste(my.filepath,'PRISM\\TDew',sep="")
for(i in 1:length(yr)){
  temp<-tempfile()
  download.file(paste0("http://services.nacse.org/prism/data/public/4km/tmean/",yr[i]),temp,mode='wb')
  unzip(zipfile=temp,exdir=pathTMean)

  temp<-tempfile()
  download.file(paste0("http://services.nacse.org/prism/data/public/4km/tmax/",yr[i]),temp,mode='wb')
  unzip(temp,exdir=pathTMax)  
  
  temp<-tempfile()
  download.file(paste0("http://services.nacse.org/prism/data/public/4km/tmin/",yr[i]),temp,mode='wb')
  unzip(temp,exdir=pathTMin)  
  
  temp<-tempfile()
  download.file(paste0("http://services.nacse.org/prism/data/public/4km/tdmean/",yr[i]),temp,mode='wb')
  unzip(temp,exdir=pathTDew)  
}


#For average temperature
setwd(paste(my.filepath,"PRISM\\TMean",sep=""))
print(paste("Mean Temperature:",sep=""))
dir.create('Monthly Averages')
files<-list.files()
files<-files[grep('bil.bil$',files)]
yr<-as.character(seq(1955,1970))
r<-character()
for (i in 1:length(yr)){
  case<-files[grep(yr[i],files)]
  r<-c(case,r)
}
for (j in 1:length(m)){
  print(paste("Month ",j," of ",length(m),sep=""))
  case<-grep(paste0(m[j],'_bil.bil$'),r)
  rstack<-stack()
  for (i in 1:length(case)){
    print(paste("case ",i," of ",length(case),sep=""))
    test<-raster(r[case[i]])
    test<-projectRaster(test,crs=proj4string(BB))
    testclip<-mask(test,BB)
    rstack<-addLayer(rstack,testclip)
  }
  assign(paste0('MeanTemp_',m[j]),mean(rstack))
  writeRaster(mean(rstack), filename=paste0('Monthly Averages/MeanTemp',m[j],'.tiff'))
}


#For maximum temperature
setwd(paste(my.filepath,"PRISM\\TMax",sep=""))
print(paste("Maximum Temperature:",sep=""))
dir.create('Monthly Averages')
files<-list.files()
files<-files[grep('bil.bil$',files)]
yr<-as.character(seq(1955,1970))
r<-character()
for (i in 1:length(yr)){
  case<-files[grep(yr[i],files)]
  r<-c(case,r)
}
for (j in 1:length(m)){
  print(paste("Month ",j," of ",length(m),sep=""))
  case<-grep(paste0(m[j],'_bil.bil$'),r)
  rstack<-stack()
  for (i in 1:length(case)){
    print(paste("case ",i," of ",length(case),sep=""))
    test<-raster(r[case[i]])
    test<-projectRaster(test,crs=proj4string(BB))
    testclip<-mask(test,BB)
    rstack<-addLayer(rstack,testclip)
  }
  assign(paste0('MaxTemp_',m[j]),mean(rstack))
  writeRaster(mean(rstack), filename=paste0('Monthly Averages/MaxTemp',m[j],'.tiff'))
}


#For minimum temperature
setwd(paste(my.filepath,"PRISM\\TMin",sep=""))
print(paste("Minimum Temperature:",sep=""))
dir.create('Monthly Averages')
files<-list.files()
files<-files[grep('bil.bil$',files)]
yr<-as.character(seq(1955,1970))
r<-character()
for (i in 1:length(yr)){
  case<-files[grep(yr[i],files)]
  r<-c(case,r)
}
for (j in 1:length(m)){
  print(paste("Month ",j," of ",length(m),sep=""))
  case<-grep(paste0(m[j],'_bil.bil$'),r)
  rstack<-stack()
  for (i in 1:length(case)){
    print(paste("case ",i," of ",length(case),sep=""))
    test<-raster(r[case[i]])
    test<-projectRaster(test,crs=proj4string(BB))
    testclip<-mask(test,BB)
    rstack<-addLayer(rstack,testclip)
  }
  assign(paste0('MinTemp_',m[j]),mean(rstack))
  writeRaster(mean(rstack), filename=paste0('Monthly Averages/MinTemp',m[j],'.tiff'))
}

#For dew point temperature
setwd(paste(my.filepath,"PRISM\\TDew",sep=""))
print(paste("Dew Point Temperature:",sep=""))
dir.create('Monthly Averages')
files<-list.files()
files<-files[grep('bil.bil$',files)]
yr<-as.character(seq(1955,1970))
r<-character()
for (i in 1:length(yr)){
  case<-files[grep(yr[i],files)]
  r<-c(case,r)
}
for (j in 1:length(m)){
  print(paste("Month ",j," of ",length(m),sep=""))
  case<-grep(paste0(m[j],'_bil.bil$'),r)
  rstack<-stack()
  for (i in 1:length(case)){
    print(paste("case ",i," of ",length(case),sep=""))
    test<-raster(r[case[i]])
    test<-projectRaster(test,crs=proj4string(BB))
    testclip<-mask(test,BB)
    rstack<-addLayer(rstack,testclip)
  }
  assign(paste0('DewTemp_',m[j]),mean(rstack))
  writeRaster(mean(rstack), filename=paste0('Monthly Averages/DewTemp',m[j],'.tiff'))
}

########Solar radiation calculation########
WB<-readOGR(paste(my.filepath,'EvapInputs.gdb',sep=""),layer="Waterbodies")

#Project coordinates into NAD 1983 UTM zone 17N to caclulate area. Then, reproject back for consistency with other files
WB<-spTransform(WB,'+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
WB@data$FID<-1:length(WB@data$COMID)
#Not sure about these yet
WB@data$AreaRcalc<-gArea(WB,byid=T)
WB<-spTransform(WB,CRS="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
WBCenter<-gCentroid(WB,byid=T,id=WB@data$FID)
#See http://www.fao.org/docrep/x0490e/x0490e07.htm#TopOfPage
diy<-seq(1,365)
distFsun<-1+0.033*cos(2*pi*diy/365)
sigma<-0.409*sin(2*pi*diy/365-1.39)
for (i in 1:length(WB@data$FID)){
  print(paste("Mean extraterrestrial radiation calc for WB ",i," of ",length(WB@data$FID),sep=""))
  lat<-as.numeric(WBCenter@coords[i,2])*pi/180
  sunsethour<-acos(-tan(sigma)*tan(lat))
  Ra<-(25*60*0.0820/pi)*distFsun*(sunsethour*sin(lat)*sin(sigma)+cos(lat)*cos(sigma)*sin(sunsethour))
  WB@data$JanRa[i]<-mean(Ra[1:31])
  WB@data$FebRa[i]<-mean(Ra[32:59])
  WB@data$MarRa[i]<-mean(Ra[60:90])
  WB@data$AprRa[i]<-mean(Ra[91:120])
  WB@data$MayRa[i]<-mean(Ra[121:151])
  WB@data$JunRa[i]<-mean(Ra[152:181])
  WB@data$JulRa[i]<-mean(Ra[182:212])
  WB@data$AugRa[i]<-mean(Ra[213:243])
  WB@data$SepRa[i]<-mean(Ra[244:273])
  WB@data$OctRa[i]<-mean(Ra[274:304])
  WB@data$NovRa[i]<-mean(Ra[305:334])
  WB@data$DecRa[i]<-mean(Ra[335:365])
}
########Evaporation Estimations########
print(paste("WB Evap Estimations",sep=""))
#Want to make sure the extracted values line up with correct waterbodies; test in GIS
WB@data$JanMaxT<-extract(MaxTemp_01,WBCenter)
WB@data$JanMinT<-extract(MinTemp_01,WBCenter)
WB@data$JanDiffT<-WB@data$JanMaxT-WB@data$JanMinT
WB@data$JanMeanT<-extract(MeanTemp_01,WBCenter)
WB@data$JanDewT<-extract(DewTemp_01,WBCenter)
WB@data$FebMaxT<-extract(MaxTemp_02,WBCenter)
WB@data$FebMinT<-extract(MinTemp_02,WBCenter)
WB@data$FebDiffT<-WB@data$FebMaxT-WB@data$FebMinT
WB@data$FebMeanT<-extract(MeanTemp_02,WBCenter)
WB@data$FebDewT<-extract(DewTemp_02,WBCenter)
WB@data$MarMaxT<-extract(MaxTemp_03,WBCenter)
WB@data$MarMinT<-extract(MinTemp_03,WBCenter)
WB@data$MarDiffT<-WB@data$MarMaxT-WB@data$MarMinT
WB@data$MarMeanT<-extract(MeanTemp_03,WBCenter)
WB@data$MarDewT<-extract(DewTemp_03,WBCenter)
WB@data$AprMaxT<-extract(MaxTemp_04,WBCenter)
WB@data$AprMinT<-extract(MinTemp_04,WBCenter)
WB@data$AprDiffT<-WB@data$AprMaxT-WB@data$AprMinT
WB@data$AprMeanT<-extract(MeanTemp_04,WBCenter)
WB@data$AprDewT<-extract(DewTemp_04,WBCenter)
WB@data$MayMaxT<-extract(MaxTemp_05,WBCenter)
WB@data$MayMinT<-extract(MinTemp_05,WBCenter)
WB@data$MayDiffT<-WB@data$MayMaxT-WB@data$MayMinT
WB@data$MayMeanT<-extract(MeanTemp_05,WBCenter)
WB@data$MayDewT<-extract(DewTemp_05,WBCenter)
WB@data$JunMaxT<-extract(MaxTemp_06,WBCenter)
WB@data$JunMinT<-extract(MinTemp_06,WBCenter)
WB@data$JunDiffT<-WB@data$JunMaxT-WB@data$JunMinT
WB@data$JunMeanT<-extract(MeanTemp_06,WBCenter)
WB@data$JunDewT<-extract(DewTemp_06,WBCenter)
WB@data$JulMaxT<-extract(MaxTemp_07,WBCenter)
WB@data$JulMinT<-extract(MinTemp_07,WBCenter)
WB@data$JulDiffT<-WB@data$JulMaxT-WB@data$JulMinT
WB@data$JulMeanT<-extract(MeanTemp_07,WBCenter)
WB@data$JulDewT<-extract(DewTemp_07,WBCenter)
WB@data$AugMaxT<-extract(MaxTemp_08,WBCenter)
WB@data$AugMinT<-extract(MinTemp_08,WBCenter)
WB@data$AugDiffT<-WB@data$AugMaxT-WB@data$AugMinT
WB@data$AugMeanT<-extract(MeanTemp_08,WBCenter)
WB@data$AugDewT<-extract(DewTemp_08,WBCenter)
WB@data$SepMaxT<-extract(MaxTemp_09,WBCenter)
WB@data$SepMinT<-extract(MinTemp_09,WBCenter)
WB@data$SepDiffT<-WB@data$SepMaxT-WB@data$SepMinT
WB@data$SepMeanT<-extract(MeanTemp_09,WBCenter)
WB@data$SepDewT<-extract(DewTemp_09,WBCenter)
WB@data$OctMaxT<-extract(MaxTemp_10,WBCenter)
WB@data$OctMinT<-extract(MinTemp_10,WBCenter)
WB@data$OctDiffT<-WB@data$OctMaxT-WB@data$OctMinT
WB@data$OctMeanT<-extract(MeanTemp_10,WBCenter)
WB@data$OctDewT<-extract(DewTemp_10,WBCenter)
WB@data$NovMaxT<-extract(MaxTemp_11,WBCenter)
WB@data$NovMinT<-extract(MinTemp_11,WBCenter)
WB@data$NovDiffT<-WB@data$NovMaxT-WB@data$NovMinT
WB@data$NovMeanT<-extract(MeanTemp_11,WBCenter)
WB@data$NovDewT<-extract(DewTemp_11,WBCenter)
WB@data$DecMaxT<-extract(MaxTemp_12,WBCenter)
WB@data$DecMinT<-extract(MinTemp_12,WBCenter)
WB@data$DecDiffT<-WB@data$DecMaxT-WB@data$DecMinT
WB@data$DecMeanT<-extract(MeanTemp_12,WBCenter)
WB@data$DecDewT<-extract(DewTemp_12,WBCenter)
#The below line may summarize by polygon, but it takes a very long time to run (~10 min for each extraction)
#extract(MinTemp_12,WB,fun=mean,border=F)

#Hargreaves equation may take the form 0.0023Ra*TD^0.5*(T+17.8). First coefficient may also be ~0.0135
#If Ra is in Mj/m^2/day, this can be converted to mm/day by dividing by latent heat and assuming density of 1000kg/m^3
#0.0023Ra*TD^0.5*(T+17.8)/lambda
#Output is in mm/day
print("Hargreaves equation used to estimate monthly evaporation at each waterbody:")
c1<-0.0023
c2<-17.8
c1*WB@data$JanRa[1]*sqrt(WB@data$JanDiffT[1])*(WB@data$JanMeanT[1]+c2)/(2.501-.002361*WB@data$JanMeanT[1])
WB@data$JanHarg<-c1*WB@data$JanRa*sqrt(WB@data$JanDiffT)*(WB@data$JanMeanT+c2)/(2.501-.002361*WB@data$JanMeanT)
WB@data$FebHarg<-c1*WB@data$FebRa*sqrt(WB@data$FebDiffT)*(WB@data$FebMeanT+c2)/(2.501-.002361*WB@data$FebMeanT)
WB@data$MarHarg<-c1*WB@data$MarRa*sqrt(WB@data$MarDiffT)*(WB@data$MarMeanT+c2)/(2.501-.002361*WB@data$MarMeanT)
WB@data$AprHarg<-c1*WB@data$AprRa*sqrt(WB@data$AprDiffT)*(WB@data$AprMeanT+c2)/(2.501-.002361*WB@data$AprMeanT)
WB@data$MayHarg<-c1*WB@data$MayRa*sqrt(WB@data$MayDiffT)*(WB@data$MayMeanT+c2)/(2.501-.002361*WB@data$MayMeanT)
WB@data$JunHarg<-c1*WB@data$JunRa*sqrt(WB@data$JunDiffT)*(WB@data$JunMeanT+c2)/(2.501-.002361*WB@data$JunMeanT)
WB@data$JulHarg<-c1*WB@data$JulRa*sqrt(WB@data$JulDiffT)*(WB@data$JulMeanT+c2)/(2.501-.002361*WB@data$JulMeanT)
WB@data$AugHarg<-c1*WB@data$AugRa*sqrt(WB@data$AugDiffT)*(WB@data$AugMeanT+c2)/(2.501-.002361*WB@data$AugMeanT)
WB@data$SepHarg<-c1*WB@data$SepRa*sqrt(WB@data$SepDiffT)*(WB@data$SepMeanT+c2)/(2.501-.002361*WB@data$SepMeanT)
WB@data$OctHarg<-c1*WB@data$OctRa*sqrt(WB@data$OctDiffT)*(WB@data$OctMeanT+c2)/(2.501-.002361*WB@data$OctMeanT)
WB@data$NovHarg<-c1*WB@data$NovRa*sqrt(WB@data$NovDiffT)*(WB@data$NovMeanT+c2)/(2.501-.002361*WB@data$NovMeanT)
WB@data$DecHarg<-c1*WB@data$DecRa*sqrt(WB@data$DecDiffT)*(WB@data$DecMeanT+c2)/(2.501-.002361*WB@data$DecMeanT)

#Calculate average monthly daylight hours for use in the Thornthwaite Method
print("Calculate average monthly daylight hours for use in the Thornthwaite Method")
for (i in 1:length(WB@data$FID)){
  print(paste("Avg mo daylight hrs calc for WB ",i," of ",length(WB@data$FID),sep=""))
  lat<-as.numeric(WBCenter@coords[i,2])*pi/180
  sunsethour<-acos(-tan(sigma)*tan(lat))
  Ld<-24*sunsethour/pi
  Ra<-(25*60*0.0820/pi)*distFsun*(sunsethour*sin(lat)*sin(sigma)+cos(lat)*cos(sigma)*sin(sunsethour))
  WB@data$JanLd[i]<-mean(Ld[1:31])
  WB@data$FebLd[i]<-mean(Ld[32:59])
  WB@data$MarLd[i]<-mean(Ld[60:90])
  WB@data$AprLd[i]<-mean(Ld[91:120])
  WB@data$MayLd[i]<-mean(Ld[121:151])
  WB@data$JunLd[i]<-mean(Ld[152:181])
  WB@data$JulLd[i]<-mean(Ld[182:212])
  WB@data$AugLd[i]<-mean(Ld[213:243])
  WB@data$SepLd[i]<-mean(Ld[244:273])
  WB@data$OctLd[i]<-mean(Ld[274:304])
  WB@data$NovLd[i]<-mean(Ld[305:334])
  WB@data$DecLd[i]<-mean(Ld[335:365])
}

ThornI<-function(meanT){
  if(meanT<0){
    out<-0
  }else{
    out<-(meanT/5)^1.514
  }
  return(out)
}

#Calculate Thornthwaite temperature index and coefficient
for (i in 1:length(WB@data$FID)){
  print(paste("Thornthwaite temperature index calc for WB ",i," of ",length(WB@data$FID),sep=""))
  Jan<-ThornI(WB@data$JanMeanT[i])
  Feb<-ThornI(WB@data$FebMeanT[i])
  Mar<-ThornI(WB@data$MarMeanT[i])
  Apr<-ThornI(WB@data$AprMeanT[i])
  May<-ThornI(WB@data$MayMeanT[i])
  Jun<-ThornI(WB@data$JunMeanT[i])
  Jul<-ThornI(WB@data$JulMeanT[i])
  Aug<-ThornI(WB@data$AugMeanT[i])
  Sep<-ThornI(WB@data$SepMeanT[i])
  Oct<-ThornI(WB@data$OctMeanT[i])
  Nov<-ThornI(WB@data$NovMeanT[i])
  Dec<-ThornI(WB@data$DecMeanT[i])
  WB@data$I[i]<-Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec
}
WB@data$a<-(0.000000675*WB@data$I^3)-(0.0000771*WB@data$I^2)+(0.01791*WB@data$I)+0.49239

#Calculate and set NAs to zero, as temperatures below 0 have 0 ET
c<-16
WB@data$JanThorn<-(c*WB@data$JanLd*(10*WB@data$JanMeanT/WB@data$I)^WB@data$a)/31/12
WB@data$JanThorn[is.na(WB@data$JanThorn)]<-0
WB@data$FebThorn<-(c*WB@data$FebLd*(10*WB@data$FebMeanT/WB@data$I)^WB@data$a)/28/12
WB@data$FebThorn[is.na(WB@data$FebThorn)]<-0
WB@data$MarThorn<-(c*WB@data$MarLd*(10*WB@data$MarMeanT/WB@data$I)^WB@data$a)/31/12
WB@data$MarThorn[is.na(WB@data$MarThorn)]<-0
WB@data$AprThorn<-(c*WB@data$AprLd*(10*WB@data$AprMeanT/WB@data$I)^WB@data$a)/30/12
WB@data$AprThorn[is.na(WB@data$AprThorn)]<-0
WB@data$MayThorn<-(c*WB@data$MayLd*(10*WB@data$MayMeanT/WB@data$I)^WB@data$a)/31/12
WB@data$MayThorn[is.na(WB@data$MayThorn)]<-0
WB@data$JunThorn<-(c*WB@data$JunLd*(10*WB@data$JunMeanT/WB@data$I)^WB@data$a)/30/12
WB@data$JunThorn[is.na(WB@data$JunThorn)]<-0
WB@data$JulThorn<-(c*WB@data$JulLd*(10*WB@data$JulMeanT/WB@data$I)^WB@data$a)/31/12
WB@data$JulThorn[is.na(WB@data$JulThorn)]<-0
WB@data$AugThorn<-(c*WB@data$AugLd*(10*WB@data$AugMeanT/WB@data$I)^WB@data$a)/31/12
WB@data$AugThorn[is.na(WB@data$AugThorn)]<-0
WB@data$SepThorn<-(c*WB@data$SepLd*(10*WB@data$SepMeanT/WB@data$I)^WB@data$a)/30/12
WB@data$SepThorn[is.na(WB@data$SepThorn)]<-0
WB@data$OctThorn<-(c*WB@data$OctLd*(10*WB@data$OctMeanT/WB@data$I)^WB@data$a)/31/12
WB@data$OctThorn[is.na(WB@data$OctThorn)]<-0
WB@data$NovThorn<-(c*WB@data$NovLd*(10*WB@data$NovMeanT/WB@data$I)^WB@data$a)/30/12
WB@data$NovThorn[is.na(WB@data$NovThorn)]<-0
WB@data$DecThorn<-(c*WB@data$DecLd*(10*WB@data$DecMeanT/WB@data$I)^WB@data$a)/31/12
WB@data$DecThorn[is.na(WB@data$DecThorn)]<-0

#Hamon equation may take the form 0.55*25.4*Ld^2*(SVD/100). Need to find saturated vapor density SVD
#Can use the metholody suggested by the Corps of Engineers in Harwell, 2012 and the FAO report
#Output is in mm/day
print("Hamon equation used to estimate monthly evaporation at each waterbody:")

#Function to calculate saturated vapor pressure in kPa from the FAO report
es<-function(temperature){
  SVP<-0.6108*exp((17.27*temperature)/(temperature+237.3))
  return(SVP)
}
#Need to find average saturated vapor pressure in kPa for each month at each waterbody
WB@data$JanEs<-0.5*(es(WB@data$JanMinT)+es(WB@data$JanMaxT))
WB@data$FebEs<-0.5*(es(WB@data$FebMinT)+es(WB@data$FebMaxT))
WB@data$MarEs<-0.5*(es(WB@data$MarMinT)+es(WB@data$MarMaxT))
WB@data$AprEs<-0.5*(es(WB@data$AprMinT)+es(WB@data$AprMaxT))
WB@data$MayEs<-0.5*(es(WB@data$MayMinT)+es(WB@data$MayMaxT))
WB@data$JunEs<-0.5*(es(WB@data$JunMinT)+es(WB@data$JunMaxT))
WB@data$JulEs<-0.5*(es(WB@data$JulMinT)+es(WB@data$JulMaxT))
WB@data$AugEs<-0.5*(es(WB@data$AugMinT)+es(WB@data$AugMaxT))
WB@data$SepEs<-0.5*(es(WB@data$SepMinT)+es(WB@data$SepMaxT))
WB@data$OctEs<-0.5*(es(WB@data$OctMinT)+es(WB@data$OctMaxT))
WB@data$NovEs<-0.5*(es(WB@data$NovMinT)+es(WB@data$NovMaxT))
WB@data$DecEs<-0.5*(es(WB@data$DecMinT)+es(WB@data$DecMaxT))

#By assuming the ideal gas law applies as in Harwell, 2012 we can find SVD in g/m^3
WB@data$JanSVD<-2166.74*(WB@data$JanEs/(273.15+WB@data$JanMeanT))
WB@data$FebSVD<-2166.74*(WB@data$FebEs/(273.15+WB@data$FebMeanT))
WB@data$MarSVD<-2166.74*(WB@data$MarEs/(273.15+WB@data$MarMeanT))
WB@data$AprSVD<-2166.74*(WB@data$AprEs/(273.15+WB@data$AprMeanT))
WB@data$MaySVD<-2166.74*(WB@data$MayEs/(273.15+WB@data$MayMeanT))
WB@data$JunSVD<-2166.74*(WB@data$JunEs/(273.15+WB@data$JunMeanT))
WB@data$JulSVD<-2166.74*(WB@data$JulEs/(273.15+WB@data$JulMeanT))
WB@data$AugSVD<-2166.74*(WB@data$AugEs/(273.15+WB@data$AugMeanT))
WB@data$SepSVD<-2166.74*(WB@data$SepEs/(273.15+WB@data$SepMeanT))
WB@data$OctSVD<-2166.74*(WB@data$OctEs/(273.15+WB@data$OctMeanT))
WB@data$NovSVD<-2166.74*(WB@data$NovEs/(273.15+WB@data$NovMeanT))
WB@data$DecSVD<-2166.74*(WB@data$DecEs/(273.15+WB@data$DecMeanT))

#Then, we can solve for the Hamon Evapotranspiration
25.4*0.55*(WB@data$JanLd[1]/12)^2*(WB@data$JanSVD[1]/100)
WB@data$JanHam<-25.4*0.55*(WB@data$JanLd/12)^2*(WB@data$JanSVD/100)
WB@data$FebHam<-25.4*0.55*(WB@data$FebLd/12)^2*(WB@data$FebSVD/100)
WB@data$MarHam<-25.4*0.55*(WB@data$MarLd/12)^2*(WB@data$MarSVD/100)
WB@data$AprHam<-25.4*0.55*(WB@data$AprLd/12)^2*(WB@data$AprSVD/100)
WB@data$MayHam<-25.4*0.55*(WB@data$MayLd/12)^2*(WB@data$MaySVD/100)
WB@data$JunHam<-25.4*0.55*(WB@data$JunLd/12)^2*(WB@data$JunSVD/100)
WB@data$JulHam<-25.4*0.55*(WB@data$JulLd/12)^2*(WB@data$JulSVD/100)
WB@data$AugHam<-25.4*0.55*(WB@data$AugLd/12)^2*(WB@data$AugSVD/100)
WB@data$SepHam<-25.4*0.55*(WB@data$SepLd/12)^2*(WB@data$SepSVD/100)
WB@data$OctHam<-25.4*0.55*(WB@data$OctLd/12)^2*(WB@data$OctSVD/100)
WB@data$NovHam<-25.4*0.55*(WB@data$NovLd/12)^2*(WB@data$NovSVD/100)
WB@data$DecHam<-25.4*0.55*(WB@data$DecLd/12)^2*(WB@data$DecSVD/100)

#Turc equation may take the form 0.013*(Rs+50)*T/(T+15). If below 50% RH, 0.013*(Rs+50)*T/(T+15)*(1+(50-RH)/70)
#Need to find relative humidity (%) and daily solar radiation (in cal/cm^2/day)
#Can use the metholody suggested by the FAO report
#Output is in mm/day
print("Turc equation used to estimate monthly evaporation at each waterbody:")
#Find average relative humidity at each waterbody
WB@data$JanRH<-100*(es(WB@data$JanDewT)/WB@data$JanEs)
WB@data$FebRH<-100*(es(WB@data$FebDewT)/WB@data$FebEs)
WB@data$MarRH<-100*(es(WB@data$MarDewT)/WB@data$MarEs)
WB@data$AprRH<-100*(es(WB@data$AprDewT)/WB@data$AprEs)
WB@data$MayRH<-100*(es(WB@data$MayDewT)/WB@data$MayEs)
WB@data$JunRH<-100*(es(WB@data$JunDewT)/WB@data$JunEs)
WB@data$JulRH<-100*(es(WB@data$JulDewT)/WB@data$JulEs)
WB@data$AugRH<-100*(es(WB@data$AugDewT)/WB@data$AugEs)
WB@data$SepRH<-100*(es(WB@data$SepDewT)/WB@data$SepEs)
WB@data$OctRH<-100*(es(WB@data$OctDewT)/WB@data$OctEs)
WB@data$NovRH<-100*(es(WB@data$NovDewT)/WB@data$NovEs)
WB@data$DecRH<-100*(es(WB@data$DecDewT)/WB@data$DecEs)


#Multiply by area, convert to m^3/day then convert to million gallons per year
evapH<-numeric()
evapT<-numeric()
evapHa<-numeric()
for (i in 1:length(WB@data$FID)){
  print(paste("Evap BGY estimation for WB ",i," of ",length(WB@data$FID),sep=""))
  evapH[i]<-sum(WB@data[i,grep('Harg',colnames(WB@data))]*c(31,28,31,30,31,30,31,31,30,31,30,31))
  evapT[i]<-sum(WB@data[i,grep('Thorn',colnames(WB@data))]*c(31,28,31,30,31,30,31,31,30,31,30,31))
  evapHa[i]<-sum(WB@data[i,grep('Ham',colnames(WB@data))]*c(31,28,31,30,31,30,31,31,30,31,30,31))
}
WB@data$AnnualHrg_mmpyr<-evapH
WB@data$AnnualHrg_BGY<-WB@data$AreaRcalc*(evapH/1000)*264.1721/1000000000
WB@data$AnnualThrn_mmpyr<-evapT
WB@data$AnnualThrn_BGY<-WB@data$AreaRcalc*(evapT/1000)*264.1721/1000000000
WB@data$AnnualHamn_mmpyr<-evapHa
WB@data$AnnualHamn_BGY<-WB@data$AreaRcalc*(evapHa/1000)*264.1721/1000000000

sum(WB@data$AnnualHrg_BGY);sum(WB@data$AnnualThrn_BGY);sum(WB@data$AnnualHamn_BGY)
#===================================================================================
# PLOTS
#===================================================================================
# MEAN TEMP
# plot(crop(MeanTemp_08,BB),main= expression('August Mean Temperature (1955-1970) ('*degree*'C)'),axes=T,xlab='Longitude',ylab='Latitude',cex.lab=2,cex.main=2,cex.axis=2)
# plot(VA,add=T,lwd=2)
# 
# # SOLAR RADIATION 
# par(mar=c(5,6,2,4))
# plot(Ra,type='l',lwd=2,ylab='Mean Radiation (MJ/m^2/day)',xlab='Month',cex.lab=2,cex.axis=2)
# 
# # DAYLIGHT HOURS
# par(mar=c(5,6,2,4))
# plot(Ld,type='l',lwd=2,ylab='Mean Daylight Hours (hours)',xlab='Month',cex.lab=2,cex.axis=2)
# 
# # THORNTHWAITE PET
# ET<-c(mean(WB@data$JanThorn),
#       mean(WB@data$FebThorn),
#       mean(WB@data$MarThorn),
#       mean(WB@data$AprThorn),
#       mean(WB@data$MayThorn),
#       mean(WB@data$JunThorn),
#       mean(WB@data$JulThorn),
#       mean(WB@data$AugThorn),
#       mean(WB@data$SepThorn),
#       mean(WB@data$OctThorn),
#       mean(WB@data$NovThorn),
#       mean(WB@data$DecThorn))
# par(mar=c(5,6,2,4))
# plot(ET,type='l',lwd=2,ylab='Mean Thornthwaite PET (mm/day)',xlab='Month',cex.lab=2,cex.axis=2)
# 
# # HARGREAVES AND THORNTHWAITE
# ET<-c(mean(WB@data$JanHarg),mean(WB@data$FebHarg),mean(WB@data$MarHarg),mean(WB@data$AprHarg),mean(WB@data$MayHarg),mean(WB@data$JunHarg),mean(WB@data$JulHarg),mean(WB@data$AugHarg),mean(WB@data$SepHarg),mean(WB@data$OctHarg),mean(WB@data$NovHarg),mean(WB@data$DecHarg))
# ET2<-c(mean(WB@data$JanThorn),mean(WB@data$FebThorn),mean(WB@data$MarThorn),mean(WB@data$AprThorn),mean(WB@data$MayThorn),mean(WB@data$JunThorn),mean(WB@data$JulThorn),mean(WB@data$AugThorn),mean(WB@data$SepThorn),mean(WB@data$OctThorn),mean(WB@data$NovThorn),mean(WB@data$DecThorn))
# par(mar=c(5,6,2,4))
# plot(ET,type='l',lwd=2,ylab='Mean PET (mm/day)',xlab='Month',cex.lab=2,cex.axis=2,col='red')
# lines(ET2,col='blue',lwd=2)
# legend(x=1,y=6,bty='n',col=c('red','blue'),legend=c("Hargreaves","Thornthwaite"),lwd=2,cex=2,y.intersp = 0.75)

#===================================================================================
