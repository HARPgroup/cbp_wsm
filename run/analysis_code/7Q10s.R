#This code is to calculate the 7Q10s from the USGS gage data 

library(dataRetrieval)
vignette("dataRetrieval",package = "dataRetrieval")
library('IHA')
library('zoo')
library('PearsonDS')

siteNo <- "02075500"
pCode <- "00060"
start.date <- "1984-01-01"
end.date <- "2005-12-31"

yahara <- readNWISdv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)
# names with codes
names(yahara)
# cleans up names
yahara <- renameNWISColumns(yahara)
names(yahara)
head(yahara)
summary(yahara) # shows basic stats, median, mean, max, min

Flow <- yahara[,c(3,4)]
Flowv <- as.vector(Flow)

Flowv$Date <- as.POSIXct(Flowv$Date)

f3 <- zoo(Flowv$Flow, order.by = Flowv$Date)

g2 <- group2(f3)
x <- as.vector(as.matrix(g2["7 Day Min"]))
x <- log(x)
pars <- PearsonDS:::pearsonIIIfitML(x)
exp(qpearsonIII(0.1,params = pars$par))
