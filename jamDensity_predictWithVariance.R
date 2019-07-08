library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)
library(RColorBrewer)
library(tolerance)
source('~/R/projects/mapRespAndLogs/dataByBatch.R')

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
#dbGetQuery(leakyDB,"SELECT * FROM Batches")
#dbGetQuery(leakyDB,"SELECT * FROM DataTypes")

############--------build----------------

wlData=dataByBatch(3)[,c("locationIDX","jamsPerKm","med_elevation","med_latRange_10","med_latRange_25","med_minLatRange_10","med_minLatRange_25","med_slope","med_SPI","med_UAA","jamCount","channelLength","med_valleyWidth_1","med_valleyWidth_05","med_slope_25","med_channelCount")]
wbData=dataByBatch(4)[,c("locationIDX","jamsPerKm","med_elevation","med_latRange_10","med_latRange_25","med_minLatRange_10","med_minLatRange_25","med_slope","med_SPI","med_UAA","jamCount","channelLength","med_valleyWidth_1","med_valleyWidth_05","med_slope_25","med_channelCount")]
jamData=rbind(wlData,wbData)
#jamData$med_channelCount[is.na(jamData$med_channelCount)]=1

wlDataCats=dataByBatch(3,meow=T)
wbDataCats=dataByBatch(4,meow=T)

#land use / mgmt
jamDataCats=rbind(wlDataCats[wlDataCats$dataTypeIDX==13,][,c("locationIDX","value")],
                  wbDataCats[wbDataCats$dataTypeIDX==33,][,c("locationIDX","value")])
names(jamDataCats)[2]="mgmt"
jamData=left_join(jamData,jamDataCats)
jamData$isManaged=jamData$mgmt=="YM"

#confinement
jamDataCats=rbind(wlDataCats[wlDataCats$dataTypeIDX==14,][,c("locationIDX","value")],
                  wbDataCats[wbDataCats$dataTypeIDX==29,][,c("locationIDX","value")])
names(jamDataCats)[2]="confinement"
jamDataCats$confinement=toupper(jamDataCats$confinement)
jamData=left_join(jamData,jamDataCats)

jamData$isConf=jamData$confinement=="C"
jamData$isUnconf=jamData$confinement=="U"
jamData$lUAA=log(jamData$med_UAA)
jamData$channelLength=jamData$channelLength/1000


fitData_count=jamData[complete.cases(jamData),]
fitData_count$jamsPerChannelKm=fitData_count$jamsPerKm/fitData_count$med_channelCount

jamData$med_channelCount=NULL
fitData=jamData[complete.cases(jamData),]


#######---------------fit-------------
jam1=glm.nb(jamCount~lUAA+I(lUAA^2)+offset(log(channelLength)),data=fitData)
plot(jam1$fitted.values~fitData$jamCount,pch="*")
abline(a=0,b=1)

jam3=glm.nb(jamCount~isManaged + med_latRange_10 + lUAA+I(lUAA^2)+offset(log(channelLength)),data=fitData)

plot(fitData$jamsPerKm~fitData$lUAA)
points(jam3$fitted.values/fitData$channelLength~fitData$lUAA,pch="*")

plot(fitData$jamsPerKm~fitData$med_latRange_10)
points(jam3$fitted.values/fitData$channelLength~fitData$med_latRange_10,pch="*")

a=0.05
p=0.95
tol=npregtol.int(x=fitData$lUAA,y=fitData$jamsPerKm,y.hat=jam3$fitted.values/fitData$channelLength,alpha=a,P=p)
plottol(tol,x=fitData$lUAA,y=fitData$jamsPerKm,y.hat=jam1$fitted.values/fitData$channelLength)
 
library(HH)
interval(jam3,conf.level=0.95,newdata=data.frame(lUAA=3,med_latRange_10=0.1,isManaged=T,channelLength=1))


pint=data.frame(interval(jam1,type="response",newdata=data.frame(lUAA=seq(from=0.1,to=5,by=0.1),med_latRange_10=0.1,isManaged=F,channelLength=1)))
plot(pint$fit~seq(from=0.1,to=5,by=0.1),ylim=c(0.1,110),log="")
points(pint$ci.low~seq(from=0.1,to=5,by=0.1),pch=".")
points(pint$ci.hi~seq(from=0.1,to=5,by=0.1),pch=".")
points(pint$pi.low~seq(from=0.1,to=5,by=0.1),pch="*")
points(pint$pi.hi~seq(from=0.1,to=5,by=0.1),pch="*")
points(fitData$jamsPerKm~fitData$lUAA,pch=2)

#on pred-obs axes
tol=npregtol.int(x=fitData$lUAA,y=fitData$jamsPerKm,y.hat=jam3$fitted.values/fitData$channelLength,alpha=0.05,P=0.9)

plot(jam3$fitted.values/fitData$channelLength~fitData$jamsPerKm,pch="*",xlim=c(0,140),ylim=c(0,50))
#points(tol$y.hat~tol$y)
points(tol$`1-sided.lower`~tol$y)
points(tol$`1-sided.upper`~tol$y)

pint3=data.frame(interval(jam3,type="response"))
plot(jam3$fitted.values/fitData$channelLength~fitData$jamsPerKm,pch="*")
points(pint3$pi.hi/fitData$channelLength~fitData$jamsPerKm)
points(pint3$pi.low/fitData$channelLength~fitData$jamsPerKm)

plot(jam3$fitted.values/fitData$channelLength,pint3$pi.low)
