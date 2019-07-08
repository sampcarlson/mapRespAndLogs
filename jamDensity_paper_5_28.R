library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)
library(RColorBrewer)
library(xtable)
source('~/R/projects/mapRespAndLogs/dataByBatch.R')

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
#dbGetQuery(leakyDB,"SELECT * FROM Batches")
#dbGetQuery(leakyDB,"SELECT * FROM DataTypes")

############--------build----------------

wlData=dataByBatch(3)[,c("locationIDX","jamsPerKm","mean_elevation","mean_latRange_10","mean_latRange_25","mean_minLatRange_10","mean_minLatRange_25","mean_slope","mean_SPI","mean_UAA","jamCount","channelLength","mean_valleyWidth_1","mean_valleyWidth_05","mean_slope_25","mean_channelCount")]
wbData=dataByBatch(4)[,c("locationIDX","jamsPerKm","mean_elevation","mean_latRange_10","mean_latRange_25","mean_minLatRange_10","mean_minLatRange_25","mean_slope","mean_SPI","mean_UAA","jamCount","channelLength","mean_valleyWidth_1","mean_valleyWidth_05","mean_slope_25","mean_channelCount")]
jamData=rbind(wlData,wbData)
#jamData$mean_channelCount[is.na(jamData$mean_channelCount)]=1

wlDataCats=dataByBatch(3,meow=T)
wbDataCats=dataByBatch(4,meow=T)

#land use / mgmt
jamDataCats=rbind(wlDataCats[wlDataCats$dataTypeIDX==13,][,c("locationIDX","value")],
                  wbDataCats[wbDataCats$dataTypeIDX==32,][,c("locationIDX","value")])
names(jamDataCats)[2]="mgmt"
jamData=left_join(jamData,jamDataCats)
jamData$isManaged=jamData$mgmt=="YM"

#confinement
jamDataCats=rbind(wlDataCats[wlDataCats$dataTypeIDX==14,][,c("locationIDX","value")],
                  wbDataCats[wbDataCats$dataTypeIDX==28,][,c("locationIDX","value")])
names(jamDataCats)[2]="confinement"
jamDataCats$confinement=toupper(jamDataCats$confinement)
jamData=left_join(jamData,jamDataCats)

jamData$isConf=jamData$confinement=="C"
jamData$isUnconf=jamData$confinement=="U"
jamData$lUAA=log(jamData$mean_UAA)
jamData$channelLength=jamData$channelLength/1000


fitData_count=jamData[complete.cases(jamData),]
fitData_count$jamsPerChannelKm=fitData_count$jamsPerKm/fitData_count$mean_channelCount



#drop channel count
fitData=jamData[,c("locationIDX","jamCount","channelLength","jamsPerKm","mean_elevation","mean_latRange_10","mean_latRange_25","mean_slope","lUAA","isManaged","isUnconf")]
fitData=fitData[complete.cases(fitData),]



fitData$isManaged=as.numeric(fitData$isManaged)
fitData$isUnconf=as.numeric(fitData$isUnconf)

#w/ allowed interactions
jamGlm=glm.nb(jamCount~isManaged + lUAA+I(lUAA^2) + isUnconf + mean_elevation + mean_slope  +
                (lUAA+I(lUAA^2)):isUnconf +
                (lUAA+I(lUAA^2)):mean_elevation +
                (lUAA+I(lUAA^2)):mean_slope +
                isUnconf:mean_elevation + 
                isUnconf:mean_slope + 
                mean_elevation:mean_slope +
                isManaged:isUnconf +
                offset(log(channelLength)),data=fitData,na.action=na.fail)
summary(jamGlm)

d=dredge(jamGlm,extra=c("R^2"),subset=c(dc(I(lUAA^2),lUAA),dc(lUAA,I(lUAA^2)),
                                        dc(`I(lUAA^2):mean_slope`,`lUAA:mean_slope`),dc(`lUAA:mean_slope`,`I(lUAA^2):mean_slope`),
                                        dc(`lUAA:mean_elevation`,`I(lUAA^2):mean_elevation`),dc(`I(lUAA^2):mean_elevation`,`lUAA:mean_elevation`),
                                        dc(`lUAA:isUnconf`,`I(lUAA^2):isUnconf`),dc(`I(lUAA^2):isUnconf`,`lUAA:isUnconf`)))


displayTable=d[1:10,c(1,2,4,5,6,7,3,12,15,13,16,9,14,17,10,11,8,19,20,23)]
digits=c(0,1,2,1,2,3,2,1,4,4,2,2,1,1,5,3,2,2,2,0,2)
row.names(displayTable)=c(1:10)
xtable(displayTable,digits=digits)

m1=get.models(d,subset=1)[[1]]
m2=get.models(d,subset=2)[[1]]
m3=get.models(d,subset=3)[[1]]


scalar=240
png(height=3*scalar,width=5*scalar,filename = "fitEffects.png")
layout(mat=matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,10,11,12),ncol=5),widths=c(3,3,3,2,2))
par(oma=c(4,5,1,1))
par(mar=c(2,2,1,1))
par(cex.axis=1.5)

# plot(predict(m1,type="response")~fitData$jamCount,ylab="Predicted Jam Count",xlab="Observed Jam Count")
# abline(a=0,b=1)
# mtext(side=2,"Predicted Jam Count",line=3)
# mtext(side=2,"Model 1",line=4.5,font=2)
# 
# plot(predict(m2,type="response")~fitData$jamCount,ylab="Predicted Jam Count",xlab="Observed Jam Count")
# abline(a=0,b=1)
# mtext(side=2,"Predicted Jam Count",line=3)
# mtext(side=2,"Model 2",line=4.5,font=2)
# 
# plot(predict(m3,type="response")~fitData$jamCount,ylab="Predicted Jam Count",xlab="Observed Jam Count")
# abline(a=0,b=1)
# mtext(side=2,"Predicted Jam Count",line=3)
# mtext(side=2,"Model 3",line=4.5,font=2)
# 
# mtext(side=1,"Observed Jam Count",line=3)

plot(predict(m1,type="response")~fitData$mean_elevation,ylab="Predicted Jam Count",xlab="Stream Elevation",cex=3)
points(fitData$jamCount~fitData$mean_elevation,pch=20,cex=1.5)
mtext(side=2,"Predicted Jam Count",line=3)
mtext(side=2,"Model 1",line=4.5,font=2)



plot(predict(m2,type="response")~fitData$mean_elevation,ylab="Predicted Jam Count",xlab="Stream Elevation",cex=1.5)
points(fitData$jamCount~fitData$mean_elevation,pch=20)
mtext(side=2,"Predicted Jam Count",line=3)
mtext(side=2,"Model 2",line=4.5,font=2)



plot(predict(m3,type="response")~fitData$mean_elevation,ylab="Predicted Jam Count",xlab="Stream Elevation")
points(fitData$jamCount~fitData$mean_elevation,pch=20)
mtext(side=2,"Predicted Jam Count",line=3)
mtext(side=2,"Model 3",line=4.5,font=2)

mtext(side=1,"Stream Elevation",line=3)


plot(predict(m1,type="response")~fitData$mean_slope,ylab="Predicted Jam Count",xlab="Stream Gradient")
plot(predict(m2,type="response")~fitData$mean_slope,ylab="Predicted Jam Count",xlab="Stream Gradient")
plot(predict(m3,type="response")~fitData$mean_slope,ylab="Predicted Jam Count",xlab="Stream Gradient")
mtext(side=1,"Stream Gradient",line=3)


plot(predict(m1,type="response")~exp(fitData$lUAA),log="x",ylab="Predicted Jam Count",xlab="Stream UAA")
plot(predict(m2,type="response")~exp(fitData$lUAA),log="x",ylab="Predicted Jam Count",xlab="Stream UAA")
plot(predict(m3,type="response")~exp(fitData$lUAA),log="x",ylab="Predicted Jam Count",xlab="Stream UAA")
mtext(side=1,"Stream UAA",line=3)

boxplot(predict(m1,type="response")~fitData$isUnconf,ylab="Predicted Jam Count",names=c("Logged","Un-logged"))
boxplot(predict(m2,type="response")~fitData$isUnconf,ylab="Predicted Jam Count",names=c("Logged","Un-logged"))
boxplot(predict(m3,type="response")~fitData$isUnconf,ylab="Predicted Jam Count",names=c("Logged","Un-logged"))
mtext(side=1,"Management History",line=3)

dev.off()

######jam density prediction performance plot------------
nsv_locations=dbGetQuery(leakyDB,"SELECT * FROM Locations WHERE watershedID = 'NSV_def'")
fitData$isNSV=fitData$locationIDX %in% nsv_locations$locationIDX

#only 19 logged points?
scalar=200
png(height=3*scalar,width=5*scalar,filename = "jamPerformance.png")
plot(fitData$jamsPerKm[fitData$isNSV]~exp(fitData$lUAA[fitData$isNSV]),log="x",cex=1.5,xlim=c(4,80))
points(fitData$jamsPerKm[fitData$isNSV]/2~exp(fitData$lUAA[fitData$isNSV]),pch=16)
points(fitData$jamsPerKm[fitData$isManaged==1]~exp(fitData$lUAA[fitData$isManaged==1]),pch="*")

dev.off()

