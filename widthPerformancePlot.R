library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)
library(MuMIn)
library(nnet)
library(MASS)
source('~/R/projects/mapRespAndLogs/dataByBatch.R')

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
dbGetQuery(leakyDB,"SELECT * FROM Batches")
dbGetQuery(leakyDB,"SELECT * FROM DataTypes")

msWidths=dataByBatch(1)[,c("locationIDX","bankfullWidth","channelCount","mean_UAA")]
wlWidths=dataByBatch(3)[,c("locationIDX","bankfullWidth","channelCount","mean_UAA")]
wbWidths=dataByBatch(4)[,c("locationIDX","bankfullWidth","mean_channelCount","mean_UAA")]

wbWidths$channelCount=wbWidths$mean_channelCount
wbWidths$mean_channelCount=NULL

allWidths=rbind(msWidths,wlWidths,wbWidths)

msDataCats=dataByBatch(1,meow=T)
wlDataCats=dataByBatch(3,meow=T)
wbDataCats=dataByBatch(4,meow=T)
cats=rbind(msDataCats,wlDataCats,wbDataCats)
cats=cats[cats$metric=="landUse",]
cats$isManaged=cats$value=="YM"
cats=cats[,c("locationIDX","isManaged")]

allWidths=left_join(allWidths,cats,by="locationIDX")
allWidths$wettedWidth=0.37644 + (0.76634*allWidths$bankfullWidth)

nsv_locations=dbGetQuery(leakyDB,"SELECT * FROM Locations WHERE watershedID = 'NSV_def'")
allWidths$isNSV=allWidths$locationIDX %in% nsv_locations$locationIDX
allWidths$channelCount[is.na(allWidths$channelCount)]=1
#allWidths$channelCount=round(allWidths$channelCount)
allWidths$wettedWidthLogged=allWidths$wettedWidth/allWidths$channelCount

unloggedCol=col2rgb("#1b9e77",alpha=T)
unloggedCol=rgb(t(unloggedCol),maxColorValue = 256)

loggedCol=col2rgb("#d95f02",alpha=T)
loggedCol=rgb(t(loggedCol),maxColorValue=256)



scalar=175
png(height=3*scalar,width=4*scalar,filename = "widthPerformance.png")
plot(allWidths$wettedWidth[allWidths$isNSV]~allWidths$mean_UAA[allWidths$isNSV],pch=17,cex=2.5,col=unloggedCol,ylim=c(0,30))
points(allWidths$wettedWidthLogged[allWidths$isNSV]~allWidths$mean_UAA[allWidths$isNSV],pch=6,cex=2.5,lwd=1.5)
points(allWidths$wettedWidth[allWidths$isManaged]~allWidths$mean_UAA[allWidths$isManaged],pch=6,lwd=2,cex=2.5,col=loggedCol)

dev.off()
