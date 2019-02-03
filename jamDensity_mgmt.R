library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)
library(MuMIn)
source('~/R/projects/mapRespAndLogs/dataByBatch.R')

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
dbGetQuery(leakyDB,"SELECT * FROM Batches")
dbGetQuery(leakyDB,"SELECT * FROM DataTypes")


segData=dataByBatch(6)[,c("locationIDX","jamsPerKm","standAge")]

#get / match some categorical data
segDataCats=dataByBatch(6,meow=T)
#i'm going to use my dem-derived 'confinement' term, but I need something for land use / mgmt
segDataCats=segDataCats[segDataCats$dataTypeIDX==22,]
segDataCats=left_join(segDataCats,segData)
boxplot(segDataCats$standAge~segDataCats$value,ylim=c(0,500))
#100 yrs is the threshold

segDataCats$isAltered=segDataCats$value=="YM"


segData2=dataByBatch(5)[,c("locationIDX","jamsPerKm","standAge")]
segData2$isAltered=segData2$standAge<=100

allJamData=rbind(segDataCats[,c("locationIDX","jamsPerKm","isAltered")],segData2[,c("locationIDX","jamsPerKm","isAltered")])
boxplot(allJamData$jamsPerKm~allJamData$isAltered)

png("jamDensity_landUse.png",height=300,width=500)
par(mar=c(0,0,0,0))
layout(matrix(c(1,4,2,4,3,4),ncol=3),widths=c(0.2,1,1),heights=c(1,0.2))
plot.new()
mtext(side=2,"Observed Frequency",line=-2.5)
par(mar=c(2,2,2,2))
hist(allJamData$jamsPerKm[allJamData$isAltered==F],ylim=c(0,30),xlim=c(0,45),main="Unaltered",xlab="",ylab="")
hist(allJamData$jamsPerKm[allJamData$isAltered==T],ylim=c(0,30),xlim=c(0,45),main="Logged",xlab="",ylab="")
plot.new()
mtext(side=1,expression("Jam Density (count km"^-1),line=-1)
dev.off()