# look at all channelCount data
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
widthData=dataByBatch(1)[,c("locationIDX","channelCount")]

widthData=widthData[complete.cases(widthData),]                         
                         
widthDataCats=dataByBatch(1,meow=T)
widthDataCats=widthDataCats[widthDataCats$dataTypeIDX==8,]
names(widthDataCats)[names(widthDataCats)=="value"]="landUse"
widthData=left_join(widthData,widthDataCats[,c("locationIDX","landUse")])
allCountData=widthData


widthData=dataByBatch(3)[,c("locationIDX","channelCount")]
widthData=widthData[complete.cases(widthData),]                         

widthDataCats=dataByBatch(3,meow=T)
widthDataCats=widthDataCats[widthDataCats$dataTypeIDX==13,]
names(widthDataCats)[names(widthDataCats)=="value"]="landUse"
widthData=left_join(widthData,widthDataCats[,c("locationIDX","landUse")])
widthData[widthData$landUse=="YM",]
allCountData=rbind(widthData,allCountData)

boxplot(allCountData$channelCount~allCountData$landUse=="YM")

hist(allCountData$channelCount,breaks=seq(from=0,to=10,by=0.5),ylim=c(0,100))
par(new=T)
hist(allCountData$channelCount[allCountData$landUse=="YM"],density=20,breaks=seq(from=0,to=10,by=0.5),ylim=c(0,100),main="",xlab="",ylab="")

hist(round(allCountData$channelCount),breaks=seq(from=0,to=10,by=1),ylim=c(0,120))
par(new=T)
hist(round(allCountData$channelCount[allCountData$landUse=="YM"]),density=20,breaks=seq(from=0,to=10,by=1),ylim=c(0,120),main="",xlab="",ylab="")

plot(density(allCountData$channelCount),ylim=c(0,12))
lines(density(allCountData$channelCount[allCountData$landUse=="YM"]),col="blue")

sum(allCountData$landUse=="YM")
sum(allCountData$landUse!="YM")

sum(allCountData$channelCount[allCountData$landUse=="YM"]>1) / sum(allCountData$landUse=="YM")
sum(allCountData$channelCount[allCountData$landUse!="YM"]>1) / sum(allCountData$landUse!="YM")


sum(allCountData$channelCount[allCountData$landUse=="YM"]>1.5) / sum(allCountData$landUse=="YM")
sum(allCountData$channelCount[allCountData$landUse!="YM"]>1.5) / sum(allCountData$landUse!="YM")

max(allCountData$channelCount[allCountData$landUse=="YM"])

