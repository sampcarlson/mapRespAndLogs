library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)
library(MuMIn)
source('~/R/projects/mapRespAndLogs/dataByBatch.R')

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
#dbGetQuery(leakyDB,"SELECT * FROM Batches")
dbGetQuery(leakyDB,"SELECT * FROM DataTypes")

#grab from segments - common denominator
widthData=dataByBatch(6)[,c("locationIDX","wettedWidth","bankfullWidth","elevation","latRange_10",
                            "latRange_25","latRange_50","slope","SPI","UAA")]
jamHabitatProb=read.csv("predictedDenseJamProbability.csv")[,c("locationIDX","denseJamProbability")]

widthData=left_join(widthData,jamHabitatProb)
widthData$locationIDX=NULL

widthData=widthData[complete.cases(widthData),]


windows()
chart.Correlation(widthData)
plot(widthData$wettedWidth~widthData$UAA)
plot(widthData$wettedWidth~widthData$denseJamProbability)
luna=nls(wettedWidth~i+a*UAA^b,data=widthData,
       start=list(i=0,a=1,b=0.4))
summary(luna)
plot(widthData$wettedWidth~predict(luna))
abline(a=0,b=1)


l=lm(widthData$wettedWidth~widthData$UAA*widthData$slope*widthData$denseJamProbability*widthData$latRange_25,na.action=na.fail)
dredge(l,extra="R^2")


#this one
widthMod=lm(log(wettedWidth)~denseJamProbability*(latRange_25+slope*UAA)+latRange_25:slope,data=widthData,na.action=na.fail)
summary(widthMod)
plot(widthData$wettedWidth~I(exp(predict(widthMod))))
abline(a=0,b=1)



widthPredict=dataByBatch(6)[,c("locationIDX","latRange_25","slope","UAA")]
widthPredict=left_join(widthPredict,jamHabitatProb)

widthPredict$width=predict(widthMod,newdata=widthPredict)
write.csv(widthPredict,"segs_width_jamHabitat.csv")

