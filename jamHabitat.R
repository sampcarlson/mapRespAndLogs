library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)
library(MuMIn)
source('~/R/projects/mapRespAndLogs/dataByBatch.R')

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
dbGetQuery(leakyDB,"SELECT * FROM Batches")
dbGetQuery(leakyDB,"SELECT * FROM DataTypes")


segData=dataByBatch(6)[,c("locationIDX","jamsPerKm","elevation","latRange_25","latRange_50","slope","SPI","UAA")]

#get / match some categorical data
segDataCats=dataByBatch(6,meow=T)
#i'm going to use my dem-derived 'confinement' term, but I need something for land use / mgmt
segDataCats=segDataCats[segDataCats$dataTypeIDX==27,]
segDataCats=left_join(segDataCats,dataByBatch(6)[,c("locationIDX","standAge")])
boxplot(segDataCats$standAge~segDataCats$value,ylim=c(0,500))
#use 100 year stand age as cutoff for old/young
#all nsv is unmanage


segData$jamsHere=F
segData$jamsHere[segData$jamsPerKm>5]=T

#dropping this will keep all seg data, keeping it will drop unsurveyed areas
#segData$jamsPerKm=NULL

fitData=segData[complete.cases(segData),]


boxplot(fitData$UAA~fitData$jamsHere)
boxplot(fitData$slope~fitData$jamsHere)
boxplot(fitData$SPI~fitData$jamsHere)


#boxplot(fitData$elevRange_25~fitData$jamsHere)
boxplot(fitData$latRange_25~fitData$jamsHere)
boxplot(fitData$latRange_50~fitData$jamsHere)


#keeping df seperate makes predict easier to use
fitData$lUAA=log(fitData$UAA)
logitJams=glm(jamsHere~poly(lUAA,2)*latRange_25,data=fitData,family = binomial,na.action = na.fail)
summary(logitJams)
dredge(logitJams,extra="R^2")
boxplot(logitJams$fitted.values~fitData$jamsHere,xlab="dense jams observed (j/km >=5)",ylab="predicted probability of dense jams",main="logit model of dense jams")


plot(predict(logitJams)~fitData$UAA)
plot(predict(logitJams)~fitData$slope)

segDataPredict=data.frame(locationIDX=segData$locationIDX,
                          denseJamProbability=plogis(predict.glm(logitJams,newdata=data.frame(lUAA=log(segData$UAA),
                                                                                              latRange_25=segData$latRange_25))))
segDataPredict$denseJamProbability=round(segDataPredict$denseJamProbability,3)

hist(segDataPredict$denseJamProbability) #seems OK!

plot(segDataPredict$denseJamProbability~segData$slope)

#join dataPredict to coords
segDataPredict=left_join(segDataPredict,
                         dbGetQuery(leakyDB,"SELECT Locations.locationIDX,Points.X, Points.Y FROM Locations LEFT JOIN Points ON Locations.locationIDX = Points.pointIDX"))
write.csv(segDataPredict,"predictedDenseJamProbability.csv")



#########consider the form of a negative binomial distribution as a possible form for jams~uaa
nb_func=function(r,p=0.5,kmin=0){
  return((r-1)*(1-p)^r*p^kmin)
}
plot(nb_func(1:100,p=0.2))