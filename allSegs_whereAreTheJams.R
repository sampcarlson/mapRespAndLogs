library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)
library(MuMIn)
source('~/R/projects/mapRespAndLogs/dataByBatch.R')

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
dbGetQuery(leakyDB,"SELECT * FROM Batches")
dbGetQuery(leakyDB,"SELECT * FROM DataTypes")



segData=dataByBatch(6)[,c("locationIDX","jamsPerKm","elevation","latRange_50","slope","SPI","UAA")]

#get / match some categorical data
segDataCats=dataByBatch(6,meow=T)
#i'm going to use my dem-derived 'confinement' term, but I need something for land use / mgmt
segDataCats=segDataCats[segDataCats$dataTypeIDX==27,]
segDataCats=left_join(segDataCats,dataByBatch(6)[,c("locationIDX","standAge")])
boxplot(segDataCats$standAge~segDataCats$value,ylim=c(0,500))
#use 100 year stand age as cutoff for old/young
#all nsv is unmanaged


#model jam density
jamData=segData[complete.cases(segData),]
jaModel=lm(jamData$jamsPerKm~jamData$elevation+jamData$latRange_50+jamData$slope+jamData$SPI+jamData$UAA,na.action=na.fail)
summary(jaModel)
dredge(jaModel)

jamData$jamsHere=jamData$jamsPerKm>=5
logitJams=glm(jamData$jamsHere~jamData$elevation+jamData$latRange_50+jamData$slope+jamData$SPI+jamData$UAA,family = binomial,na.action = na.fail)
dredge(logitJams)


#consider non-linear relationships
#might be best to fit uaa and slope seperatly, w/ possible interaction, rather than use SPI?
hist(log(jamData$UAA))

plot(jamData$jamsPerKm~log(jamData$UAA))

u=glm(jamsPerKm~log(UAA)+I(log(UAA)^2), data=jamData,na.action=na.fail)
summary(u)
dredge(u)
points(predict(u)~log(jamData$UAA), pch="*")


plot(jamData$jamsPerKm~jamData$UAA)

plot(jamData$jamsPerKm~jamData$slope)

plot(jamData$jamsPerKm~jamData$SPI)

plot(jamData$jamsPerKm~jamData$elevation)

plot(jamData$jamsPerKm~jamData$latRange_50)


#keeping df seperate makes predict easier to use
logitJams=glm(jamsHere~log(UAA) + I(log(UAA)^2)+latRange_50+slope,data=jamData,family = binomial,na.action = na.fail)
summary(logitJams)
dredge(logitJams,extra="R^2")
plot(logitJams$fitted.values,jamData$jamsHere)
boxplot(logitJams$fitted.values~jamData$jamsHere,xlab="dense jams observed (j/km >=5)",ylab="predicted probability of dense jams",main="logit model of dense jams")


plot(logitJams$fitted.values~jamData$latRange_50)

segDataPredict=data.frame(locationIDX=segData$locationIDX,
                          denseJamProbability=plogis(predict.glm(logitJams,newdata=data.frame(latRange_50=segData$latRange_50,
                                                                                              slope=segData$slope,
                                                                                              SPI = segData$SPI))))

hist(segDataPredict$denseJamProbability) #seems a little high

#join dataPredict to coords
segDataPredict=left_join(segDataPredict,
                         dbGetQuery(leakyDB,"SELECT Locations.locationIDX,Points.X, Points.Y FROM Locations LEFT JOIN Points ON Locations.locationIDX = Points.pointIDX"))
write.csv(segDataPredict,"predictedDenseJamProbability")


