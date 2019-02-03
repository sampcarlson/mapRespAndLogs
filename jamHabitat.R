library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)
library(MuMIn)
source('~/R/projects/mapRespAndLogs/dataByBatch.R')

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
dbGetQuery(leakyDB,"SELECT * FROM Batches")
dbGetQuery(leakyDB,"SELECT * FROM DataTypes")

wlData=dataByBatch(4)[,c("locationIDX","jamsPerKm","mean_latRange_10","mean_elevation","mean_slope","mean_SPI","mean_UAA")]
hist(wlData$jamsPerKm)
sum(wlData$jamsPerKm<=5)/nrow(wlData)
wbData=dataByBatch(5)[,c("locationIDX","jamsPerKm","mean_latRange_10","mean_elevation","mean_slope","mean_SPI","mean_UAA")]
hist(wbData$jamsPerKm)
sum(wbData$jamsPerKm<=5)/nrow(wbData)

jamData=rbind(wlData,wbData)

wlDataCats=dataByBatch(4,meow=T)
wbDataCats=dataByBatch(5,meow=T)

#i'm going to use my dem-derived 'confinement' term, but I need something for land use / mgmt
jamDataCats=rbind(wlDataCats[wlDataCats$dataTypeIDX==22,][,c("locationIDX","value")],
                  wbDataCats[wbDataCats$dataTypeIDX==42,][,c("locationIDX","value")])
names(jamDataCats)[2]="mgmt"
jamData=left_join(jamData,jamDataCats)

jamData$isManaged=jamData$mgmt=="YM"

jamData$jamsHere=F
jamData$jamsHere[jamData$jamsPerKm>5]=T

fitData=jamData[complete.cases(jamData),]

hist(fitData$jamsPerKm)

sparse_wt=sum(fitData$jamsHere)/sum(!fitData$jamsHere)


fitData$weight=1
fitData$weight[!fitData$jamsHere]=sparse_wt

hist(fitData$jamsPerKm[fitData$jamsHere])
hist(fitData$jamsPerKm[!fitData$jamsHere])


hist(fitData$jamsPerKm[fitData$isManaged])
hist(fitData$jamsPerKm[!fitData$isManaged])


boxplot(fitData$jamsPerKm~fitData$isManaged)

fitData$lUAA=log(fitData$mean_UAA)


#logitJams=glm(jamsHere~isManaged+(mean_elevation*poly(lUAA,2)*mean_latRange_10)^2,weights = fitData$weight ,data=fitData,family = binomial,na.action = na.fail)

logitJams=glm(jamsHere~isManaged+mean_elevation+poly(lUAA,2)+mean_latRange_10,weights = fitData$weight ,data=fitData,family = binomial,na.action = na.fail)

logitJams=glm(jamsHere~poly(lUAA,2),weights = fitData$weight ,data=fitData,family = binomial,na.action = na.fail)


summary(logitJams)

getSparseMean=function(m){
  return(mean(m$fitted.values[fitData$jamsHere==F]))
}
dredge(logitJams,extra= c("getSparseMean","R^2"))


boxplot(logitJams$fitted.values~fitData$jamsHere,range=1.5,
        xlab="dense jams observed (j/km >=5)",ylab="predicted probability of dense jams",main="logit model of dense jams")



plot(plogis(predict(logitJams))~fitData$lUAA)
plot(plogis(predict(logitJams))~fitData$mean_elevation)

segData=dataByBatch(6)[,c("locationIDX","latRange_10","latRange_50","slope","SPI","UAA")]

segDataPredict=data.frame(locationIDX=segData$locationIDX,
                          denseJamProbability=plogis(predict.glm(logitJams,newdata=data.frame(lUAA=log(segData$UAA),
                                                                                              mean_latRange_25=segData$latRange_25,
                                                                                              mean_slope=segData$slope,
                                                                                              isManaged=F))))
hist(segDataPredict$denseJamProbability) 

plot(segDataPredict$denseJamProbability~segData$slope)

plot(segDataPredict$denseJamProbability~segData$UAA)


#join dataPredict to coords
segDataPredict=left_join(segDataPredict,
                         dbGetQuery(leakyDB,"SELECT Locations.locationIDX,Points.X, Points.Y FROM Locations LEFT JOIN Points ON Locations.locationIDX = Points.pointIDX"))
write.csv(segDataPredict,"predictedDenseJamProbability.csv")


nsvData=dataByBatch(6)[,c("locationIDX","latRange_10","elevation","slope","SPI","UAA","jamsPerKm")]
nsv_locations=dbGetQuery(leakyDB,"SELECT locationIDX FROM Locations WHERE Locations.watershedID = 'NSV_def'")
nsvData=inner_join(nsv_locations,nsvData)
nrow(nsvData)/10 # km
hist(nsvData$elevation)

nsvData=nsvData[complete.cases(nsvData),]
nrow(nsvData)/10 # km
hist(nsvData$elevation)
3200 * 3.2808
#unreasonably small min basin size used here, so stream network is oddly long
#actually 69 km of streams in neo model - ~41% surveyed