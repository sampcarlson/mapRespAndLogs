library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
dbGetQuery(leakyDB,"SELECT * FROM Batches")

nsvLocations=dbGetQuery(leakyDB,"SELECT DISTINCT Locations.locationIDX FROM Locations LEFT JOIN Data ON Locations.locationIDX = Data.locationIDX
                          WHERE Data.batchIDX = 5")$locationIDX
nsvData=dbGetQuery(leakyDB,paste0("SELECT Data.locationIDX, DataTypes.metric, Data.value FROM Data
                                  LEFT JOIN DataTypes ON Data.dataTypeIDX = DataTypes.DataTypeIDX
                                  WHERE locationIDX IN (",paste(nsvLocations,collapse=", "),")"))

nsvData$value=as.numeric(nsvData$value)
nsvData=dcast(nsvData,formula=locationIDX ~ metric,fun.aggregate = mean)

morphLocations=dbGetQuery(leakyDB,"SELECT DISTINCT Locations.locationIDX FROM Locations LEFT JOIN Data ON Locations.locationIDX = Data.locationIDX
                           WHERE Data.batchIDX = 4")$locationIDX

jamData=dbGetQuery(leakyDB,paste0("SELECT Data.locationIDX, DataTypes.metric, Data.value FROM Data
                                 LEFT JOIN DataTypes ON Data.dataTypeIDX = DataTypes.DataTypeIDX
                                 WHERE locationIDX IN (",paste(morphLocations,collapse=", "),")"))


jamData=jamData[!jamData$metric %in% c("confinement","landUse"),]
jamData$value=as.numeric(jamData$value)
jamData=dcast(jamData,formula=locationIDX ~ metric,fill=NaN)

newNames=strsplit(names(jamData),split="mean_")
getLast=function(x){
  l=length(x)
  return(x[l])
}
names(jamData)=sapply(newNames,getLast)
jamData=jamData[,names(jamData) %in% c(names(nsvData),"jamsPerKm")]

nsvData$jamsPerKm=0
nsvData=rbind(jamData,nsvData)

nsvData$jamsFoundHere=F
nsvData$jamsFoundHere[nsvData$jamsPerKm>=5]=T
nsvData=nsvData[complete.cases(nsvData),]

boxplot(nsvData$latRange_25~nsvData$jamsFoundHere)
boxplot(nsvData$slope~nsvData$jamsFoundHere)
boxplot(nsvData$elevRange_25~nsvData$jamsFoundHere)


whereJamModel=glm(nsvData$jamsFoundHere~nsvData$slope+nsvData$elevRange_25+nsvData$slope,family=binomial())
summary(whereJamModel)
plot(whereJamModel$fitted.values,nsvData$jamsFoundHere)
