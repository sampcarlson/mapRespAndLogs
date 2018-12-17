library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
dbGetQuery(leakyDB,"SELECT * FROM Batches")

morphLocations=dbGetQuery(leakyDB,"SELECT DISTINCT Locations.locationIDX FROM Locations LEFT JOIN Data ON Locations.locationIDX = Data.locationIDX
                           WHERE Data.batchIDX = 4")$locationIDX

jamData=dbGetQuery(leakyDB,paste0("SELECT Data.locationIDX, DataTypes.metric, Data.value FROM Data
                                 LEFT JOIN DataTypes ON Data.dataTypeIDX = DataTypes.DataTypeIDX
                                 WHERE locationIDX IN (",paste(morphLocations,collapse=", "),")"))

jamDataCats=jamData[jamData$metric %in% c("confinement","landUse"),]
jamData=jamData[!jamData$metric %in% c("confinement","landUse"),]
jamData$value=as.numeric(jamData$value)
jamData=dcast(jamData,formula=locationIDX ~ metric,fill=NaN)
jamDataCats=dcast(jamDataCats,formula=locationIDX ~ metric,fill=NaN)
jamData=left_join(jamData,jamDataCats,by="locationIDX")
jamData$confinement=as.factor(jamData$confinement)
jamData$landUse=as.factor(jamData$landUse)
plot(jamData$jamsPerKm,jamData$mean_SPI)

#chart.Correlation(jamData[,2:20])
hist(jamData$jamsPerKm,breaks=10)
