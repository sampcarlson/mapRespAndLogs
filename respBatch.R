library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)
source('~/R/projects/mapRespAndLogs/dataByBatch.R')

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
dbGetQuery(leakyDB,"SELECT * FROM Batches")
dbGetQuery(leakyDB,"SELECT * FROM DataTypes")



metabData=dataByBatch(3,excludeDataTypeIDXs = c(37,58))
plot(metabData$mean_jamsPerKm,-metabData$ER)
metabModel=lm(-metabData$ER~metabData$mean_jamsPerKm)
summary(metabModel)

#not much temperature effect apparent, especially after jam fit
plot(-metabData$ER,metabData$temperature)

plot(metabModel$residuals,metabData$temperature)


#others
windows()
chart.Correlation(metabData[,c(2,4,7:26)])
