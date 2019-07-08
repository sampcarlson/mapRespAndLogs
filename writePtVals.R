library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)
source('~/R/projects/mapRespAndLogs/dataByBatch.R')

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
dbGetQuery(leakyDB,"SELECT * FROM Batches")
pts=dataByBatch(5)
coords=dbGetQuery(leakyDB,"SELECT locationIDX, X, Y FROM Points INNER JOIN Locations ON Locations.pointIDX = Points.pointIDX")
pts=left_join(pts,coords)
write.csv(pts,"C:/Users/sam/Documents/Spatial/allPoints.csv")

write.table(rep("Real",ncol(pts)),"C:/Users/sam/Documents/Spatial/allPoints.csvt",quote=T,sep=", ",col.names=F,row.names = F,eol=",")
