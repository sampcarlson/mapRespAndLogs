library(RSQLite)
library(reshape2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)


leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
dbGetQuery(leakyDB,"SELECT * FROM DataTypes")

erLocations=dbGetQuery(leakyDB,"SELECT Data.locationIDX FROM Data LEFT JOIN DataTypes ON Data.dataTypeIDX = DataTypes.dataTypeIDX WHERE DataTypes.metric = 'ER'")$locationIDX

erData=dbGetQuery(leakyDB,paste0("SELECT Data.locationIDX, DataTypes.metric, Data.value FROM Data
                                 LEFT JOIN DataTypes ON Data.dataTypeIDX = DataTypes.DataTypeIDX
                                 WHERE locationIDX IN (",paste(erLocations,collapse=", "),")"))
erData=dcast(erData,locationIDX~metric,)

cats=erData[,c("mean_confinement","mean_landUse")]
cats$mean_confinement=as.factor(cats$mean_confinement)
cats$mean_landUse=as.factor(cats$mean_landUse)
erData=data.frame(lapply(erData,as.numeric))
erData$ER=-erData$ER
erData$mean_confinement=cats$mean_confinement
erData$mean_landUse=cats$mean_landUse
erData$mean_confinement=NULL
erData$mean_landUse=NULL
#windows()
#plot(erData)
#chart.Correlation(erData[,2:23])

hist(erData$mean_jamsPerKm)
erFit=lm(erData$ER~erData$mean_jamsPerKm)
summary(erFit)
plot(erData$ER~erData$mean_jamsPerKm,ylim=c(0,300))
abline(erFit)
