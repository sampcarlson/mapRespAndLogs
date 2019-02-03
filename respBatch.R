library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)
source('~/R/projects/mapRespAndLogs/dataByBatch.R')

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
dbGetQuery(leakyDB,"SELECT * FROM Batches")
dbGetQuery(leakyDB,"SELECT * FROM DataTypes")



metabData=dataByBatch(3,excludeDataTypeIDXs = c(40,64))
plot(metabData$mean_jamsPerKm,-metabData$ER)
metabModel=lm(-metabData$ER~metabData$mean_jamsPerKm)
abline(metabModel)
summary(metabModel)

#ER is in mmol 02 m^-2 day^-1
#convert to g O2 m^-2 day^-1

metabData$ER_g=-metabData$ER*32/1000
plot(metabData$mean_jamsPerKm,metabData$ER_g)
metabModel_g=lm(ER_g~mean_jamsPerKm,data=metabData)
abline(metabModel_g)
summary(metabModel_g)


#not much temperature effect apparent, especially after jam fit
plot(metabData$ER_g~metabData$temperature)

plot(metabModel$residuals,metabData$temperature)

#others:
plot(metabData$ER_g~metabData$mean_woodSurfaceArea)
plot(metabData$ER_g~metabData$mean_sedOCPerKm)

# windows()
# chart.Correlation(metabData[,c(2,4,7:26)])

png("predictR.png",width=400,height=400)
par(mar=c(3.1,3.5,1,1))
par(mfrow=c(2,2))

plot(metabData$ER_g~metabData$temperature,xlab="",ylab="",ylim=c(0,10),pch=16)
mtext(side=1,line=2.2,expression("Temperature ("*degree*C*")"),cex=0.8)
mtext(side=2,line=2,expression("Respiration Rate (g O"[2]*" m"^-2*" day"^-1*")" ),cex=0.8)

plot(metabData$ER_g~metabData$mean_sedOCPerKm,xlab="",ylab="",ylim=c(0,10),pch=16)
mtext(side=1,line=2.2,expression("Sediment Carbon (Kg C km"^-1*")"),cex=0.8)
mtext(side=2,line=2,expression("Respiration Rate (g O"[2]*" m"^-2*" day"^-1*")" ),cex=0.8)

plot(metabData$mean_jamsPerKm,metabData$ER_g,xlab="",ylab="",ylim=c(0,10),pch=16)
mtext(side=1,line=2.2,expression("Jam Density (count km"^-1*")"),cex=0.8)
mtext(side=2,line=2,expression("Respiration Rate (g O"[2]*" m"^-2*" day"^-1*")" ),cex=0.8)
metabModel=lm(metabData$ER_g~metabData$mean_jamsPerKm)
abline(metabModel)

plot(metabData$ER_g~metabData$mean_woodSurfaceArea,xlab="",ylab="",ylim=c(0,10),pch=16)
mtext(side=1,line=2.2,expression("Wood Surface Area (m"^2*" km"^-1*")"),cex=0.8)
mtext(side=2,line=2,expression("Respiration Rate (g O"[2]*" m"^-2*" day"^-1*")" ),cex=0.8)
dev.off()
par(mfrow=c(1,1))
