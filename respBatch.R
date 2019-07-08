library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)
library(MuMIn)
source('~/R/projects/mapRespAndLogs/dataByBatch.R')

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
dbGetQuery(leakyDB,"SELECT * FROM Batches")
dbGetQuery(leakyDB,"SELECT * FROM DataTypes")


#70 is mean of livers jam density, 78 is mean of beckman jam density
metabData=dataByBatch(2,excludeDataTypeIDXs = c(76))
metabData$ER_g=-metabData$ER*32/1000
metabData$jamsPerChannelKm=metabData$mean_jamsPerKm/metabData$mean_channelCount

plot(metabData$mean_jamsPerKm,-metabData$ER)
#plot(metabData$jamsPerChannelKm,-metabData$ER)

metabModel=lm(-metabData$ER~metabData$mean_jamsPerKm)
abline(metabModel)
summary(metabModel)

metabModel=nls(ER_g~i+j*mean_jamsPerKm,data=metabData,start=list(i=1,j=1))
metab_nl=nls(ER_g~i+(v*mean_jamsPerKm)/(k+mean_jamsPerKm),data=metabData,start=list(i=1,v=280,k=2))
summary(metab_nl)
summary(metabModel)
anova(metabModel,metab_nl)


#not much temperature effect apparent, especially after jam fit
plot(metabData$ER_g~metabData$temperature)
summary(lm(metabData$ER_g~metabData$temperature))
plot(predict(metab_nl)-metabData$ER_g,metabData$temperature)

#others:
plot(metabData$ER_g~metabData$mean_woodDepth)
plot(metabData$ER_g~metabData$mean_sedOCPerKm)
summary(lm(metabData$ER_g~metabData$mean_sedOCPerKm))

fitMe=metabData[,c("ER_g","mean_jamsPerKm","temperature","mean_woodDepth","mean_sedOCPerKm")]


fitMe=fitMe[complete.cases(fitMe),]

metabAll=glm(ER_g~(mean_jamsPerKm+temperature+mean_woodDepth+mean_sedOCPerKm)^2,data=fitMe,na.action=na.fail)
summary(metabAll)
dredge(metabAll,m.lim=c(0,3))
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
abline(metabModel,lty=3)
curve(1.559+(9.263*x)/(21.057+x),add=T)


plot(metabData$ER_g~metabData$mean_woodDepth,xlab="",ylab="",ylim=c(0,10),pch=16)
mtext(side=1,line=2.2,expression("Wood Volume Per Area (m"^3*" m"^-2*")"),cex=0.8)
mtext(side=2,line=2,expression("Respiration Rate (g O"[2]*" m"^-2*" day"^-1*")" ),cex=0.8)
dev.off()
par(mfrow=c(1,1))

