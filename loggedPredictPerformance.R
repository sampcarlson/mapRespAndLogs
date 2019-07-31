library(RSQLite)
library(reshape2)
library(dplyr)
library(MASS)
library(RColorBrewer)
source('~/R/projects/mapRespAndLogs/dataByBatch.R')

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
#dbGetQuery(leakyDB,"SELECT * FROM Batches")
#dbGetQuery(leakyDB,"SELECT * FROM DataTypes")

############--------build----------------

wlData=dataByBatch(3)[,c("locationIDX","jamsPerKm","mean_elevation","mean_latRange_10","mean_latRange_25","mean_minLatRange_10","mean_minLatRange_25","mean_slope","mean_SPI","mean_UAA","jamCount")]
wbData=dataByBatch(4)[,c("locationIDX","jamsPerKm","mean_elevation","mean_latRange_10","mean_latRange_25","mean_minLatRange_10","mean_minLatRange_25","mean_slope","mean_SPI","mean_UAA","jamCount")]
jamData=rbind(wlData,wbData)
#jamData$mean_channelCount[is.na(jamData$mean_channelCount)]=1

wlDataCats=dataByBatch(3,meow=T)
wbDataCats=dataByBatch(4,meow=T)

#land use / mgmt
jamDataCats=rbind(wlDataCats[wlDataCats$dataTypeIDX==13,][,c("locationIDX","value")],
                  wbDataCats[wbDataCats$dataTypeIDX==32,][,c("locationIDX","value")])
names(jamDataCats)[2]="mgmt"
jamData=left_join(jamData,jamDataCats)
jamData$isManaged=jamData$mgmt=="YM"

#confinement
jamDataCats=rbind(wlDataCats[wlDataCats$dataTypeIDX==14,][,c("locationIDX","value")],
                  wbDataCats[wbDataCats$dataTypeIDX==28,][,c("locationIDX","value")])
names(jamDataCats)[2]="confinement"
jamDataCats$confinement=toupper(jamDataCats$confinement)
jamData=left_join(jamData,jamDataCats)

jamData$isConf=jamData$confinement=="C"
jamData$isUnconf=jamData$confinement=="U"
jamData$lUAA=log(jamData$mean_UAA)
jamData$channelLength=jamData$channelLength/1000

nsv_locations=dbGetQuery(leakyDB,"SELECT * FROM Locations WHERE watershedID = 'NSV_def'")
jamData$isNSV=jamData$locationIDX %in% nsv_locations$locationIDX




jamData$isManaged=as.numeric(jamData$isManaged)
jamData$isUnconf=as.numeric(jamData$isUnconf)


###########jam distribution plot----------
scalar=120
png(height=3.5*scalar,width=5*scalar,filename = "jamDistribution.png")

plot(density(jamData$jamsPerKm[jamData$isNSV],bw="SJ",adjust=1,from=0),ylim=c(0,0.1),xlim=c(0,150),col=unloggedCol,lwd=4,main="",xlab="Jams per Km",xaxs="i")
lines(density(jamData$jamsPerKm[jamData$isManaged==1],bw="SJ",adjust=1,from=0),col=loggedCol,lwd=4)
lines(density(jamData$jamsPerKm[jamData$isNSV]/2,bw="SJ",adjust=1,from=0),lty=3,lwd=3)

legend(x="topright",legend=c("Unlogged","Logged (observed)","Logged (simulated)"),lwd=c(4,4,3),lty=c(1,1,3),col=c(unloggedCol,loggedCol,"black"),bty="n",seg.len=2.5)
dev.off()




######jam density prediction performance plot------------

jamData=jamData[jamData$mean_elevation<=3250,]
jamData=jamData[jamData$mean_UAA>=4,]
jamData=jamData[complete.cases(jamData),]


#only 19 logged points
#unlogged: #1b9e77
#logged: #d95f02
unloggedCol=col2rgb("#1b9e77",alpha=T)
unloggedCol[4]=200
unloggedCol=rgb(t(unloggedCol),maxColorValue = 256)
loggedCol=col2rgb("#d95f02",alpha=T)
loggedCol[4]=200
loggedCol=rgb(t(loggedCol),maxColorValue=256)

scalar=120
png(height=3.5*scalar,width=5*scalar,filename = "jamPredictUAA.png")

plot(jamData$jamsPerKm[jamData$isNSV]~exp(jamData$lUAA[jamData$isNSV]),log="x",pch=16,cex=2,col=unloggedCol,xlab="",ylab="")
mtext(side=1,"UAA",cex=1,line=3)
mtext(side=2,"Jams per Km",cex=1,line=3)
for(i in 1:length(jamData$jamCount[jamData$isNSV])){
  lines(x=rep(exp(jamData$lUAA[jamData$isNSV][i]),2),y=c(jamData$jamsPerKm[jamData$isNSV][i],jamData$jamsPerKm[jamData$isNSV][i]/2),col="grey",lty=3,lwd=2)
}

points(jamData$jamsPerKm[jamData$isManaged==1]~exp(jamData$lUAA[jamData$isManaged==1]),pch=16,cex=2,col=loggedCol)
#points(jamData$jamsPerKm[jamData$isManaged==1]~exp(jamData$lUAA[jamData$isManaged==1]),pch=16,cex=.5,col="#d95f02")

points(jamData$jamsPerKm[jamData$isNSV]/2~exp(jamData$lUAA[jamData$isNSV]),pch=20,cex=2)
#points(jamData$jamsPerKm[jamData$isNSV]/2~exp(jamData$lUAA[jamData$isNSV]),pch=1,cex=4,lwd=1)
legend(x="topright",legend=c("Unlogged", "Logged (observed)", "Logged (simulated)"),pch=c(16,16,20),col=c(unloggedCol,loggedCol,"black"),pt.cex=c(2,2,2),lwd=1,lty=0,bty="n")
dev.off()


# jamData$this_x=jamData$mean_slope
# scalar=200
# png(height=3*scalar,width=5*scalar,filename = "jamPerformance.png")
# plot(jamData$jamsPerKm[jamData$isNSV]~jamData$this_x[jamData$isNSV],log="",pch=16,cex=2,col=unloggedCol)
# 
# for(i in 1:length(jamData$jamCount[jamData$isNSV])){
#   lines(x=rep(jamData$this_x[jamData$isNSV][i],2),y=c(jamData$jamsPerKm[jamData$isNSV][i],jamData$jamsPerKm[jamData$isNSV][i]/2),col="grey",lty=3)
# }
# 
# points(jamData$jamsPerKm[jamData$isNSV]/2~jamData$this_x[jamData$isNSV],pch=16,col=loggedCol,cex=3,lwd=5)
# #points(jamData$jamsPerKm[jamData$isNSV]/2~jamData$this_x[jamData$isNSV],pch=1,cex=4,lwd=1)
# 
# 
# points(jamData$jamsPerKm[jamData$isManaged==1]~jamData$this_x[jamData$isManaged==1],pch=1,cex=3,lwd=3)
# #points(jamData$jamsPerKm[jamData$isManaged==1]~jamData$this_x[jamData$isManaged==1]),pch=16,cex=.5,col="#d95f02")
# 
# dev.off()




