library(RSQLite)
library(reshape2)
library(sp)
library(rgeos)
library(raster)
library(rgrass7)
library(dplyr)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)
library(RColorBrewer)
library(rgdal)

source("C:/Users/sam/Documents/R/projects/rGrassTools/grassTools.r")
source('~/R/projects/mapRespAndLogs/dataByBatch.R')
source('~/R/projects/mapRespAndLogs/fitWidth.R')
leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")

neoNetPath="C:/Users/sam/Dropbox/Logjams/nsv_modelInput/nsvSegsCopy/NSVStreamSegmentsIDX.shp"
#neoNetPath="C:/Users/sam/Documents/LeakyRivers/Data/shpArchive/neoNetwork/NSVStreamSegmentsIDX.shp"


addBatchDataToNeoNetwork=function(returnVars=returnVars,batchID=5){
  neoNetPath="C:/Users/sam/Dropbox/Logjams/nsv_modelInput/nsvSegsCopy/NSVStreamSegmentsIDX.shp"
  neoCharacteristicsPath="C:/Users/sam/Dropbox/Logjams/nsv_modelInput/nsvSegsCopy/NSVEndpointsFinal.shp"
  
  neoNet=shapefile(neoNetPath)
  neoUAA=shapefile(neoCharacteristicsPath)
  neoUAA$UAA=(neoUAA$rast_val*(9.11882^2))/(1000^2)
  neoUAA=neoUAA[,c("SegIDX","UAA")]
  
  neoDF=left_join(neoNet@data,neoUAA@data,by=c("AUTO" = "SegIDX"))
  neoDF$AUTO=as.numeric(neoDF$AUTO)
  neoDF=stats::aggregate(neoDF,by=list((id=neoDF$AUTO)),FUN=mean)[,c("AUTO","UAA")]
  
  allPtData=dataByBatch(batchID)[,c("locationIDX",returnVars)]
  
  allSegs=dbGetQuery(leakyDB,"SELECT Locations.isPoint, Locations.locationIDX, Points.X, Points.Y FROM Locations LEFT JOIN Points ON Locations.pointIDX = Points.pointIDX WHERE Locations.isPoint='1' ")
  allSegs=allSegs[complete.cases(allSegs),]
  
  allSegs_spdf=SpatialPointsDataFrame(coords=allSegs[,c("X","Y")],data=data.frame(lIDX=allSegs$locationIDX))
  
  InitGrass_byRaster(rasterPath = "C:/Users/Sam/Documents/spatial/data/dem/leakyRivers/trim/LeakyRiversDEM_rectTrim_knobFix_NSV.tif")
  
  writeVECT(allSegs_spdf,"dbSegPoints",v.in.ogr_flags = (c("overwrite","o")))
  #rgdal::writeOGR(allSegs_spdf,"batch5pts.shp",layer="batch5pts",driver="ESRI Shapefile")
  
  neoNet=shapefile(neoNetPath)
  writeVECT(neoNet,"neoSegs",v.in.ogr_flags = (c("overwrite")))
  neoResultDF=neoNet@data

  for(addVar in returnVars){
    neoResultDF$tempColName=NA
    names(neoResultDF)[names(neoResultDF)=="tempColName"]=addVar
  }
  
  for(autoCat in neoNet@data$AUTO){
    execGRASS("v.extract",input="neoSegs",where=paste0("AUTO = '",autoCat,"'"),output="thisNeoSeg",flags=c( "quiet","overwrite"))
    
    #execGRASS("v.out.ogr",input="thisNeoSeg",output="C:/Users/Sam/Desktop/thisNeoSeg.shp",format="ESRI_Shapefile")
    #execGRASS("v.out.ogr",input="dbSegPoints",output="C:/Users/Sam/Desktop/dbSegPoints.shp",format="ESRI_Shapefile",flags="overwrite")
    
    #execGRASS("v.to.points",input="thisNeoSeg",output="thisSegPts",dmax=100,flags=c("quiet","overwrite"))
    #thisSegPts=grassTableToDF(execGRASS("v.db.select",map="thisSegPts",layer="2",intern=T))
    # for(segPtCat in thisSegPts$cat){
    #   execGRASS("v.extract",input="thisSegPts",where=paste0("cat = ",segPtCat),output="thisSegPt",flags=c("quiet","overwrite"))
    #   grassTableToDF( execGRASS("v.distance",from="thisSegPt",to="dbSegPoints",upload=c("to_attr","dist"),to_column="locationIDX",flags=c("quiet","p"),intern=T))
    # }
    
    thisSegLength = grassTableToDF(execGRASS("v.report",map="thisNeoSeg",option="length",intern=T))$length
    nPoints=round(thisSegLength/100)
    
    distFromSeg=grassTableToDF(execGRASS("v.distance",from="thisNeoSeg",to="dbSegPoints",dmax=100,upload=c("to_attr","dist"),to_column="lIDX",flags=c("a","p","quiet"),intern=T))
    
    distFromSeg=left_join(distFromSeg,allPtData[,c("locationIDX","UAA")],by=c("to_attr"="locationIDX"))
    thisUAA=neoDF$UAA[neoDF$AUTO==autoCat]
    distFromSeg$fitMetric=abs(thisUAA-distFromSeg$UAA)*(distFromSeg$dist/10)
    
    distFromSeg=distFromSeg[base::order(distFromSeg$fitMetric),]
    
    
    distFromSeg=distFromSeg[1:nPoints,]
    distFromSeg$UAA=NULL
    
    print(paste("mean dist from seg ",autoCat,"to db pts = ",round(mean(distFromSeg$dist,na.rm=T)),"m"))
    thisNeoSegData=inner_join(distFromSeg,allPtData,by=c("to_attr"="locationIDX"))
    for(addVar in returnVars){
      thisVal=mean(thisNeoSegData[,addVar],na.rm=T)
      neoResultDF[neoResultDF$AUTO==autoCat,addVar]=thisVal
    }
  }
  return(neoResultDF)
}

neoDF=addBatchDataToNeoNetwork(returnVars=c("jamsPerKm","channelCount","UAA","elevation","latRange_10","slope"))
write.csv(neoDF,"neoDF.csv")

neoDF=read.csv("neoDF.csv")
#neoDF$channelCount[is.na(neoDF$channelCount)]=1
treeline=3250
minStreamSize=4
neoResult=read.csv("C:/Users/sam/Dropbox/Logjams/jamModelResults/2019-03-30_unlogged_1.csv")
neoResult=neoResult[,c("ID","Length")]
names(neoResult)[1]="AUTO"
sum(neoResult$Length,na.rm=T)/1000
neoDF_dataExtent=left_join(neoDF,neoResult)
neoDF_dataExtent=neoDF_dataExtent[neoDF_dataExtent$elevation<=treeline,]
neoDF_dataExtent=neoDF_dataExtent[neoDF_dataExtent$UAA>=minStreamSize,]
kmSurveyed=sum(neoDF_dataExtent$Length[!is.na(neoDF_dataExtent$jamsPerKm)],na.rm = T)/1000
kmUnsurveyed=sum(neoDF_dataExtent$Length[is.na(neoDF_dataExtent$jamsPerKm)],na.rm = T)/1000
kmUnsurveyed/(kmSurveyed+kmUnsurveyed)

medJamDensity=median(neoDF_dataExtent$jamsPerKm,na.rm=T)
medJamDensity
neoDF$jamsPerKm[is.na(neoDF$jamsPerKm) & neoDF$UAA>=minStreamSize & neoDF$elevation<=treeline]=medJamDensity
#use this to make a map of where jams and surveys are and aren't
#neoDF$jamsPerKm[is.na(neoDF$jamsPerKm)]=-1

neoDF$jamsPerKm[is.na(neoDF$jamsPerKm)]=0


widthFun=getWidthFun()

#####build resp glm--------------
#note this is sensitive ot dataTypeIDX changes
buildRespGlm=function(){
  leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
  metabData=dataByBatch(2,excludeDataTypeIDXs = c(76))
  
  #ER is in mmol 02 m^-2 day^-1
  #convert to g O2 m^-2 day^-1
  
  metabData$ER_g=-metabData$ER*32/1000
  plot(metabData$mean_jamsPerKm,metabData$ER_g)
  #metabModel_g=lm(ER_g~mean_jamsPerKm,data=metabData)
  #abline(metabModel_g)
  
  
  metab_nl=nls(ER_g~i+(v*mean_jamsPerKm)/(k+mean_jamsPerKm),data=metabData,start=list(i=1,v=8,k=20))
  summary(metab_nl)
  points(1:120,predict(metab_nl,newdata=data.frame(mean_jamsPerKm=1:120)),pch=".")
  
  
  return(metab_nl)
  
}
respGlm=buildRespGlm()

#predict and write-------

neoDF$jamsPerKm_unlogged=neoDF$jamsPerKm
neoDF$jamsPerKm_logged=(exp(-0.9480)*neoDF$jamsPerKm)
neoDF$jamsPerKm_logged_minConfInt=(exp((-0.9480+(1.96*0.2969)))*neoDF$jamsPerKm)
neoDF$jamsPerKm_logged_maxConfInt=(exp((-0.9480-(1.96*0.2967)))*neoDF$jamsPerKm)
#neoDF$jamsPerKm_logged=(exp(-1.26829)*neoDF$jamsPerKm)/neoDF$channelCount


hist(neoDF$jamsPerKm_unlogged)
hist(neoDF$jamsPerKm_logged)
neoDF$jamDiff=neoDF$jamsPerKm_unlogged-neoDF$jamsPerKm_logged
hist(neoDF$jamDiff)
mean(neoDF$jamDiff)

neoDF$channelCountOld=neoDF$channelCount
neoDF$channelCount[is.na(neoDF$channelCount)]=1

neoDF$width_unlogged=predict(widthFun,newdata=data.frame(UAA=neoDF$UAA,channelCount=neoDF$channelCount))
neoDF$width_logged=predict(widthFun,newdata=data.frame(UAA=neoDF$UAA,channelCount=1))

neoDF$channelCount=neoDF$channelCountOld

#resp fitted as g O2 m^-2 day^-1
#need R as mg O2 m−2 sec−1 
neoDF$resp_logged=predict(respGlm,newdata=data.frame(mean_jamsPerKm=neoDF$jamsPerKm_logged))*(1000/86400)
neoDF$resp_logged_minConfInt=predict(respGlm,newdata=data.frame(mean_jamsPerKm=neoDF$jamsPerKm_logged_minConfInt))*(1000/86400)
neoDF$resp_logged_maxConfInt=predict(respGlm,newdata=data.frame(mean_jamsPerKm=neoDF$jamsPerKm_logged_maxConfInt))*(1000/86400)


neoDF$resp_unlogged=predict(respGlm,newdata=data.frame(mean_jamsPerKm=neoDF$jamsPerKm_unlogged))*(1000/86400)


neoPredictDF=neoDF[,c("AUTO","jamsPerKm_unlogged","jamsPerKm_logged","jamsPerKm_logged_maxConfInt","jamsPerKm_logged_minConfInt","resp_unlogged","resp_logged","resp_logged_maxConfInt","resp_logged_minConfInt","channelCount","width_unlogged","width_logged")]
neoPredictDF$width_logged_maxConfInt=neoPredictDF$width_logged
neoPredictDF$width_logged_minConfInt=neoPredictDF$width_logged

neoPredictDF$jamsPerKm_logged_multi=neoPredictDF$jamsPerKm_logged
neoPredictDF$width_logged_multi=neoPredictDF$width_unlogged
neoPredictDF$resp_logged_multi=neoPredictDF$resp_logged

neoPredictDF$jamsPerKm_unlogged_single=neoPredictDF$jamsPerKm_unlogged
neoPredictDF$width_unlogged_single=neoPredictDF$width_logged
neoPredictDF$resp_unlogged_single=neoPredictDF$resp_unlogged

write.csv(neoPredictDF,"C:/Users/sam/Dropbox/Logjams/defineRespAndJams/neoPredictDf.csv")
