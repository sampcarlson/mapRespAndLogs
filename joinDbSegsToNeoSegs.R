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

source("C:/Users/sam/Documents/R/projects/rGrassTools/grassTools.r")
source('~/R/projects/mapRespAndLogs/dataByBatch.R')
leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
neoNetPath="C:/Users/sam/Documents/LeakyRivers/Data/shpArchive/neoNetwork/NSVStreamSegmentsIDX.shp"



addBatchDataToNeoNetwork=function(returnVars,batchID=6){
  
  allPtData=dataByBatch(batchID)[,c("locationIDX",returnVars)]
  
  allSegs=dbGetQuery(leakyDB,"SELECT Locations.locationIDX,Points.X, Points.Y FROM Locations LEFT JOIN Points ON Locations.locationIDX = Points.pointIDX WHERE Locations.isPoint='1' ")
  allSegs=allSegs[complete.cases(allSegs),]
  allSegs_spdf=SpatialPointsDataFrame(coords=allSegs[,c("X","Y")],data=data.frame(locationIDX=allSegs$locationIDX))
  #rgdal::writeOGR(allSegs_spdf,"batch6pts.shp",layer="batrh6pts",driver="ESRI Shapefile")
  InitGrass_byRaster(rasterPath = "C:/Users/Sam/Documents/spatial/data/dem/leakyRivers/trim/LeakyRiversDEM_rectTrim_knobFix_NSV.tif")
  writeVECT(allSegs_spdf,"dbSegPoints",v.in.ogr_flags = (c("overwrite","o")))
  
  neoNet=shapefile(neoNetPath)
  writeVECT(neoNet,"neoSegs",v.in.ogr_flags = (c("overwrite")))
  neoResultDF=neoNet@data
  for(addVar in returnVars){
    neoResultDF$tempColName=NA
    names(neoResultDF)[names(neoResultDF)=="tempColName"]=addVar
  }
  
  for(autoCat in neoNet@data$AUTO){
    execGRASS("v.extract",input="neoSegs",where=paste0("AUTO = '",autoCat,"'"),output="thisNeoSeg",flags=c( "quiet","overwrite"))
    distFromSeg=grassTableToDF(execGRASS("v.distance",from="thisNeoSeg",to="dbSegPoints",dmax=500,upload=c("to_attr","dist"),to_column="locationIDX",flags=c("a","p","quiet"),intern=T))
    distFromSeg=distFromSeg[base::order(distFromSeg$dist),]
    distFromSeg=distFromSeg[1:5,]
    print(paste("mean dist from seg ",autoCat,"to db pts = ",round(mean(distFromSeg$dist,na.rm=T)),"m"))
    thisNeoSegData=inner_join(distFromSeg,allPtData,by=c("to_attr"="locationIDX"))
    for(addVar in returnVars){
      thisVal=mean(thisNeoSegData[,addVar],na.rm=T)
      neoResultDF[neoResultDF$AUTO==autoCat,addVar]=thisVal
    }
  }
  return(neoResultDF)
}

neoDF=addBatchDataToNeoNetwork(returnVars=c("latRange_10","UAA","elevation"))
neoDF$lUAA=log(neoDF$UAA)
write.csv(neoDF,"neoDF.csv")

######build jam glm--------------
buildJamGlm=function(){
  leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
  
  wlData=dataByBatch(4)[,c("locationIDX","jamsPerKm","mean_elevation","mean_latRange_10","mean_latRange_25","mean_minLatRange_10","mean_minLatRange_25","mean_slope","mean_SPI","mean_UAA","jamCount","channelLength")]
  wbData=dataByBatch(5)[,c("locationIDX","jamsPerKm","mean_elevation","mean_latRange_10","mean_latRange_25","mean_minLatRange_10","mean_minLatRange_25","mean_slope","mean_SPI","mean_UAA","jamCount","channelLength")]
  jamData=rbind(wlData,wbData)
  
  wlDataCats=dataByBatch(4,meow=T)
  wbDataCats=dataByBatch(5,meow=T)
  
  #land use / mgmt
  jamDataCats=rbind(wlDataCats[wlDataCats$dataTypeIDX==22,][,c("locationIDX","value")],
                    wbDataCats[wbDataCats$dataTypeIDX==42,][,c("locationIDX","value")])
  names(jamDataCats)[2]="mgmt"
  jamData=left_join(jamData,jamDataCats)
  jamData$isManaged=jamData$mgmt=="YM"
  
  #confinement
  jamDataCats=rbind(wlDataCats[wlDataCats$dataTypeIDX==23,][,c("locationIDX","value")],
                    wbDataCats[wbDataCats$dataTypeIDX==38,][,c("locationIDX","value")])
  names(jamDataCats)[2]="confinement"
  jamDataCats$confinement=toupper(jamDataCats$confinement)
  jamData=left_join(jamData,jamDataCats)
  
  
  nsv_locations=dbGetQuery(leakyDB,"SELECT locationIDX FROM Locations WHERE Locations.watershedID = 'NSV_def'")
  jamData=inner_join(jamData,nsv_locations)
  
  fitData=jamData[complete.cases(jamData),]
  
  fitData$isUnconf=fitData$confinement=="U"
  fitData$isConf=fitData$confinement=="C"
  
  fitData$lUAA=log(fitData$mean_UAA)
  fitData$channelLength=fitData$channelLength/1000 # convert to km for jams/km offset
  
  jamGlm=glm.nb(jamCount~mean_latRange_10+lUAA+I(lUAA^2)+mean_elevation:(lUAA+I(lUAA^2))+offset(log(channelLength)),data=fitData,na.action=na.fail,control = glm.control(maxit=500))
  
  return(jamGlm)
}
jamGlm=buildJamGlm()

#####build resp glm--------------
buildRespGlm=function(){
  leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")

  #ER is in mmol 02 m^-2 day^-1
  #convert to g O2 m^-2 day^-1
  
  metabData$ER_g=-metabData$ER*32/1000
  plot(metabData$mean_jamsPerKm,metabData$ER_g)
  metabModel_g=lm(ER_g~mean_jamsPerKm,data=metabData)

  return(metabModel_g)
  
}
respGlm=buildRespGlm()

#predict and write-------
neoDF$jamsPerKm=exp(predict(jamGlm,newdata=data.frame(mean_latRange_10=neoDF$latRange_10,
                                                      lUAA=neoDF$lUAA,
                                                      mean_elevation=neoDF$elevation,
                                                      channelLength=1)))
hist(neoDF$jamDensity)

#g O2 m^-2 day^-1
neoDF$R_g_d=predict(respGlm,newdata=data.frame(mean_jamsPerKm=neoDF$jamsPerKm))

#need R as mg O2 m−2 sec−1 
neoDF$resp=neoDF$R_g_d*(1000/86400)

neoPredictDF=neoDF[,c("AUTO","jamsPerKm","resp")]
write.csv(neoPredictDF,"C:/Users/sam/Dropbox/Logjams/defineRespAndJams/neoPredictDf.csv")
