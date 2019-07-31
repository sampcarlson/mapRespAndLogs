getWidthFun=function(){
  library(RSQLite)
  library(reshape2)
  library(dplyr)
  library(PerformanceAnalytics)
  library(MuMIn)
  source('~/R/projects/mapRespAndLogs/dataByBatch.R')
  
  leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
  #dbGetQuery(leakyDB,"SELECT * FROM Batches")
  #dbGetQuery(leakyDB,"SELECT * FROM DataTypes")
  
  wbWidths=dataByBatch(4)
  msWidths=dataByBatch(1)
  
  wbWidths=wbWidths[,names(wbWidths)%in%names(msWidths)]
  
  names(msWidths)[!names(msWidths)%in%names(wbWidths)]
  msWidths=msWidths[,names(msWidths)%in%names(wbWidths)]
  
  allWidths=rbind(wbWidths,msWidths)
  names(allWidths)

  allWidths$wettedWidth=allWidths$mean_wettedWidth
  allWidths=allWidths[,c("locationIDX","bankfullWidth","wettedWidth","mean_channelCount","mean_elevation","mean_individualBankfullWidth","mean_jamsPerKm","mean_latRange_10","mean_latRange_25","mean_minLatRange_10","mean_minLatRange_25","mean_slope","mean_slope_25","mean_SPI","mean_UAA","mean_valleyWidth_05","mean_valleyWidth_1")]
  
  sum(!is.na(allWidths$bankfullWidth))
  sum(!is.na(allWidths$wettedWidth))
  
  
  
  ww_bw=lm(mean_wettedWidth~bankfullWidth,data=msWidths)
  summary(ww_bw)
  #ww = 0.37644 + bw * 0.76634
  plot(msWidths$mean_wettedWidth~msWidths$bankfullWidth,ylab="Wetted Width (m)",xlab="Bankfull Width (m)")
  abline(ww_bw)
  
  allWidths$wettedWidth[is.na(allWidths$wettedWidth)]=predict.lm(ww_bw,newdata=data.frame(bankfullWidth=allWidths$bankfullWidth[is.na(allWidths$wettedWidth)]))
  #allWidths$mean_channelCount=round(allWidths$mean_channelCount)

  #luna=nls(wettedWidth~a*mean_UAA^b + c*mean_channelCount*mean_UAA^b,data=allWidths,start=list(a=1,b=0.4,c=1),control = nls.control(maxiter=5000))
  
  #luna=nls(wettedWidth~a*mean_UAA^b + c*mean_channelCount*mean_UAA^b + j*(mean_jamsPerKm/mean_channelCount)*mean_UAA^b,data=allWidths,start=list(a=1,b=0.4,c=1,j=1),control = nls.control(maxiter=5000))
  fitData=allWidths[,c("wettedWidth","mean_channelCount","mean_UAA","mean_slope")]
  fitData=fitData[complete.cases(fitData),]
  
  luna=nls(wettedWidth~ a*mean_UAA^b + c*(mean_channelCount-1)*a*mean_UAA^b  ,data=fitData,start=list(a=1,b=0.4,c=1),control = nls.control(maxiter=5000))
  
  
  #luna=nls(wettedWidth~ a*mean_UAA^b + c*(mean_jamsPerKm)*a*mean_UAA^b  ,data=allWidths,start=list(a=1,b=0.4,c=1),control = nls.control(maxiter=5000))
  plot(allWidths$wettedWidth~allWidths$mean_UAA)
  
  plot(allWidths$wettedWidth~round(allWidths$mean_channelCount))
  
  plot(allWidths$wettedWidth~allWidths$mean_jamsPerKm)
  
  plot(allWidths$wettedWidth~allWidths$mean_slope)
  
  summary(luna)
  
  scalar=115
  png(height=4*scalar,width=4*scalar,filename = "widthModelFit.png")
  plot(predict(luna)~fitData$wettedWidth,xlab="Observed wetted width (m)", ylab="Predicted wetted width (m)",cex=1.5,log="xy")
  abline(a=0,b=1)
  dev.off()
  
  wlDataCats=dataByBatch(3,meow=T)
  wbDataCats=dataByBatch(4,meow=T)
  cats=rbind(wlDataCats,wbDataCats)
  cats=cats[cats$metric=="landUse",]
  cats$isManaged=cats$value=="YM"
  cats=cats[,c("locationIDX","isManaged")]
  
  allWidths=left_join(allWidths,cats,by="locationIDX")
  
  nsv_locations=dbGetQuery(leakyDB,"SELECT * FROM Locations WHERE watershedID = 'NSV_def'")
  allWidths$isNSV=allWidths$locationIDX %in% nsv_locations$locationIDX
  allWidths$mean_channelCount[is.na(allWidths$mean_channelCount)]=1
  allWidths$channelCount_int=round(allWidths$mean_channelCount)
  allWidths$wettedWidthLogged=allWidths$wettedWidth/allWidths$channelCount_int
  
  scalar=200
  png(height=4*scalar,width=4*scalar,filename = "widthModelFit.png")
  plot(allWidths$wettedWidth[allWidths$isNSV]~allWidths$mean_UAA[allWidths$isNSV])
  points(allWidths$wettedWidthLogged[allWidths$isNSV]~allWidths$mean_UAA[allWidths$isNSV],pch="*")
  points(allWidths$wettedWidth[allWidths$isManaged]~allWidths$mean_UAA[allWidths$isManaged],pch=2)
  
  return(luna)
}
