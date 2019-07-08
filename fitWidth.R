getWidthFun=function(){
  library(RSQLite)
  library(reshape2)
  library(dplyr)
  library(PerformanceAnalytics)
  library(MuMIn)
  source('~/R/projects/mapRespAndLogs/dataByBatch.R')
  
  leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
  dbGetQuery(leakyDB,"SELECT * FROM Batches")
  dbGetQuery(leakyDB,"SELECT * FROM DataTypes")
  
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
  
  #allWidths$fitWidth=allWidths$bankfullWidth*0.8593
  allWidths$fitWidth=allWidths$wettedWidth
  #luna=nls(fitWidth~a*UAA^b + c*channelCount*UAA^b,data=allWidths,start=list(a=1,b=0.4,c=1),control = nls.control(maxiter=5000))
  
  #luna=nls(fitWidth~a*UAA^b + c*channelCount*UAA^b + j*(jamsPerKm/channelCount)*UAA^b,data=allWidths,start=list(a=1,b=0.4,c=1,j=1),control = nls.control(maxiter=5000))
  
  
  luna=nls(fitWidth~ a*UAA^b + c*(channelCount-1)*a*UAA^b  ,data=allWidths,start=list(a=1,b=0.4,c=1),control = nls.control(maxiter=5000))
  
  #luna=nls(fitWidth~ a*UAA^b + c*(jamsPerKm)*a*UAA^b  ,data=allWidths,start=list(a=1,b=0.4,c=1),control = nls.control(maxiter=5000))
  
 
  summary(luna)
  
  plot(predict(luna)~allWidths$fitWidth)
  #points(predict(luna_c)~allWidths$fitWidth,pch="*")
  
  abline(a=0,b=1)
  
  return(luna)
}
