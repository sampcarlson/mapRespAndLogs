getJamEffectDiff=function(m){
  segData=read.csv("segData.csv")
  m_logged=exp(predict(m,newdata=data.frame(med_latRange_10=segData$latRange_10,
                                        med_slope=segData$slope,
                                        lUAA=log(segData$UAA),
                                        med_elevation=segData$elevation,
                                        channelLength=1,
                                        isManaged=T)))
  m_unLogged=exp(predict(m,newdata=data.frame(med_latRange_10=segData$latRange_10,
                                          med_slope=segData$slope,
                                          lUAA=log(segData$UAA),
                                          med_elevation=segData$elevation,
                                          channelLength=1,
                                          isManaged=F)))
  return(round(mean(m_unLogged,na.rm=T)-mean(m_logged,na.rm=T),1))
  
}

maxP=function(m){
  return(max(coef(summary(m))[,4]))
}