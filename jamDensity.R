library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)
library(RColorBrewer)
source('~/R/projects/mapRespAndLogs/dataByBatch.R')

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
dbGetQuery(leakyDB,"SELECT * FROM Batches")
dbGetQuery(leakyDB,"SELECT * FROM DataTypes")

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


##########confinement#########
#10 is good
boxplot(fitData$mean_latRange_10~fitData$confinement)
boxplot(fitData$mean_latRange_25~fitData$confinement)

boxplot(fitData$mean_minLatRange_10~fitData$confinement)
boxplot(fitData$mean_minLatRange_25~fitData$confinement)

fitData$isUnconf=fitData$confinement=="U"
fitData$isConf=fitData$confinement=="C"

boxplot(fitData$mean_minLatRange_10~fitData$isUnconf)
boxplot(fitData$mean_minLatRange_10~fitData$isConf)

mgdData=fitData[fitData$isManaged,]
nmgdData=fitData[!fitData$isManaged,]

###############jam density###############
hist(fitData$jamsPerKm)
boxplot(fitData$jamsPerKm~fitData$isManaged)

fitData$lUAA=log(fitData$mean_UAA)
fitData$channelLength=fitData$channelLength/1000 # convert to km for jams/km offset
reds=brewer.pal(9,"Reds")
latRange_colors=reds[2+cut(fitData$mean_latRange_10,7,labels=F)]
plot(fitData$jamsPerKm~fitData$lUAA,pch=as.numeric(fitData$isManaged)+16,cex=0.25+cut(fitData$mean_elevation,5,labels=F)/3,col=latRange_colors)

plot(fitData$jamsPerKm~fitData$mean_minLatRange_10)
plot(fitData$jamsPerKm~fitData$mean_latRange_10)
plot(fitData$jamsPerKm~fitData$mean_slope)



wt_mgd=62/68
wt_nmgd=6/68
fitData$weights=1
fitData$weights[fitData$isManaged]=2
fitData$weights[!fitData$isManaged]=1

# mgdData=fitData[fitData$isManaged,]
# fitData=fitData[!fitData$isManaged,]

#jamGlm=glm(round(jamsPerKm)~isManaged+poly(lUAA,2)*mean_latRange_25,data=fitData,na.action=na.fail,family = poisson)


#jamGlm=glm.nb(round(jamsPerKm)~isManaged:poly(lUAA,2)+poly(lUAA,2)*mean_latRange_10,data=fitData,na.action=na.fail)
#jamGlm=glm.nb(jamCount~mean_latRange_10:(lUAA+I(lUAA^2))+(lUAA+I(lUAA^2))+offset(log(channelLength)),data=fitData,na.action=na.fail)

#jamGlm=glm.nb(round(jamsPerKm)~(mean_SPI+mean_elevation+mean_slope+mean_latRange_10+(lUAA+I(lUAA^2)))^2,data=fitData,na.action=na.fail)


#jamGlm=glm.nb(jamCount~isManaged*mean_latRange_10+(lUAA+I(lUAA^2))+isManaged:lUAA+offset(log(channelLength)),data=fitData,na.action=na.fail,control = glm.control(maxit=500))

#jamGlm=glm.nb(jamCount~isManaged*mean_latRange_10+(lUAA+I(lUAA^2))+offset(log(channelLength)),data=fitData,na.action=na.fail,control = glm.control(maxit=500))


#jamGlm=glm.nb(jamCount~isManaged+mean_latRange_10+(lUAA+I(lUAA^2))+offset(log(channelLength)),data=fitData,na.action=na.fail,control = glm.control(maxit=500),weights = fitData$weights)

#jamGlm_noOffset=glm.nb(jamCount~isManaged+mean_latRange_10:(lUAA+I(lUAA^2))+(lUAA+I(lUAA^2)),data=fitData,na.action=na.fail,control = glm.control(maxit=500),weights = fitData$weights)

#jamGlm_density=glm.nb(round(jamsPerKm)~isManaged+mean_latRange_10:(lUAA+I(lUAA^2))+(lUAA+I(lUAA^2)),data=fitData,na.action=na.fail,control = glm.control(maxit=500),weights = fitData$weights)

#this one has mgmt effect
jamGlm=glm.nb(jamCount~isManaged+mean_minLatRange_10+(lUAA+I(lUAA^2))+offset(log(channelLength)),data=fitData,na.action=na.fail,control = glm.control(maxit=500))

jamGlm=glm.nb(jamCount~(lUAA+I(lUAA^2))+offset(log(channelLength)),data=fitData,na.action=na.fail,control = glm.control(maxit=500))

jamGlm=glm.nb(jamCount~mean_latRange_10+lUAA+I(lUAA^2)+mean_elevation:(lUAA+I(lUAA^2))+offset(log(channelLength)),data=fitData,na.action=na.fail,control = glm.control(maxit=500))


hist(jamGlm$fitted.values)

summary(jamGlm)
dredge(jamGlm,extra="R^2",m.lim=c(2,7))
plot(jamGlm$fitted.values~fitData$jamsPerKm)
abline(a=0,b=1)


hist(jamGlm$fitted.values[fitData$isManaged]) 
hist(fitData$jamsPerKm[fitData$isManaged])

plot(jamGlm$fitted.values,exp(predict.glm(jamGlm,newdata = fitData)))

hist(fitData$jamCount)
hist(jamGlm$fitted.values)


#########verify offset does what I think it does ----------
fittedDF_1km=data.frame(lUAA=fitData$lUAA,
                        mean_latRange_10=fitData$mean_latRange_10,
                        mean_slope=fitData$mean_slope,
                        mean_elevation=fitData$mean_elevation,
                        #channelLength=fitData$channelLength,
                        channelLength=1,
                        isManaged=F)

fittedDF=data.frame(lUAA=fitData$lUAA,
                    mean_latRange_10=fitData$mean_latRange_10,
                    mean_slope=fitData$mean_slope,
                    mean_elevation=fitData$mean_elevation,
                    channelLength=fitData$channelLength,
                    #channelLength=1,
                    isManaged=F)

hist(exp(predict.glm(jamGlm,newdata = fittedDF)))
hist(fitData$jamsPerKm)

resultDF=data.frame(predictedJamsPerKm=exp(predict.glm(jamGlm,newdata = fittedDF_1km)),
                    predictedJamCount=exp(predict.glm(jamGlm,newdata = fittedDF)),
                    observedJamsPerKm=fitData$jamsPerKm,
                    observedJamCount=fitData$jamCount,
                    observedChannelLength=fitData$channelLength)
plot(resultDF$predictedJamCount,cex=resultDF$observedChannelLength,resultDF$predictedJamsPerKm)
plot(resultDF$predictedJamCount/resultDF$observedChannelLength,resultDF$predictedJamsPerKm)

#jam density data is fitted to less precise data, resulting in slightly different fits
plot(exp(predict(jamGlm_density)),exp(predict(jamGlm)),cex=fitData$channelLength)
plot(exp(predict(jamGlm_density)),exp(predict(jamGlm))/fitData$channelLength)

######predict for whole NSV--------------
segData=dataByBatch(6)[,c("locationIDX","latRange_10","elevation","slope","SPI","UAA")]
nsv_locations=dbGetQuery(leakyDB,"SELECT locationIDX FROM Locations WHERE Locations.watershedID = 'NSV_def'")
segData=inner_join(nsv_locations,segData)

predictDF_mgd=data.frame(lUAA=log(segData$UAA),
                         mean_latRange_10=segData$latRange_10,
                         mean_slope=segData$slope,
                         mean_elevation=segData$elevation,
                         channelLength=1,
                         isManaged=T)

predictDF_nmgd=data.frame(lUAA=log(segData$UAA),
                          mean_latRange_10=segData$latRange_10,
                          mean_slope=segData$slope,
                          mean_elevation=segData$elevation,
                          channelLength=1,
                          isManaged=F)


segDataPredict=data.frame(locationIDX=segData$locationIDX,
                          jamsPerKm_mgd=pmin(50,exp(predict.glm(jamGlm,newdata=predictDF_mgd))),
                          jamsPerKm_nmgd=pmin(50,exp(predict.glm(jamGlm,newdata=predictDF_nmgd))))

hist(segDataPredict$jamsPerKm_nmgd)
hist(segDataPredict$jamsPerKm_mgd)

plot(segDataPredict$jamsPerKm_mgd~segData$UAA)

