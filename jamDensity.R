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

wlData=dataByBatch(4)[,c("locationIDX","jamsPerKm","med_elevation","med_latRange_10","med_latRange_25","med_minLatRange_10","med_minLatRange_25","med_slope","med_SPI","med_UAA","jamCount","channelLength","med_valleyWidth_1","med_slope_25")]
wbData=dataByBatch(5)[,c("locationIDX","jamsPerKm","med_elevation","med_latRange_10","med_latRange_25","med_minLatRange_10","med_minLatRange_25","med_slope","med_SPI","med_UAA","jamCount","channelLength","med_valleyWidth_1","med_slope_25")]
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
#jamData=inner_join(jamData,nsv_locations)

fitData=jamData[complete.cases(jamData),]


##########confinement#########
#10 is good
boxplot(fitData$med_latRange_10~fitData$confinement)
boxplot(fitData$med_latRange_25~fitData$confinement)

boxplot(fitData$med_minLatRange_10~fitData$confinement)
boxplot(fitData$med_minLatRange_25~fitData$confinement)

boxplot(fitData$med_valleyWidth_1~fitData$confinement)

boxplot(fitData$med_slope_25~fitData$confinement)

plot(fitData$med_latRange_10,fitData$med_slope_25)

fitData$isUnconf=fitData$confinement=="U"
fitData$isConf=fitData$confinement=="C"

boxplot(fitData$med_slope_25~fitData$isUnconf)

windows()
chart.Correlation(data.frame(conf=as.numeric(fitData$confinement=="C"),
                l10=fitData$med_latRange_10,ml10=fitData$med_minLatRange_10,
                l25=fitData$med_latRange_25,ml25=fitData$med_minLatRange_25,
                vw=fitData$med_valleyWidth_1,
                s25=fitData$med_slope_25,
                sl=fitData$med_slope,
                jpk=fitData$jamsPerKm))

plot(fitData$jamsPerKm~fitData$med_minLatRange_25,pch=1+as.numeric(fitData$isManaged))
points(3,40,pch="|")
points(3,35,pch="|")
points(3,30,pch="|")

plot(fitData$jamsPerKm~fitData$med_latRange_10,pch=1+as.numeric(fitData$isManaged))
points(1.1,40,pch="|")
points(1,35,pch="|")
points(1,30,pch="|")


fitData$isConf3=fitData$med_minLatRange_25>3
fitData$isConf5=fitData$med_minLatRange_25>5
boxplot(fitData$jamsPerKm~fitData$isConf5)

###############jam density###############
hist(fitData$jamsPerKm)
boxplot(fitData$jamsPerKm~fitData$isManaged)

fitData$lUAA=log(fitData$med_UAA)
fitData$channelLength=fitData$channelLength/1000 # convert to km for jams/km offset
reds=brewer.pal(9,"Reds")
latRange_colors=reds[2+cut(fitData$med_minLatRange_25,7,labels=F)]
plot(fitData$jamsPerKm~fitData$lUAA,pch=as.numeric(fitData$isManaged)+16,cex=0.25+cut(fitData$med_slope,5,labels=F)/3,col=latRange_colors)

plot(fitData$jamsPerKm[!fitData$isManaged]~fitData$med_latRange_10[!fitData$isManaged])

plot(fitData$med_elevation~fitData$lUAA)
plot(fitData$jamsPerKm~fitData$med_slope_25)
plot(fitData$jamsPerKm~fitData$med_latRange_10)

plot(fitData$jamsPerKm~fitData$med_minLatRange_25)
plot(fitData$jamsPerKm~fitData$med_slope)
plot(fitData$jamsPerKm~fitData$med_valleyWidth_1)
plot(fitData$jamsPerKm~fitData$med_SPI)


plot(fitData$med_minLatRange_25,fitData$med_valleyWidth_1)



#t.test(fitData$isUnconf~fitData$med_valleyWidth_1>20)
#fitData$isUnconf=fitData$med_latRange_10>2



jamGlm=glm.nb(jamCount~isManaged+(lUAA+I(lUAA^2))+as.numeric(!isManaged):med_latRange_10+offset(log(channelLength)),data=fitData,na.action=na.fail)
#463.07

#
#jamGlm=glm.nb(jamCount~isManaged*(med_slope+med_latRange_25+(lUAA+I(lUAA^2)))^2+offset(log(channelLength)),data=fitData,na.action=na.fail)


#numbers relate to ellen email:
#1
#jamGlm=glm.nb(jamCount~isManaged*med_latRange_10+(lUAA+I(lUAA^2))+offset(log(channelLength)),data=fitData,na.action=na.fail,control = glm.control(maxit=500))
#3.4, aic:463.7, r2:.53

#2
#jamGlm=glm.nb(jamCount~med_latRange_10+ lUAA + I(lUAA^2)+isManaged:(lUAA + I(lUAA^2))+med_slope+med_slope:lUAA +offset(log(channelLength)),data=fitData,na.action=na.fail,control = glm.control(maxit=500))
#3.1, aic:463.3, r2:.56

#3
#jamGlm=glm.nb(jamCount~med_latRange_10  + lUAA + I(lUAA^2)+isManaged:(med_latRange_10+ lUAA + I(lUAA^2))+med_slope+med_slope:lUAA +offset(log(channelLength)),data=fitData,na.action=na.fail,control = glm.control(maxit=500))
#3.0, aic: 450.8, r2:.626


summary(jamGlm)
getJamEffectDiff(jamGlm)

dredge(jamGlm,extra=c("getJamEffectDiff","R^2","maxP"),m.lim=c(2,10))
plot(jamGlm$fitted.values~fitData$jamsPerKm)
abline(a=0,b=1)


hist(jamGlm$fitted.values[fitData$isManaged]) 
hist(fitData$jamsPerKm[fitData$isManaged])

plot(jamGlm$fitted.values,exp(predict.glm(jamGlm,newdata = fitData)))

hist(fitData$jamCount)
hist(jamGlm$fitted.values)


#########verify offset does what I think it does ----------
fittedDF_1km=data.frame(lUAA=fitData$lUAA,
                        med_latRange_10=fitData$med_latRange_10,
                        med_slope=fitData$med_slope,
                        med_elevation=fitData$med_elevation,
                        #channelLength=fitData$channelLength,
                        channelLength=1,
                        isManaged=F)

fittedDF=data.frame(lUAA=fitData$lUAA,
                    med_latRange_10=fitData$med_latRange_10,
                    med_slope=fitData$med_slope,
                    med_elevation=fitData$med_elevation,
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
#plot(exp(predict(jamGlm_density)),exp(predict(jamGlm)),cex=fitData$channelLength)
#plot(exp(predict(jamGlm_density)),exp(predict(jamGlm))/fitData$channelLength)

######predict for whole NSV--------------
segData=dataByBatch(6)[,c("locationIDX","latRange_10","elevation","slope","SPI","UAA")]
nsv_locations=dbGetQuery(leakyDB,"SELECT locationIDX FROM Locations WHERE Locations.watershedID = 'NSV_def'")
segData=inner_join(nsv_locations,segData)

predictDF_mgd=data.frame(lUAA=log(segData$UAA),
                         med_latRange_10=segData$latRange_10,
                         med_slope=segData$slope,
                         med_elevation=segData$elevation,
                         channelLength=1,
                         isManaged=T)

predictDF_nmgd=data.frame(lUAA=log(segData$UAA),
                          med_latRange_10=segData$latRange_10,
                          med_slope=segData$slope,
                          med_elevation=segData$elevation,
                          channelLength=1,
                          isManaged=F)


segDataPredict=data.frame(locationIDX=segData$locationIDX,
                          jamsPerKm_mgd=pmin(50,exp(predict.glm(jamGlm,newdata=predictDF_mgd))),
                          jamsPerKm_nmgd=pmin(50,exp(predict.glm(jamGlm,newdata=predictDF_nmgd))))

hist(segDataPredict$jamsPerKm_nmgd)
hist(segDataPredict$jamsPerKm_mgd)
hist(segDataPredict$jamsPerKm_nmgd-segDataPredict$jamsPerKm_mgd)
# + means management decreases jam density
mean(segDataPredict$jamsPerKm_nmgd-segDataPredict$jamsPerKm_mgd,na.rm = T)


plot(segDataPredict$jamsPerKm_nmgd-segDataPredict$jamsPerKm_mgd~predictDF_mgd$lUAA)


plot(segDataPredict$jamsPerKm_mgd~segData$UAA)

write.csv(segDataPredict,"segDataPredict.csv")
write.csv(segData,"segData.csv")