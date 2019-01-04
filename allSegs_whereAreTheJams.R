library(RSQLite)
library(reshape2)
library(dplyr)
library(PerformanceAnalytics)
library(MuMIn)
source('~/R/projects/mapRespAndLogs/dataByBatch.R')

leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
dbGetQuery(leakyDB,"SELECT * FROM Batches")
dbGetQuery(leakyDB,"SELECT * FROM DataTypes")


segData=dataByBatch(6)[,c("locationIDX","jamsPerKm","elevation","elevRange_25","latRange_25","latRange_50","slope","SPI","UAA")]

#get / match some categorical data
segDataCats=dataByBatch(6,meow=T)
#i'm going to use my dem-derived 'confinement' term, but I need something for land use / mgmt
segDataCats=segDataCats[segDataCats$dataTypeIDX==22,]
segDataCats=left_join(segDataCats,dataByBatch(6)[,c("locationIDX","standAge")])
boxplot(segDataCats$standAge~segDataCats$value,ylim=c(0,500))
#use 100 year stand age as cutoff for old/young
#all nsv is unmanaged


#model jam density
jamData=segData[complete.cases(segData),]
jaModel=lm(jamData$jamsPerKm~jamData$elevation+jamData$latRange_50+jamData$slope+jamData$SPI+jamData$UAA,na.action=na.fail)
summary(jaModel)
dredge(jaModel)



#consider non-linear relationships
#might be best to fit uaa and slope seperatly, w/ possible interaction, rather than use SPI?
hist(log(jamData$UAA))

plot(jamData$jamsPerKm~log(jamData$UAA))
u=glm(jamsPerKm~poly(log(UAA),2)+latRange_25, data=jamData,na.action=na.fail)

u=glm(jamsPerKm~poly(log(UAA),2)+poly(log(SPI),2)+slope + latRange_25, data=jamData,na.action=na.fail)

summary(u)
dredge(u,extra="R^2")
points(predict(u)~log(jamData$UAA), pch="*")



plot(jamData$jamsPerKm~sqrt(jamData$UAA))

plot(jamData$jamsPerKm~sqrt(jamData$slope))

plot(jamData$jamsPerKm~sqrt(jamData$SPI))

plot(jamData$jamsPerKm~jamData$elevation)

plot(jamData$jamsPerKm~jamData$latRange_25)
plot(jamData$jamsPerKm~jamData$latRange_50)
plot(jamData$jamsPerKm~jamData$elevRange_25)


############# play with high degree polynomials##########
plot(jamData$jamsPerKm~jamData$UAA)
pj=glm(jamsPerKm~poly(UAA,2), data=jamData,na.action=na.fail)
summary(pj)
dredge(pj)
points(1:200,predict(object=pj,newdata=data.frame(UAA=1:200)), pch="*")

