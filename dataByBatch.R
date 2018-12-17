
dataByBatch=function(batchID, combineByMetricName=T,meow=F, excludeDataTypeIDXs=NULL){
  library(RSQLite)
  library(reshape2)

  leakyDB=dbConnect(SQLite(),"C:/Users/sam/Documents/LeakyRivers/Data/sqLiteDatabase/LeakyDB.db")
  
  locations=dbGetQuery(leakyDB,paste0("SELECT DISTINCT Locations.locationIDX FROM Locations LEFT JOIN Data ON Locations.locationIDX = Data.locationIDX
                                      WHERE Data.batchIDX = ",batchID))$locationIDX
  
  longData=dbGetQuery(leakyDB,paste0("SELECT Data.locationIDX, DataTypes.metric, DataTypes.unit, DataTypes.dataTypeIDX, Data.value FROM Data
                                     LEFT JOIN DataTypes ON Data.dataTypeIDX = DataTypes.DataTypeIDX
                                     WHERE locationIDX IN (",paste(locations,collapse=", "),")"))
  dbDisconnect(leakyDB)
  if(!is.null(excludeDataTypeIDXs)){
    longData=longData[!(longData$dataTypeIDX %in% excludeDataTypeIDXs),]
  }
  
  longCats=longData[longData$unit=="categorical",]
  longNumeric=longData[longData$unit!="categorical",]
  longNumeric$value=as.numeric(as.character(longNumeric$value))
  if(combineByMetricName){
    wideNumeric=dcast(longNumeric,locationIDX~metric,fun.aggregate = mean)
    
  } else{
    wideNumeric=dcast(longNumeric,locationIDX~dataTypeIDX,fun.aggregate = mean)
  }
  
  if(meow){
    return(longCats)
  } else {
    return(wideNumeric)
  }
}
