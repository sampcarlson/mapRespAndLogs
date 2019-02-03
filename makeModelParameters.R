#set min and max er, consider units.....

sampleRespFromProbs=function(denseJamProbs,minER=50){
  source('~/R/projects/mapRespAndLogs/dataByBatch.R')
  #define metabModel
  metabData=dataByBatch(3,excludeDataTypeIDXs = c(39,60))
  metabModel=lm(-ER~mean_jamsPerKm,data=metabData)
  
  #denseJam distribution
  jamDist=dataByBatch(6)[,c("jamsPerKm")]
  jamDist=jamDist[complete.cases(jamDist)]
  denseJamDist=jamDist[jamDist>5]
  sparseJamDist=jamDist[jamDist<5]
  
  sampleRespFromProb=function(denseJamProb,min=minER){
    if(denseJamProb>runif(1)){
      #dense jams are here - sample from jam dist & calc resp
      jamDensity=base::sample(denseJamDist,1)
    } else {
      #dense jams are not here - sample around sparse jam dist
      jamDensity=base::sample(sparseJamDist,1)
    }
    
    ER=max(min,rnorm(1,mean=predict(metabModel,newdata = data.frame(mean_jamsPerKm=jamDensity)),
                       sd=sigma(metabModel)))
    return(ER)
  }
  
  ERs=sapply(denseJamProbs,sampleRespFromProb)
  
  return(ERs)
}

segData=read.csv("segs_width_jamHabitat.csv")[,c("locationIDX","denseJamProbability","width")]

segData$resp=sampleRespFromProbs(segData$denseJamProbability)
#fucking fuck units are wrong - need to know neo model units

