
jamGlm=glm.nb(jamCount~lUAA+I(lUAA^2)+offset(log(channelLength)),data=fitData,na.action=na.fail)
jamGlm$coefficients

plot(fitData$lUAA,fitData$jamCount)
points(fitData$lUAA,predict.glm(jamGlm,newdata=data.frame(lUAA=fitData$lUAA,channelLength=1),type="response"),pch="*")


fitData$lUAA_m=log(fitData$mean_UAA*(1000^2))
jamGlm_m=glm.nb(jamCount~lUAA_m+I(lUAA_m^2)+offset(log(channelLength)),data=fitData,na.action=na.fail)
jamGlm_m$coefficients

plot(fitData$lUAA_m,fitData$jamCount)
points(fitData$lUAA_m,predict.glm(jamGlm_m,newdata = data.frame(lUAA_m=fitData$lUAA_m,channelLength=1), type="response"),pch="*")

#w/o offset
jamGlm=glm.nb(jamCount~poly(lUAA,2),data=fitData,na.action=na.fail)
jamGlm$coefficients

plot(fitData$lUAA,fitData$jamCount)
points(fitData$lUAA,predict.glm(jamGlm,newdata=data.frame(lUAA=fitData$lUAA),type="response"),pch="*")


fitData$lUAA_m=log(fitData$mean_UAA*(1000^2))
jamGlm_m=glm.nb(jamCount~poly(lUAA_m,2),data=fitData,na.action=na.fail)
jamGlm_m$coefficients

plot(fitData$lUAA_m,fitData$jamCount)
points(fitData$lUAA_m,predict.glm(jamGlm_m,newdata = data.frame(lUAA_m=fitData$lUAA_m), type="response"),pch="*")
points(fitData$lUAA_m,predict(jamGlm_m,type="response"))

#poisson

jamGlm=glm(jamCount~poly(lUAA,2)+offset(log(channelLength)),family = poisson,data=fitData,na.action=na.fail)
jamGlm$coefficients

plot(fitData$lUAA,fitData$jamCount)
points(fitData$lUAA,predict.glm(jamGlm,newdata=data.frame(lUAA=fitData$lUAA,channelLength=1),type="response"),pch="*")


fitData$lUAA_m=log(fitData$mean_UAA*(1000^2))
jamGlm_m=glm(jamCount~poly(lUAA_m,2)+offset(log(channelLength)),family = poisson,data=fitData,na.action=na.fail)
jamGlm_m$coefficients

plot(fitData$lUAA_m,fitData$jamCount)
points(fitData$lUAA_m,predict.glm(jamGlm_m,newdata = data.frame(lUAA_m=fitData$lUAA_m,channelLength=1), type="response"),pch="*")

#sqrt
jamLm=lm(sqrt(jamsPerKm)~lUAA+I(lUAA^2),data=fitData,na.action=na.fail,x=T,y=T)
jamLm$coefficients

plot(fitData$lUAA,fitData$jamCount)
points(fitData$lUAA,predict(jamLm,type="response")^2,pch="*")


jamLm_m=lm(sqrt(jamsPerKm)~lUAA_m+I(lUAA_m^2),data=fitData,na.action=na.fail,x=T,y=T)
jamLm_m$coefficients

plot(fitData$lUAA_m,fitData$jamCount)
points(fitData$lUAA_m,predict(jamLm_m)^2,pch="*")




quad_fun=function(x,i,a,b){
  return(i+a*x+b*(x^2))
}

y=seq(from=-0.5,to=4.5,by=0.1)
plot(y,quad_fun(y,i=2.7,a=1.22,b=-0.44))
points(y,quad_fun(y,i=1.18,a=1.2,b=-0.37),pch="*")

y=seq(from=13,to=19,by=0.1)
plot(y,quad_fun(y,i=-67.7,a=8.87,b=-0.27))
