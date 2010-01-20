load("APS3modelSH.rData")

Actual<-APS3modelSH$y
Predicted<-APS3modelSH$fitted.values
PredictedDummy<-ifelse(APS3modelSH$fitted.values>0.5, 1, 0)
Data<-APS3modelSH$data

APS3table<-cbind(Actual, Predicted, Data)

newsummary<-ddply(APS3byLA, .(la), summarise, predicted=sum())