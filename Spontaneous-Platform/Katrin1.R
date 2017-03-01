setwd("C:/Users/LocalAdmin/Desktop/")
message("Enter the prefix for the saved files")
Textname<- scan(n=1,what= character())
id_table<-read.table("DownTimeImport.txt", header = FALSE, sep="\t", quote = "\"",dec = " ")
id_table1<- sapply(id_table,function(id_table){paste(id_table,".txt",sep="")})
#id_table<-as.list(id_table1)
Nflies<- length(id_table1)
import<-NULL



flyJoystickDownsampleTime <- function(data, samplePeriod) {
  
  AvgPosition1<-NULL
  FlyDataPosition1<-NULL
  AvgPosition2<-NULL
  FlyDataPosition2<-NULL
  AvgPosition3<-NULL
  FlyDataPosition3<-NULL
  AvgTime<-NULL
  FlyDataTime<-NULL
  
  
  for (i in seq(0,ceiling(max(data$TimeStamp)),samplePeriod)){
    
    AvgPosition1 <-mean(data$Position1[data$TimeStamp>=i & data$TimeStamp <= (i+samplePeriod)])  
    FlyDataPosition1<-c(FlyDataPosition1,AvgPosition1)
    
    AvgPosition2 <-mean(data$Position2[data$TimeStamp>=i & data$TimeStamp <= (i+samplePeriod)])  
    FlyDataPosition2<-c(FlyDataPosition2,AvgPosition2)
    
    AvgPosition3 <-mean(data$Position3[data$TimeStamp>=i & data$TimeStamp <= (i+samplePeriod)])  
    FlyDataPosition3<-c(FlyDataPosition3,AvgPosition3)
    
    AvgTime <-mean(data$TimeStamp[data$TimeStamp>=i & data$TimeStamp <= (i+samplePeriod)])  
    FlyDataTime<-c(FlyDataTime,AvgTime)
  }
  flyTracesDownTime <- data.frame("Time" = FlyDataTime, "Position1" = FlyDataPosition1, "Position2" = FlyDataPosition2, "Position3" = FlyDataPosition3)
  #return(FlyDataTime)
  return(flyTracesDownTime)
}



for (oo in 1:Nflies){
  setwd("C:/Users/LocalAdmin/Desktop/Christian articles/R")
  source("analysisOversampling2.r")
  import<- cbind(import,fly)
  
  #ifelse (i==1, Prediction <- prediction, Prediction<- rbind(Prediction,prediction))  
  #ifelse(i==1, Params <- data.frame("Fly name"=filename, "SDev"=SDev, "Corr Dim"=D,"Lyapunov Exp"=ml.estimation,"Sample entropy estimate"=EstimatedEntropy,"detrend fluctuation analysis"=detr.fluct, "Surrogate statistic"=st$data.statistic,
  #                                  "Min Regr. range"= regression.range.min , "Max Regr. range"= regression.range.max , "Min dim regression"= use.embeddings.min, "Max dim regression"=use.embeddings.max,"TheilerWind"= theilerw)
  #       , Params[i,1:ncol(Params)] <- c(filename, SDev, D,ml.estimation,EstimatedEntropy,detr.fluct,st$data.statistic, regression.range.min , regression.range.max , use.embeddings.min, use.embeddings.max, theilerw))
  #ifelse(i==1, Tests<- data.frame("Terasvirta"= manytests$Terasvirta$p.value, "White"= manytests$White$p.value, "Keenan"= manytests$Keenan$p.value, "McLeodLi"= mean(manytests$McLeodLi$p.values), "Tsay"=manytests$Tsay$p.value, "TarTest"= manytests$TarTest$p.value)
  #       , Tests[i,1:ncol(Tests)] <- c(manytests$Terasvirta$p.value, manytests$White$p.value, manytests$Keenan$p.value, mean(manytests$McLeodLi$p.values), manytests$Tsay$p.value, manytests$TarTest$p.value))
  
}

#boxplot (Prediction)
import<-na.omit(import)
spec<-spectrum(import)
#colnames(import)<-c("plat1.1","plat2.5","plat3.3","plat1.3","plat2.1","plat3.5","plat1.5","plat2.3","plat3.1","plat1.6","plat2.7","plat3.8","plat1.10","plat2.12","plat3.8","plat1.fly2","plat2.nothing","plat3.flyX","plat1.flyX","plat2.nothing","plat3.fly2")

setwd("C:/Users/LocalAdmin/Desktop/StreamingPlatform")

write.table(import, paste(Textname,".txt",sep=""), sep="\t", row.names = FALSE,col.names = TRUE)

#plot(import[,19],type="l")
#plot(import[,19],type="l")
#lines(data[,1],type="l")
#plot(data[,1],type="l")
#spectrum(data[,1:2])
#write.table(Prediction, paste(Textname,"Pred.txt",sep=""), sep="\t", row.names = FALSE,col.names = FALSE)
#write.table(Tests, paste(Textname,"Tests.txt",sep=""), sep="\t", row.names = FALSE,col.names = FALSE)