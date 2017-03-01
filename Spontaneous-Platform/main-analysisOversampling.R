setwd("C:/Users/LocalAdmin/Desktop/")
message("Enter the prefix for the saved files")
Textname<- scan(n=1,what= character())
id_table<-read.table("DownTimeImport.txt", header = FALSE, sep="\t", quote = "\"",dec = " ")
id_table1<- sapply(id_table,function(id_table){paste(id_table,".txt",sep="")})
#id_table<-as.list(id_table1)
Nflies<- length(id_table1)
import<-NULL



flyJoystickDownsampleTime <- function(data, samplePeriod) {
  
  AvgPosition<-NULL
  FlyDataPosition<-NULL
  AvgTime<-NULL
  FlyDataTime<-NULL
  
  
  for (i in seq(0,ceiling(max(data$TimeStamp)),samplePeriod)){
    
    AvgPosition <-mean(data$Position[data$TimeStamp>=i & data$TimeStamp <= (i+samplePeriod)])  
    FlyDataPosition<-c(FlyDataPosition,AvgPosition)
    
    AvgTime <-mean(data$TimeStamp[data$TimeStamp>=i & data$TimeStamp <= (i+samplePeriod)])  
    FlyDataTime<-c(FlyDataTime,AvgTime)
  }
  flyTracesDownTime <- data.frame("Time" = FlyDataTime, "Position" = FlyDataPosition)
  #return(FlyDataTime)
  return(flyTracesDownTime)
}



for (oo in 1:Nflies){
  setwd("C:/Users/LocalAdmin/Desktop/Christian articles/R")
  source("analysisOversampling.r")
  import<- cbind(import,fly)
  
  #ifelse (i==1, Prediction <- prediction, Prediction<- rbind(Prediction,prediction))  
  #ifelse(i==1, Params <- data.frame("Fly name"=filename, "SDev"=SDev, "Corr Dim"=D,"Lyapunov Exp"=ml.estimation,"Sample entropy estimate"=EstimatedEntropy,"detrend fluctuation analysis"=detr.fluct, "Surrogate statistic"=st$data.statistic,
  #                                  "Min Regr. range"= regression.range.min , "Max Regr. range"= regression.range.max , "Min dim regression"= use.embeddings.min, "Max dim regression"=use.embeddings.max,"TheilerWind"= theilerw)
  #       , Params[i,1:ncol(Params)] <- c(filename, SDev, D,ml.estimation,EstimatedEntropy,detr.fluct,st$data.statistic, regression.range.min , regression.range.max , use.embeddings.min, use.embeddings.max, theilerw))
  #ifelse(i==1, Tests<- data.frame("Terasvirta"= manytests$Terasvirta$p.value, "White"= manytests$White$p.value, "Keenan"= manytests$Keenan$p.value, "McLeodLi"= mean(manytests$McLeodLi$p.values), "Tsay"=manytests$Tsay$p.value, "TarTest"= manytests$TarTest$p.value)
  #       , Tests[i,1:ncol(Tests)] <- c(manytests$Terasvirta$p.value, manytests$White$p.value, manytests$Keenan$p.value, mean(manytests$McLeodLi$p.values), manytests$Tsay$p.value, manytests$TarTest$p.value))
  
}

#boxplot (Prediction)



write.table(import, paste(Textname,".txt",sep=""), sep="\t", row.names = FALSE,col.names = FALSE)

#write.table(Prediction, paste(Textname,"Pred.txt",sep=""), sep="\t", row.names = FALSE,col.names = FALSE)
#write.table(Tests, paste(Textname,"Tests.txt",sep=""), sep="\t", row.names = FALSE,col.names = FALSE)
