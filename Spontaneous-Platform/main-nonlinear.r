require("nonlinearTseries")
setwd("C:/Users/LocalAdmin/Desktop/spontaneity poster")
message("Enter the prefix for the saved files")
Textname<- scan(n=1,what= character())
id_table<-read.table("import.txt", header = FALSE, sep="\t", quote = "\"",dec = " ")
id_table1<- sapply(id_table,function(id_table){paste(id_table,".dat",sep="")})
#id_table<-as.list(id_table1)
skip<-36
Nflies<- length(id_table1)
Prediction<- NULL
Params<- data.frame("Fly","SDev","Corr Dim","Lyapunov Exp","Sample entropy estimate","detrend fluctuation analysis","Surrogate statistic","Min Regr. range", "Max Regr. range", "Min dim regression", "Max dim regression","TheilerWind")
for (i in 1:Nflies){
  setwd("C:/Users/LocalAdmin/Desktop/Christian articles/R")
  source("nonlinear-multipleflies.r")
  ifelse (i==1, Prediction <- prediction, Prediction<- rbind(Prediction,prediction))  
  ifelse(i==1, Params <- data.frame("Fly name"=filename, "SDev"=SDev, "Corr Dim"=D,"Lyapunov Exp"=ml.estimation,"Sample entropy estimate"=EstimatedEntropy,"detrend fluctuation analysis"=detr.fluct, "Surrogate statistic"=st$data.statistic,
                                    "Min Regr. range"= regression.range.min , "Max Regr. range"= regression.range.max , "Min dim regression"= use.embeddings.min, "Max dim regression"=use.embeddings.max,"TheilerWind"= theilerw)
                                    , Params[i,1:ncol(Params)] <- c(filename, SDev, D,ml.estimation,EstimatedEntropy,detr.fluct,st$data.statistic, regression.range.min , regression.range.max , use.embeddings.min, use.embeddings.max, theilerw))
  ifelse(i==1, Tests<- data.frame("Terasvirta"= manytests$Terasvirta$p.value, "White"= manytests$White$p.value, "Keenan"= manytests$Keenan$p.value, "McLeodLi"= mean(manytests$McLeodLi$p.values), "Tsay"=manytests$Tsay$p.value, "TarTest"= manytests$TarTest$p.value)
         , Tests[i,1:ncol(Tests)] <- c(manytests$Terasvirta$p.value, manytests$White$p.value, manytests$Keenan$p.value, mean(manytests$McLeodLi$p.values), manytests$Tsay$p.value, manytests$TarTest$p.value))
  
}

#boxplot (Prediction)

write.table(Params, paste(Textname,"Spontaneity.txt",sep=""), sep="\t", row.names = FALSE,col.names = FALSE)
write.table(Prediction, paste(Textname,"Pred.txt",sep=""), sep="\t", row.names = FALSE,col.names = FALSE)
write.table(Tests, paste(Textname,"Tests.txt",sep=""), sep="\t", row.names = FALSE,col.names = FALSE)


#library(xlsx)
#write.xlsx(Params, "Params.xlsx") 

#recurrencePlot(time.series=use.ts, embedding.dim=emb.dim, time.lag=LAG, radius=0.2)

#Predicted<- read.table("InterpPred.txt", sep="\t", dec = ".")

#corr<-NULL
#correl<-NULL

#for (i in 1:(length(Corr[,1])/2)){
#  for (oo in seq(1,2000,100)){
#    correl<-cor(Corr[i,oo:(oo+99)],Corr[(i+(length(Corr[,1])/2)),oo:(oo+99)])
#    corr<-c(corr,correl)
#  }
#}
#cor.table<- matrix(corr,nrow = 20,ncol = 6)

#TWO<- apply(cor.table,1,mean)

#plot(TWO,type="l",lwd=2.5, xlab="Time steps(x100)", ylab="Correlation coefficient")