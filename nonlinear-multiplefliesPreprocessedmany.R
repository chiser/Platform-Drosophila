setwd("C:/Users/Students/Desktop/downsampling christian")
require("nonlinearTseries")
require("beepr")
#filename = id_table1[i]
Prediction<- NULL
Prediction2<- NULL
Params<-NULL
Tests<-NULL
Observed<-NULL
Observed2<-NULL

# esto se va a incluir en futuras versiones del paquete, es util para utilizar
# vectores en el argumento prediction.step... recomendado!
nonLinearPredict = Vectorize(nonLinearPrediction, vectorize.args = "prediction.step")
nonLinearPredict2 = Vectorize(nonLinearPredict, vectorize.args = "radius")

message("Enter the prefix for the saved files")
Textname<- scan(n=1,what= character())

data = read.table(file.choose(), header = TRUE, sep = "\t", quote = "\"" ,
                   dec = ".", fill = TRUE, comment.char = "")
                   
data = na.omit(data)
diffdata<-paste("diff",colnames(data))
each<-NULL
differenced<-NULL
for (i in 1:length(diffdata)){
  
  each<-diff(data[,i])
  differenced<- cbind(differenced,each)
      
}

colnames(differenced)<-diffdata

for (i in 1:length(diffdata)){
  
use.ts = differenced[,i]
#Sampling<- diff(data$t.s.)
### For modelling a white noise ts with same SDev
#SDev<-sd(Sampling)

#ts2approx<-cbind(data$t.s.,data$diff)
#interpolated<-approx(ts2approx[,1],ts2approx[,2],xout=seq(1,data$t.s.[length(data$t.s.)],30))
#use.ts<- interpolated$y
#plot(use.ts,type="l")


#use.ts = rnorm(length(data$diff), mean=0, sd=SDev)


### For modelling a fully deterministic time series-----------
#x<- sin(seq(1,length(data$diff), by=1/8))
#plot(x,type="l")
#xs <- seq(-2*pi,2*pi,pi/1500)
#wave.1 <- sin(30*xs)
#wave.2 <- sin(100*xs)
#use.ts <- 0.5 * wave.1 + 0.25 * wave.2
#plot(xs,use.ts,type="l",col="red",ylim=c(-1,1)); abline(h=0,lty=3)

# nonlinear testing --------------------------------------------------------
# Idealmente lo primero que deberiamos hacer es testear si la serie temporal
# que queremos analizar tiene caracteristicas no-lineales. Lo cierto es que
# es un tema complicado. Para iniciarte puedes echarle un vistazo a la viñeta
# y leer el articulo
# "Testing for nonlinearity in time series: the method of surrogate data" de
# Theiler. En este script nos lo saltamos

#output<- paste(id_table1[i],".pdf", sep="")
#pdf(file= output)

# time lag selection ------------------------------------------------------
# Aqui preguntas para que es n.partitions. Para calcular la Average Mutual
# Information es necesario construir un histograma de la serie temporal
# echale un vistazo a la ayuda de mutualInformation). n.partitions es el
# numero de bins que usamos para construir este histograma


LAG <- timeLag(use.ts, technique = "ami",selection.method = "first.e.decay",lag.max = 20, do.plot = TRUE)


# selecting the embedding dim ---------------------------------------------

emb.dim = estimateEmbeddingDim(use.ts, threshold = 0.95,
                               time.lag = LAG, max.embedding.dim = 25)

# theiler window selection ------------------------------------------------
# el plot puede interpretarse como sigue: cada uno de las lineas indica
# la distancia que tienes que moverte  en el espacio de fases (epsilon)
# para encontrar un porcentaje concreto de vecinos dependiento de la separacion
# temporal. Para distancias temporales pequeñas se suelen encontrar 
# valores de radio pequeños debido a correlaciones temporales. Dado que
# queremos evitar estas correlaciones temporales, debemos elegir como valor
# para la theiler window un valor temporal en el que las lineas de contorno saturen.
# En este caso podemos elegir un valor de 5 para la theiler window. 
spaceTime = spaceTimePlot(time.series = use.ts,time.lag = LAG,
                          embedding.dim = emb.dim,
                          number.time.steps = 40,type="b")

#message("please enter the theiler window according to the graph")
#theilerw = scan(n=1,what= numeric())
theilerw = 30
# correlation dimension ---------------------------------------------------
# Los valores de los radios se seleccionan de forma que C(r) vaya de valores 
# pequeños (por ejemplo, 1e-5) hasta 1. El orden de magnitud de los radios
# se puede estimar observando la time series. La serie diferencia tiene un
# rango de -1 a 1 (approx). Asi pues, un valor adecuado para el radio maximo 
# sera del orden de magnitud de 1
corr.dim =  corrDim(use.ts, min.embedding.dim = emb.dim, 
                    max.embedding.dim = emb.dim + 7,
                    time.lag = LAG,
                    min.radius = 0.04, max.radius = (max(use.ts)-min(use.ts))/2, 
                    n.points.radius = 45, 
                    theiler.window = theilerw,
                    do.plot = TRUE)

message("please enter the parameters for the estimation of the correlation dimension")## TODO: take a corr.dim$corr.matrix and find the max slope and so it is automatically
regression.range.min = 0.05                         #scan(n=1,what= numeric())
message("please enter the parameters for the estimation of the correlation dimension")
regression.range.max =  0.1                        #scan(n=1,what= numeric())
message("please enter the parameters for the estimation of the correlation dimension")
use.embeddings.min = corr.dim$embedding.dims[5]
message("please enter the parameters for the estimation of the correlation dimension")
use.embeddings.max = corr.dim$embedding.dims[8]

#output<- paste(diffdata[i],".pdf", sep="")
#pdf(file= output)
# Basandonos en el grafico anterior podemos ver un pequeño plateau para
# los valores comprendidos entre 0.1 y 0.3 (aproximadamente) para los
# valores mas altos de la embedding dim. Obtenemos la estimacion final
# especificando estos valores en la funcion estimate
#plot(Sampling)
D<- estimate(corr.dim,regression.range = c(regression.range.min,regression.range.max),use.embeddings = use.embeddings.min:use.embeddings.max,do.plot = T)


#While
#a non integer saturating correlation dimension is
#usually indicative of chaos in the system, it may
#be  noted  that  there  are  a  certain  class  of  deter-
#  ministic non linear systems which are strange and
#non-chaotic, i.e.  they exhibit fractal nature but do
#not exhibit chaos

# information dimension ---------------------------------------------------
# La information dimension es una variacion de la correlation dimension
# que se puede relacionar con el concepto de entropia (en el sentido de 
# informacion). La estimacion es similar a la correlation dimension


# lyapunov ----------------------------------------------------------------
# La estimacion sigue el mismo procedimiento que la correlation dimension. Puedes
# consultar la vi;eta para un ejemplo.


# recurrence plot ---------------------------------------------------------
# Si ves un bloque negro en la recurrence plot es porque has seleccionado
# un radio demasiado elevado. Nos interesan radios no muy grandes para obtener
# una matriz de recurrencia dispersa.Yo emplearia rqa en vez de recurrence plot
# ya que produce la misma figura y nos da una cuantificacion de  la matriz
# de recurrencia. Para entender que significan los valores
# obtenidos con el RQA http://www.recurrence-plot.tk/rqa.php 

#message("please enter the radius for the recurrence plot")
#r = scan(n=1,what= numeric())


#rqa.res = rqa(time.series=use.ts,
#              embedding.dim = emb.dim,time.lag=LAG,
#              radius = r)
#plot(rqa.res)

# dfa ---------------------------------------------------------------------
# Este no es un metodo no lineal en si mismo, si no un metodo de analisis
# fractal. Esta incluido en el paquete porque los metodos fractales 
# se emplean mucho para caracterizar series complejas (como las no lineales)



######################## Lo que yo anado



detrend = dfa(use.ts, 
              window.size.range = c(10, 400),
              npoints = 20,do.plot=TRUE)     
detr.fluct<-estimate(detrend,regression.range = c(30,300))
# un valor cercano a 0.5 indica que, en el caso de modelar la serie como un
# proceso puramente aleatorio, un modelo adecuado podria ser ruido blanco.
ml<-maxLyapunov(use.ts, min.embedding.dim = 2,                                        #from nonlinearTseries
                max.embedding.dim = emb.dim+4, time.lag = LAG, radius=1,
                theiler.window = theilerw, min.neighs = 5, min.ref.points = 500,
                max.time.steps = 10,
                do.plot = TRUE)

plot(ml)
ml.estimation = estimate(ml,regression.range = c(0,15),
                         use.embeddings=2:emb.dim+4,
                         do.plot = TRUE)

#surrogate data test

st = surrogateTest(use.ts,significance = 0.05,one.sided = F,
                   FUN = timeAsymmetry, do.plot=T)

#sample entropy

se = sampleEntropy(corr.dim, do.plot = T)
se.est = estimate(se, do.plot = T,
                  regression.range = c(0,2))
EstimatedEntropy<- mean(se.est)

## prediction



for (oo in 1:8){
beep(1)
message("please enter the radius")
radius = scan(n=3,what= numeric())

# obtenemos predicciones para las proximas 500 (prediction.step) muestras basandonos
#en las 4000 primeras muestras de x
prediction = nonLinearPredict2 (use.ts[1:round(length(use.ts)/2)], embedding.dim = emb.dim, time.lag = LAG, radius = c(0.0005,radius),
                                radius.increment = 0.001, prediction.step = 1:40)
# comparamos... Fijate como la prediccion va empeorando a medida que pasa el tiempo. Esto
# es tipico de sistemas no lineales

plot(use.ts[(round(length(use.ts)/2)+1):(round(length(use.ts)/2)+40)], type = "l", ylim = range(use.ts[(round(length(use.ts)/2)+1):(round(length(use.ts)/2)+40)], Prediction))
lines(prediction[,1], type = "l", col = 2)
lines(prediction[,2], type = "l", col = 3)
lines(prediction[,3], type = "l", col = 4)
lines(prediction[,4], type = "l", col = 5)

beep(2)
message("Is the prediction fine?y/n")
PredictionAnswer = scan(n=1,what= character())

if(PredictionAnswer== "y") break

}

for (oo in 1:8){
  beep(1)
  message("please enter the radius")
  radius2 = scan(n=3,what= numeric())
  
  # obtenemos predicciones para las proximas 500 (prediction.step) muestras basandonos
  #en las 4000 primeras muestras de x
  prediction2 = nonLinearPredict2 (use.ts[1:(length(use.ts)-2000)], embedding.dim = emb.dim, time.lag = LAG, radius = c(0.0005,radius2),
                                  radius.increment = 0.001, prediction.step = 1:40)
  # comparamos... Fijate como la prediccion va empeorando a medida que pasa el tiempo. Esto
  # es tipico de sistemas no lineales
  
  plot(use.ts[((length(use.ts)-2000)+1):((length(use.ts)-2000)+40)], type = "l", ylim = range(use.ts[((length(use.ts)-2000)+1):((length(use.ts)-2000)+40)], Prediction2))
  lines(prediction[,1], type = "l", col = 2)
  lines(prediction[,2], type = "l", col = 3)
  lines(prediction[,3], type = "l", col = 4)
  lines(prediction[,4], type = "l", col = 5)
  
  beep(2)
  message("Is the prediction fine?y/n")
  PredictionAnswer = scan(n=1,what= character())
  
  if(PredictionAnswer== "y") break
  
}



manytests<-nonlinearityTest (time.series=use.ts, verbose = TRUE)


#denoised<-nonLinearNoiseReduction(time.series=use.ts, embedding.dim=emb.dim, radius=0.1)
#plot(denoised,type="l")

#print(D,rqa.res,mean(se.est),ml.estimation)   


#dev.off()

Observed<-c(Observed,use.ts[(round(length(use.ts)/2)+1):(round(length(use.ts)/2)+40)])
Observed2<-c(Observed2,use.ts[((length(use.ts)-2000)+1):((length(use.ts)-2000)+40)])
Prediction<- cbind(Prediction,prediction)
Prediction2<- cbind(Prediction2,prediction2)


ifelse(i==1, Params <- data.frame("Fly name"=diffdata[i], "Corr Dim"=D,"Lyapunov Exp"=ml.estimation,"Sample entropy estimate"=EstimatedEntropy,"detrend fluctuation analysis"=detr.fluct, "Surrogate statistic"=st$data.statistic,
                                  "Min Regr. range"= regression.range.min , "Max Regr. range"= regression.range.max , "Min dim regression"= use.embeddings.min, "Max dim regression"=use.embeddings.max,"TheilerWind"= theilerw,"Prediction radius"= radius, "Prediction radius2"= radius2)
       , Params[i,1:ncol(Params)] <- c(diffdata[i], D,ml.estimation,EstimatedEntropy,detr.fluct,st$data.statistic, regression.range.min , regression.range.max , use.embeddings.min, use.embeddings.max, theilerw, radius,radius2))

ifelse(i==1, Tests<- data.frame("Terasvirta"= manytests$Terasvirta$p.value, "White"= manytests$White$p.value, "Keenan"= manytests$Keenan$p.value, "McLeodLi"= mean(manytests$McLeodLi$p.values), "Tsay"=manytests$Tsay$p.value, "TarTest"= manytests$TarTest$p.value)
       , Tests[i,1:ncol(Tests)] <- c(manytests$Terasvirta$p.value, manytests$White$p.value, manytests$Keenan$p.value, mean(manytests$McLeodLi$p.values), manytests$Tsay$p.value, manytests$TarTest$p.value))

}

write.table(Params, paste(Textname,"Spontaneity.txt",sep=""), sep="\t", row.names = FALSE,col.names = FALSE)
write.table(Prediction, paste(Textname,"Pred.txt",sep=""), sep="\t", row.names = FALSE,col.names = FALSE)
write.table(Prediction2, paste(Textname,"Pred2.txt",sep=""), sep="\t", row.names = FALSE,col.names = FALSE)
write.table(Tests, paste(Textname,"Tests.txt",sep=""), sep="\t", row.names = FALSE,col.names = FALSE)
write.table(Observed, paste(Textname,"Observed.txt",sep=""), sep="\t", row.names = FALSE,col.names = FALSE)
write.table(Observed2, paste(Textname,"Observed2.txt",sep=""), sep="\t", row.names = FALSE,col.names = FALSE)

beep(3)