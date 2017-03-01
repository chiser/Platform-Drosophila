require("nonlinearTseries")
setwd("C:/Users/LocalAdmin/Desktop/platform/altehaken-intensitytests")

filename = "Gr66a_1_m_ohne_15lx.dat"
skip = 36
data = read.table(filename, header = FALSE, sep = "\t", quote = "\"" ,
                   dec = ".", fill = TRUE, skip = skip , 
                   comment.char = "", nrows = 6001-skip,
                   col.names=c("n","t.s.","pos1","pos2","pos3"))
data = na.omit(data)
data$diff = c(0,diff(data$pos1,lag=1))
use.ts = data$diff

### For modelling a white noise ts with same SDev
SDev<-sd(use.ts)
use.ts = rnorm(length(data$diff), mean=0, sd=SDev)


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

pdf(file="random.pdf")
# time lag selection ------------------------------------------------------
# Aqui preguntas para que es n.partitions. Para calcular la Average Mutual
# Information es necesario construir un histograma de la serie temporal
# echale un vistazo a la ayuda de mutualInformation). n.partitions es el
# numero de bins que usamos para construir este histograma
LAG = timeLag(use.ts, technique = "ami",
              selection.method = "first.e.decay",
              lag.max = 20, do.plot = TRUE)
# selecting the embedding dim ---------------------------------------------
emb.dim = estimateEmbeddingDim(use.ts, threshold = 0.95,
                               time.lag = LAG, max.embedding.dim = 15)

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
                          number.time.steps = 20,type="b")
theilerw = 2

# correlation dimension ---------------------------------------------------
# Los valores de los radios se seleccionan de forma que C(r) vaya de valores 
# pequeños (por ejemplo, 1e-5) hasta 1. El orden de magnitud de los radios
# se puede estimar observando la time series. La serie diferencia tiene un
# rango de -1 a 1 (approx). Asi pues, un valor adecuado para el radio maximo 
# sera del orden de magnitud de 1
corr.dim =  corrDim(use.ts, min.embedding.dim = emb.dim, 
                    max.embedding.dim = emb.dim + 7,
                    time.lag = LAG,
                    min.radius = 0.04, max.radius = (max(data$pos1)-min(data$pos1))/2, 
                    n.points.radius = 45, 
                    theiler.window = theilerw,
                    do.plot = TRUE)

# Basandonos en el grafico anterior podemos ver un pequeño plateau para
# los valores comprendidos entre 0.1 y 0.3 (aproximadamente) para los
# valores mas altos de la embedding dim. Obtenemos la estimacion final
# especificando estos valores en la funcion estimate
D<- estimate(corr.dim,regression.range = c(0.3,.6),use.embeddings = 14:18,
         do.plot = T)


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


rqa.res = rqa(time.series=use.ts,
              embedding.dim = emb.dim,time.lag=LAG,
              radius = 0.5)
plot(rqa.res)

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
                         use.embeddings=4:5,
                         do.plot = TRUE)

#surrogate data test

st = surrogateTest(use.ts,significance = 0.05,one.sided = F,
                   FUN = timeAsymmetry, do.plot=T)

#sample entropy

se = sampleEntropy(corr.dim, do.plot = T)
se.est = estimate(se, do.plot = T,
                  regression.range = c(0,2))
EstimatedSEntropy<- mean(se.est)

## prediction

prediction = vector("numeric", length=15)
 
for (i in 1:15){  
prediction[i]<-  nonLinearPrediction(time.series=use.ts[1:round(length(use.ts)/2)],embedding.dim=emb.dim,
                           time.lag=LAG,
                           prediction.step=i,radius=1,
                           radius.increment=0.03/2)
}

plot(prediction)


manytests<-nonlinearityTest (time.series=use.ts, verbose = TRUE)


denoised<-nonLinearNoiseReduction(time.series=use.ts, embedding.dim=emb.dim, radius=0.1)

plot(denoised,type="l")
print(D,rqa.res,mean(se.est),ml.estimation)

df <- data.frame("Corr Dim"=D,"Lyapunov Exp"=ml.estimation,"Sample entropy estimate"=mean(se.est),"detrend fluctuation analysis"=detr.fluct)

dev.off()

