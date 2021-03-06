
setwd("C:/Users/LocalAdmin/Desktop/StreamingPlatform")

filename = id_table1[oo]


data = read.table(filename, header = FALSE, sep = ";", quote = "\"" ,
                  dec = ".", fill = TRUE , comment.char = "", 
                  col.names=c("Position","StreamingTime","Error"))


data$TimeStamp<-seq(0,(0.004*length(data$Position))-0.004,0.004)
 


TimeAverage<-flyJoystickDownsampleTime(data,0.05)

fly<- matrix("numeric", nrow=length(TimeAverage$Position), ncol=1)
fly<-TimeAverage$Position

#plot(data$Position[0:1000]~data$TimeStamp[0:1000],type="l")   ### TODO: I have to see this with a fly because the resonance frequency of the platform is high enough to make the 20Hz sampling an undersampling
#plot(TimeAverage[0:100,],type="l")

#interp<- spline(TimeAverage$Position)
#plot(interp$y[1:200],type="l")

