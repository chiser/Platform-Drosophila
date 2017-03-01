# Set working directory where I save the data


# Import in a dataframe just the values. Information concerning the experiment will be read further on if necessary. Add as.vector() if necessary.
#filename <- id_table[a]
filename <- "Gr66a_5_w_mit.dat"

#filename <- paste(scan(n=1),".dat", sep="")
data <- read.table(filename, header = FALSE, sep = "\t", quote = "\"" , dec = ".", fill = TRUE, skip = skip , comment.char = "", nrows = 6001-skip,col.names=c("n","t.s.","pos1","pos2","pos3"))

#Nevertheless, for opening many table I should do something like this. This opens in fixed 18 columns so I don´t need to care about the rest
#dat <- lapply(filenames,function(name) {
#  read.table(name,header=FALSE, sep ="\t", col.names= paste ("V",1:18),fill=TRUE)
#})

#data2 <- read.table(filename,header=FALSE, sep ="\t", col.names= paste ("V",1:11),fill=TRUE)

# I should consider not putting the factors, just numbers, then I could use this which is much faster. I have to make it to skip the first 35 lines

#data2 <- matrix(scan("lkj.dat", n = 5*6001), 5, 6001, byrow = TRUE)

# Import in a dataframe the information of the experiments. It needs probably to be improved: avoiding setting the number of columns to the first row.

info <- read.delim2(filename, header = FALSE, sep = "", quote = "\"",dec = ".", fill = TRUE, stringsAsFactors = TRUE)[1:41,]

# It seems to make no difference

#info2 <- read.delim ("lkj.dat", sep = " " )[1:39,]

# Another option, it gives single string lines showing even every \t, so its not the best option

#info3 <- readLines (con= "lkj.dat", n = 39, ok = FALSE, warn = TRUE, skipNul = FALSE)

#file("lkj.dat")

#showConnections(all=TRUE)
Hysteresis <- as.numeric(as.character(info$V3[40]))
lengthExp <- length(data$t.s.)
TimeExp <- data$t.s.[lengthExp]
data$Sampling<-c(0,diff(data$t.s., lag = 1))
MaxSample<-max(data$Sampling)
##### A function to return the last second of the data. Another way of doing it (with function)
#TimeExp <- function(data){
#  return(data$t.s.[length(data$t.s.)])	# return the last timestamp in the dataframe
#}
#plot(data$pos1~data$t.s., type="l",)
#mymatrix <- cbind(data$pos1,data$t.s.)
#lines(mymatrix, type="l")
# This graph show how homogeneous was the sampling rate
#plot(data[[2]]~data[[1]], type="l")

#PIsw<- c(info[11,2:5],info[12,1:5],info[13,1])

# This is for extracting the excerps where the light was on. This is not so nice as the code below
#data$side<-vector("character", length = lengthExp)
#for(i in 1:lengthExp){
#  if (data$pos1[i]>=Hysteresis)
#    data$side[i]<- "ON"
#  if (data$pos1[i]>= -Hysteresis && data$pos1[i]<= Hysteresis)
#    data$side[i]<- "MIDDLE"
#  if (data$pos1[i]<= -Hysteresis)
#    data$side[i]<- "OFF"
#}

#data$light<-vector("character", length = lengthExp)
#for(i in 1:lengthExp){
#  if(i==1 && data$side[i]=="MIDDLE") data$light <- "OFF"
#  if(i>1){
#    if (data$side[i]== "ON")
#      data$light[i]<- "ON"
#    if (data$side[i]== "MIDDLE") 
#      data$light[i]<- data$light[i-1]
#    if (data$side[i]== "OFF")
#      data$light[i]<- "OFF"
#  }
#}


switch_on <- (data$pos1 > Hysteresis) # potential signals to turn on the light
switch_off <- (data$pos1 < -Hysteresis) # potential switch-off signals
data$state <- vector("logical",length= lengthExp) 

data$state[1] <- data$pos1[1] # the default initial state is chosen as "ON". 
data$state[1] <- check_switch(data$state[1],switch_off[1],switch_on[1])
for (i in 2:lengthExp){
  data$state[i] <- data$state[i-1]
  data$state[i] <- check_switch(data$state[i], switch_off[i], switch_on[i])
}

#Extracting excerpts ON/OFF

timeOn <- NULL
for(i in 1:lengthExp){
  if(data$state[i]==1){
    ON<- data.frame("Time"=data$t.s[i],"Position"=data$pos1[i])
    timeOn<- rbind(timeOn,ON)
  }
}
timeOn$Sampling<-c(0,diff(timeOn$Time, lag = 1))

timeOff <- NULL
for(i in 1:lengthExp){
  if(data$state[i]==0){
    OFF<- data.frame("Time"=data$t.s[i],"Position"=data$pos1[i])
    timeOff<- rbind(timeOff,OFF)
  }
}
timeOff$Sampling<-c(0,diff(timeOff$Time, lag = 1))

#From timeOn I have to cut off the lines when it is in OFF. 

for(i in 1:length(timeOn[[1]])){
  ifelse(timeOn$Sampling[i]>MaxSample, timeOn$Sampling[i]<- 0, timeOn$Sampling[i]<-timeOn$Sampling[i])
}

for(i in 1:length(timeOff[[1]])){
  ifelse(timeOff$Sampling[i]>MaxSample, timeOff$Sampling[i]<- 0, timeOff$Sampling[i]<-timeOff$Sampling[i])
}

#make a cumulative time to plot each time the fly was in ON/OFF
timeOn$CumTime[1]<-0
for(i in 2:length(timeOn[[1]])){
  if (timeOn$Sampling[i]!=0)
    timeOn$CumTime[i]<-timeOn$Sampling[i]+timeOn$CumTime[i-1]
  if (timeOn$Sampling[i]==0)
    timeOn$CumTime[i]<-0
}
timeOn$burst[1]<-1
for(i in 2:length(timeOn[[1]])){
  if(timeOn$CumTime[i]==0)
    timeOn$burst[i]<-timeOn$burst[i-1]+1
  if(timeOn$CumTime[i]!=0)
    timeOn$burst[i]<- timeOn$burst[i-1]
}
BurstdurationON<-NULL
BurstsON<-unique(timeOn$burst)
for(oo in 1:length(BurstsON)){
    BurstdurationON<- c(BurstdurationON,max(timeOn$CumTime[timeOn$burst==BurstsON[oo]]))
}

timeOff$CumTime[1]<-0
for(i in 2:length(timeOff[[1]])){
  if (timeOff$Sampling[i]!=0)
    timeOff$CumTime[i]<-timeOff$Sampling[i]+timeOff$CumTime[i-1]
  if (timeOff$Sampling[i]==0)
    timeOff$CumTime[i]<-0
}
timeOff$burst[1]<-1
for(i in 2:length(timeOff[[1]])){
  if(timeOff$CumTime[i]==0)
    timeOff$burst[i]<-timeOff$burst[i-1]+1
  if(timeOff$CumTime[i]!=0)
    timeOff$burst[i]<- timeOff$burst[i-1]
}
BurstdurationOFF<-NULL
BurstsOFF<-unique(timeOff$burst)
for(oo in 1:length(BurstsOFF)){
  BurstdurationOFF<- c(BurstdurationOFF,max(timeOff$CumTime[timeOff$burst==BurstsOFF[oo]]))
}
#Lines for the graphs in burst duration
AverageOFF<- mean(BurstdurationOFF)
fitOFF<- lm(BurstdurationOFF~BurstsOFF)
AverageON<- mean(BurstdurationON)
fitON<- lm(BurstdurationON~BurstsON)



#Calculating total time of lights on/off and the PI
Toff<- 0
Ton<- 0
for(i in 1:lengthExp){
  if (data$state[i]==0)
    Toff<- Toff+data$Sampling[2]
  if (data$state[i]==1)
    Ton<- Ton+data$Sampling[i]
}

PI<- (Ton-Toff)/(Ton+Toff)

#PI without taking time in consideration
OnEvents<- sum(data$state)
OffEvents<- lengthExp-OnEvents
PI2<- (OnEvents-OffEvents)/(OnEvents+OffEvents)

#Finding maxima and minima
Maxima <- max(data[[3]])
Minima <- min(data[[3]])
Totalamplitude <- Maxima-Minima
Baseline <- mean(data[[3]])

Max<- paste("The highest datapoint is",Maxima,sep=" ")
Min<- paste("The lowest datapoint is",Minima,sep=" ")  
Base<-paste("The mean is",Baseline,sep=" ") 
ShowPI<-paste("The PI by time is",PI,sep=" ")
ShowPI2<-paste("The PI by events is",PI2,sep=" ")


# Finding all maxima and minima present

#Number of frames. TODO: I could make a for loop to avoid doing so many columns

}
# I have tried to to it in a for loop but I can´t index within a function. I have to see how can I do it
#excerps2 <- vector("numeric", length=20)
#for (i in 1:9){
 #  for(oo in 0:1){
 #    if(oo == 0) end[i] <- end(i,oo)
 #    if(oo == 1) beg[i+1] <- end(i,oo)
 #    excerps2<- c(excerps2,end[i],beg[i+1])
 #  }
  #}


end1 <- end(1,0)
beg2 <- end(1,1)
end2 <- end(2,0)
beg3 <- end(2,1)
end3 <- end(3,0)
beg4 <- end(3,1)
end4 <- end(4,0)
beg5 <- end(4,1)
end5 <- end(5,0)
beg6 <- end(5,1)
end6 <-end(6,0)
beg7 <-end(6,1)
end7 <-end(7,0)
beg8 <-end(7,1)
end8 <-end(8,0)
beg9 <-end(8,1)
end9 <-end(9,0)
beg10 <-end(9,1)
end10 <-lengthExp

#Another way of doing it but without establishing and initial function

#end1 <-round(lengthExp/10)
#beg2 <- round(lengthExp/10)+1
#end2 <-2*(round(lengthExp/10))
#beg3 <-(2*(round(lengthExp/10)))+1
#end3 <-3*(round(lengthExp/10))
#beg4 <-(3*(round(lengthExp/10)))+1
#end4 <-4*(round(lengthExp/10))
#beg5 <-(4*(round(lengthExp/10)))+1
#end5 <-5*(round(lengthExp/10))
#beg6 <-(5*(round(lengthExp/10)))+1
#end6 <-6*(round(lengthExp/10))
#beg7 <-(6*(round(lengthExp/10)))+1
#end7 <-7*(round(lengthExp/10))
#beg8 <-(7*(round(lengthExp/10)))+1
#end8 <-8*(round(lengthExp/10))
#beg9 <-(8*(round(lengthExp/10)))+1
#end9 <-9*(round(lengthExp/10))
#beg10 <-(9*(round(lengthExp/10)))+1
#end10 <-lengthExp

excerps <- c(beg1=1, end1 = end1,beg2= beg2,end2= end2,beg3= beg3,end3= end3,beg4= beg4,end4= end4,beg5= beg5,end5= end5,beg6= beg6,end6= end6,beg7= beg7,end7= end7,beg8= beg8,end8= end8,beg9= beg9,end9= end9,beg10= beg10,end10= end10) 

#Time. Making a function and then getting the times of the periods and then putting them in a vector
#The software doesn´t do it by time rather by datapoints, so this is not useful
#ss <- function (a){
#            for (i in 1:lengthExp){
#              if (data$t.s.[i]<= data$t.s.[a])
#                end <- data$t.s.[i]
#                beg <- end+1
#              output <- c(end, beg)
#            }
#            return (output)
#          }

#TimeEnd1Beg2 <- ss(end1)
#TimeEnd2Beg3 <- ss(end2)
#TimeEnd3Beg4 <- ss(end3)
#TimeEnd4Beg5 <- ss(end4)
#TimeEnd5Beg6 <- ss(end5)
#TimeEnd6Beg7 <- ss(end6)
#TimeEnd7Beg8 <- ss(end7)
#TimeEnd8Beg9 <- ss(end8)
#TimeEnd9Beg10 <- ss(end9)

#TimeExcerps <- c(TimeBeg1=0, TimeEnd1 = TimeEnd1Beg2[1],TimeBeg2= TimeEnd1Beg2[2],TimeEnd2= TimeEnd2Beg3[1],TimeBeg3= TimeEnd2Beg3[2],TimeEnd3= TimeEnd3Beg4[1],TimeBeg4= TimeEnd3Beg4[2],TimeEnd4= TimeEnd4Beg5[1],TimeBeg5= TimeEnd4Beg5[2],TimeEnd5= TimeEnd5Beg6[1],TimeBeg6= TimeEnd5Beg6[2],TimeEnd6= TimeEnd6Beg7[1],TimeBeg7= TimeEnd6Beg7[2],TimeEnd7= TimeEnd7Beg8[1],TimeBeg8= TimeEnd7Beg8[2],TimeEnd8= TimeEnd8Beg9[1],TimeBeg9= TimeEnd8Beg9[2],TimeEnd9= TimeEnd9Beg10[1],TimeBeg10= TimeEnd9Beg10[2],TimeEnd10= TimeExp) 

# A longer way to write the code for calculating times

#for (i in 1:lengthExp){
#  if (data$t.s.[i]<= data$t.s.[end1])
#   Tend1 <- data$t.s.[i]
#   Tbeg2 <- Tend1+1
#}
#for (i in 1:lengthExp){
#  if (data$t.s.[i]<= data$t.s.[end2])
#    Tend2 <- data$t.s.[i]
#  Tbeg3 <- Tend2+1
#}
#for (i in 1:lengthExp){
#  if (data$t.s.[i]<= data$t.s.[end3])
#    Tend3 <- data$t.s.[i]
#  Tbeg4 <- Tend3+1
#}
#for (i in 1:lengthExp){
#  if (data$t.s.[i]<= data$t.s.[end4])
#    Tend4 <- data$t.s.[i]
#  Tbeg5 <- Tend4+1
#}
#for (i in 1:lengthExp){
#  if (data$t.s.[i]<= data$t.s.[end5])
#    Tend5 <- data$t.s.[i]
#  Tbeg6 <- Tend5+1
#}
#for (i in 1:lengthExp){
#  if (data$t.s.[i]<= data$t.s.[end6])
#    Tend6 <- data$t.s.[i]
#  Tbeg7 <- Tend6+1
#}
#for (i in 1:lengthExp){
#  if (data$t.s.[i]<= data$t.s.[end7])
#    Tend7 <- data$t.s.[i]
#  Tbeg8 <- Tend7+1
#}
#for (i in 1:lengthExp){
#  if (data$t.s.[i]<= data$t.s.[end8])
#    Tend8 <- data$t.s.[i]
#  Tbeg9 <- Tend8+1
#}
#for (i in 1:lengthExp){
#  if (data$t.s.[i]<= data$t.s.[end9])
#    Tend9 <- data$t.s.[i]
#  Tbeg10 <- Tend9+1
#}
#Tend10<- TimeExp

#TimeExcerps <- c(Tbeg1=1,Tend1 = Tend1,Tbeg2= Tbeg2,Tend2= Tend2,Tbeg3= Tbeg3,Tend3= Tend3,Tbeg4= Tbeg4,Tend4= Tend4,Tbeg5= Tbeg5,Tend5= Tend5,Tbeg6= Tbeg6,Tend6= Tend6,Tbeg7= Tbeg7,Tend7= Tend7,Tbeg8= Tbeg8,Tend8= Tend8,Tbeg9= Tbeg9,Tend9= Tend9,Tbeg10= Tbeg10,Tend10= Tend10) 


# Calculating accumulated walking bias for each period of the first platform. TODO: write a for loop in addition to the function to make it even shorter

#for (i in 3:length(data)){
#  for(oo in 1:(length(excerps)/2)){
#    AccBias[i,oo]<- AcumBias(data[i],excerps[(2*oo)-1],excerps[2*oo])    ## I have to find out how to save all with the 3 different flies
#    AccBias <- c(AccBias,AccBias[i,oo])
#  }
#  return(AccBias)
#}
#AccBiase <- return(AccBias)

AccBias1 <- AcumBias(data$pos1,1,end1)
AccBias2 <- AcumBias(data$pos1,beg2,end2)
AccBias3 <- AcumBias(data$pos1,beg3,end3)
AccBias4 <- AcumBias(data$pos1,beg4,end4)
AccBias5 <- AcumBias(data$pos1,beg5,end5)
AccBias6 <- AcumBias(data$pos1,beg6,end6)
AccBias7 <- AcumBias(data$pos1,beg7,end7)
AccBias8 <- AcumBias(data$pos1,beg8,end8)
AccBias9 <- AcumBias(data$pos1,beg9,end9)
AccBias10 <- AcumBias(data$pos1,beg10,end10)

#Another way of doing it without function
#AccBias1 <- sum(data$pos1[1:end1])
#AccBias2 <- sum(data$pos1[beg2:end2])
#AccBias3 <- sum(data$pos1[beg3:end3])
#AccBias4 <- sum(data$pos1[beg4:end4])
#AccBias5 <- sum(data$pos1[beg5:end5])
#AccBias6 <- sum(data$pos1[beg6:end6])
#AccBias7 <- sum(data$pos1[beg7:end7])
#AccBias8 <- sum(data$pos1[beg8:end8])  
#AccBias9 <- sum(data$pos1[beg9:end9])
#AccBias10 <- sum(data$pos1[beg10:end10])

AccBias <- c(AccBias1 = AccBias1, AccBias2 = AccBias2, AccBias3 = AccBias3, AccBias4 = AccBias4, AccBias5 = AccBias5, AccBias6 = AccBias6, AccBias7 = AccBias7, AccBias8 = AccBias8, AccBias9 = AccBias9, AccBias10 = AccBias10)
# Introducing time into one vector in the ms scale. Taking out the cumulative amount. This is easily done with the Sampling variable shown above
  
  
# Calculating PIs in different ways.
# First one is takes into account the magnitude of handedness but not the time
PeriodPI <- AccBias/1800

# Takes into account the magnitude of handedness and the time

PI3 <- sum(data$pos1*data$Sampling)/(lengthExp*mean(data$Sampling))
ShowPI3<-paste("The PI by time and magnitude is",PI3,sep=" ")

# Takes into account the time but not magnitude (just the event left/right)

# data$bias <- data$bias[!is.na(data$bias)] I get an error because then I don´t have anything to replace with and then I have less values than rows. 
#system.time(
#RightTime1 <- NULL

#for(i in 1:round(lengthExp/10)){
#    if (data$pos1[i] > 0)
#        RightTime1 <- c(RightTime1, timming[i])
#        Righttime1 <- sum(RightTime1)
#}
#)

#system.time(
  #RightTime1 <- NULL
  
#  for(i in 1:round(lengthExp/10)){
#    if (data$pos1[i] > 0)
#      RightTime1 <- c(RightTime1, timming[i])
#    Righttime1 <- sum(RightTime1)
#  }
#)

#system.time(
#  RightTime2 <- NULL
#  
#  for(i in (round(lengthExp/10)+1) : (2*(round(lengthExp/10))) {
#    if (data$pos1[i] > 0)
#      RightTime2 <- c(RightTime2, timming[i])
#      Righttime2 <- sum(RightTime2)
#  }
#)

#for(i in 1:round(lengthExp/10)){
#  if (data$pos1[i] > 0)
#    RightTime1 <- c(RightTime1, timming[i])
#  Righttime1 <- sum(RightTime1)
#}

#for(i in 1:round(lengthExp/10)){
#  if (data$pos1[i] > 0)
#    RightTime1 <- c(RightTime1, timming[i])
#  Righttime1 <- sum(RightTime1)
#}


#if (i < (lengthExp/10) && data$bias[i] == TRUE)
#          RightTime1 <- c(RightTime1, timming[i])
#        Righttime1 <- sum(RightTime1)
#        if (i < (lengthExp/10) && data$bias[i] == TRUE)
#          RightTime1 <- c(RightTime1, timming[i])
#        Righttime1 <- sum(RightTime1)
#        if (i < (lengthExp/10) && data$bias[i] == TRUE)
#          RightTime1 <- c(RightTime1, timming[i])
#        Righttime1 <- sum(RightTime1)
#        if (i < (lengthExp/10) && data$bias[i] == TRUE)
#          RightTime1 <- c(RightTime1, timming[i])
#        Righttime1 <- sum(RightTime1)
#        if (i < (lengthExp/10) && data$bias[i] == TRUE)
#          RightTime1 <- c(RightTime1, timming[i])
#        Righttime1 <- sum(RightTime1)
#        if (i < (lengthExp/10) && data$bias[i] == TRUE)
#          RightTime1 <- c(RightTime1, timming[i])
#        Righttime1 <- sum(RightTime1)
#}


#open a pdf file
pdf(paste(filename,".pdf", sep=""))
plot(data[[3]]~data[[2]], type="l", main=filename)

# violinplot for the distribution
require("vioplot")
vioplot(data[[3]], ylim = c(-5,5), col="lightgrey", names=filename)
abline(h=0, col =2)
title ("Distribution the choice")
#I have to see where to put this
text(1,4,Max, pos=1)
text(1,3,Min,pos=3)
text(1,2,Base,pos=3)

#Checking for normality
require("car") 
qqPlot(data[[3]])
text (-3,3,ShowPI,pos=1)
text (-3,2,ShowPI2,pos=1)
text (-3,1,ShowPI3,pos=1)

# TODO: find a way to plot it nicer.
OnDynamics<-plot(timeOn$Position~timeOn$CumTime, type="l",main="Dynamics when light on")
OffDynamics<-plot(timeOff$Position~timeOff$CumTime, type="l",main="Dynamics when lights off")

#Burst duration for ON/OFF along the experiment
plot(BurstdurationOFF,main="Burstduration when OFF")
segments(x0=0,y0=AverageOFF,x1=length(BurstsOFF),y1=AverageOFF,col="blue",lwd=1)
abline(fitOFF,col="red")

plot(BurstdurationON,main="Burstduration when ON")
segments(x0=0,y0=AverageON,x1=length(BurstsON),y1=AverageON,col="blue",lwd=1)
abline(fitON,col="Red")

#Checking the On/Off dynamics
OnDynamics

plot(AccBias,type="b",main="Cumulative magnitude of choice for each period")

#See how the sampling reliability was
Sampling<-diff(data[[2]], lag = 1)
plot(Sampling)


# FFT function and plotting
X.k<- fft(data[[3]])

plot.frequency.spectrum <- function(X.k, xlimits=c(0,20)) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

plot.frequency.spectrum(X.k)

dev.off()