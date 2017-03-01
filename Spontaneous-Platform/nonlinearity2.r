require("tseriesChaos")
require("fNonlinear")
require("vioplot")
require("car")
require("nonlinearTseries")

setwd("C:/Users/LocalAdmin/Desktop/platform/altehaken-intensitytests")
#filename <- id_table1[a]
filename <- "Gr66a_2_m_ohne_15lx.dat"
#filename <- "Gr66a_3_m_ohne_15lx.dat"
skip<-36
#filename <- paste(scan(n=1),".dat", sep="")
data <- read.table(filename, header = FALSE, sep = "\t", quote = "\"" , dec = ".", fill = TRUE, skip = skip , comment.char = "", nrows = 6001-skip,col.names=c("n","t.s.","pos1","pos2","pos3"))
data<-na.omit(data)
#Comment<-is.na(data[1,1])
#if(Comment==TRUE)
# Import in a dataframe the information of the experiments. It needs probably to be improved: avoiding setting the number of columns to the first row.


#data$Sampling<-c(0,diff(data$t.s., lag = 1))
data$diff<-c(0,diff(data$pos1,lag=1))

derivative<- data$diff
derivative<- data$pos1
#diffplot<- data$diff[1:3000]

#lag1<- data$diff[2:3001]
#lag2<- data$diff[3:3002]
#lag3<- data$diff[4:3003]
#lag4<- data$diff[5:3004]
#lag5<- data$diff[6:3005]
#lag6<- data$diff[7:3006]

pdf(paste(filename,"nonlinear",".pdf", sep=""))

#distribution of differenced movements
vioplot(derivative, ylim = c(-5,5), col="lightgrey", names=filename)
abline(h=0, col =2)

hist(derivative,prob=0,breaks=200)
lines(density(derivative))

qqPlot(derivative)

# I have to find a way to make log10 with the negative numbers. I want to see if it follows and power law. And the meaning of power law!?
#preprop<- derivative+5.000
#logar<- log10(preprop)
#plot(logar)

# To determine a reasonable delay. Both mutual give the same.Need to know what partitions are, if important, and technique and selection method and value

mutualPlot(derivative, partitions = 16, lag.max = 100, doplot = TRUE)  # from fNonlinear

mutualInformation(derivative, lag.max = 20, n.partitions = 16, do.plot = TRUE)     #from nonlinearTseries

LAG<- timeLag(derivative, technique = "ami",selection.method = "first.e.decay", value = 1/exp(1), lag.max = 20, do.plot = TRUE,main = "TimeLag")


#Method of false nearest neghbours to help deciding the optimal embedding dimension
#First row: fraction of false neighbours. Second row:total number of neighbours

OptimalEmb<-false.nearest(derivative,m=40,d=1,t=180, eps=1, rt=10)      # from tseriesChaos
print.false.nearest(OptimalEmb)
plot.false.nearest(OptimalEmb)


falsennPlot(derivative, m=40, d=1, t=180, rt = 10, eps = 1, doplot = TRUE)  # from fNonlinear


DIM<-estimateEmbeddingDim(derivative, number.points = 5965,                       #from nonlinearTseries. When E1 stops changing then it reached the dimension
                     time.lag = LAG, max.embedding.dim = 20, threshold = 0.95, # E2 is aprox one for every value when the system is stochastic. For deterministic some d are different from 1
                     max.relative.change = 0.1, do.plot = TRUE,
                     main = "Computing the embedding dimension", xlab = "dimension (d)",
                     ylab = "E1(d) & E2(d)")

#Computes the sample correlation integral over a grid of neps length scales starting from eps.min and for multiple embedding dimensions
# From this I get a proper epsilon, and together with the calculated dimension, lag, theiler window I can pass enough arguments to further functions

epsilon<- d2(derivative, m=20, d=LAG, t=180, eps.min=1, neps=20)

# Build the Taken vectors with de embedding determined by estimateEmbeddingDim and LAG

Takens= buildTakens(derivative, embedding.dim=DIM, time.lag=LAG)

WHO<-findAllNeighbours(takens=Takens, radius=10)                      #from nonlinearTseries. This one needs epsilon but this isn´t still calculated. It tells me who is neighbour from who. The falsenn tell me just how many neighbours

# For choosing a proper Theiler window. I still don´t understand very well what I see but I think I have to get the lower time when distance is at the lowest

SpaceTime<- spaceTimePlot(takens = Takens)

dataSpaceTime <- contourLines(SpaceTime)

# Poincaré section

embedding<-embedd(derivative,DIM,LAG)
plot(embedding)

Poincare<-poincareMap(takens = Takens)     # from nonlinearTseries


# Functions for estimating the correlation sum and the correlation dimension of a dynamical system from 1-dimensional time series using Takens' vectors.

correlDim<- corrDim(derivative, min.embedding.dim = 2, max.embedding.dim = DIM+2,time.lag = LAG, min.radius=1, max.radius=10, corr.order = 2,
                    n.points.radius = 5,  theiler.window = 100, do.plot = TRUE)
        
        ## Once I calculate this I can make a graph log C(e)-log e and get the value of the slope (v) of the linear part. Do it for several dimensions and plot it
        ## to find out if v converges to a constant value to see if it is chaotic. I think all this is done with estimate function

estimate(correlDim)

# I have to see what infDim does, what is the difference with the above. how to set fixed mass?

infD<-infDim(derivative, min.embedding.dim = 2,
       max.embedding.dim = DIM+2, time.lag = LAG, min.fixed.mass=1,
       max.fixed.mass=90, number.fixed.mass.points = 10, radius=1,
       increasing.radius.factor = sqrt(2), 
       do.plot = TRUE)

logRadius(infD)

# Recurrence plot. I have to see why is all black in the graph with my sample. Computationally expensive!!

RecurrencePlot<-recurr(derivative, 2, 1)

recurrencePlot(takens = Takens, radius)              #from nonlinearTseries
               

# detrended fluctuation analysis. I need to know how to choose what window size range and points to use

detrend<-dfa(time.series, window.size.range = c(10, 300), npoints = 20,do.plot=TRUE)      #from nonlinearTseries

windowSizes(detrend)                 # to see what window sizes where used for te dfa

#Generates surrogate samples from the original time series.

FFTsurrogate(time.series, n.samples = 1)

fluctuationFunction(x)

# Calculation of Lyapunov from time series. I think from literature this isn´t that reliable because is prone to error in certain cases

ml<-maxLyapunov(time.series, min.embedding.dim = 2,                                        #from nonlinearTseries
            max.embedding.dim = min.embedding.dim, time.lag = 1, radius,
            theiler.window = 1, min.neighs = 5, min.ref.points = 500,
            max.time.steps = 10, number.boxes = NULL, sampling.period = 1,
            do.plot = TRUE, ...)

plot(ml)
ml.estimation = estimate(ml,regression.range = c(0,15),
                         use.embeddings=4:5,
                         do.plot = TRUE)

cat("The calculated max Lyapunov exponent is: ",ml.estimation,"\n")

# Testing for nonlinearity and if it is different from a gaussian linear process

nonlinearityTest(time.series, verbose = TRUE)

surrogateTest(time.series, significance = 0.05, one.sided = FALSE,
              alternative = c("smaller", "larger"), K = 1, FUN, verbose = TRUE,
              do.plot = TRUE, xlab = "Values of the statistic", ylab = "",
              main = "Surrogate data testing", ...)

# Forecasting

nonLinearPrediction(time.series, embedding.dim, time.lag, prediction.step,
                    radius, radius.increment)


# TODO: maybe taking noise out or other test to test if it isn´t just random. Also de entropy invariant, although I think they are not appropiate with time series with noise or something


dev.off()