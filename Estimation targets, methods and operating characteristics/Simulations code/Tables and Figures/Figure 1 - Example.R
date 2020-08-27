# An example of duration-response curve:

# Load relevant libraries
library(boot)
library(gamlss)
fp<-gamlss::fp

# Set seed
set.seed(1)

#Load Simulation Scenarios
source("C:/Users/rmjlmqu/Documents/Durations Design/Durations Design - Inference/R script/Simulation Scenarios.R")

# Set working directory
setwd("C:/Users/rmjlmqu/Documents/Durations Design/Durations Design - Inference/Simulation Results")

# Load Simulation Parameters
load("Simulation Parameters.RData")

# Load Simulated Data
load("Simulated Data.RData")

# Set working directory
setwd("C:/Users/rmjlmqu/Documents/Durations Design/Durations Design - Inference/Figures")

# Select scenario 6 and first simulation:
s=6
i=200
y<-whole.data[[s]][,2,i]
durlong<-whole.data[[s]][,1,i]
data.mfp<-data.frame(y, durlong)
fit.i<-gamlss(y~fp(durlong), data=data.mfp, trace=F, family = BI)

# Define formula to predict curve:
predicted<-function(x, fit, datac) { 
  return(predict(object=fit,newdata=data.frame(durlong=x), data=datac, type="resp"))
}
invisible(capture.output(y.dur.est<-predicted(max.dur, fit.i, data.mfp)))

# Define acceptability curve
tolerab.func<-function(d) {
  if (d<18) {
    return(-0.005*d+0.14)
  } else {
    return(-0.025*d+0.5)
  }
}
acceptability<-function(x) {
  return(y.dur.est-tolerab.func(x)+0*x)
}
acceptability<-function(x) {
  return(y.dur.est-0.01)
}

pdf(file="Example.pdf",width=7,height=7)

curve(predicted(x, fit.i, data.mfp), xlim=c(min.dur,max.dur), ylim=c(0.5,1), xlab = "Duration", ylab="% cure", lwd=3, main="Duration-Response Curve")
abline(h=(y.dur.est-0.05),lty=2, col="blue")
abline(h=(y.dur.est-0.1),lty=3, col="red")
segments(8,y.dur.est-0.1,28,y.dur.est,col="grey")
dev.off()