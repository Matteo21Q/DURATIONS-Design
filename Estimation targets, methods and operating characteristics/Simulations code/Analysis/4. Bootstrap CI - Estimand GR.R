# Load relevant libraries
library(boot)
library(gamlss)
library(numDeriv)
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

time.start<-Sys.time()
# Some matrix initializations:
Test.accept<-matrix(NA,n.sim,n.scen)
duration.recommended<-matrix(NA,n.sim,n.scen)
real.min.duration<-real.min.duration.disc<-rep(NA,n.scen)
Power.scen<-matrix(NA,n.sim, n.scen)
PowerTRUE.scen<-matrix(NA,n.sim, n.scen)
T1ER.scen<-matrix(0, n.sim, n.scen)
Test.accept2<-matrix(NA,n.sim,n.scen)
duration.recommended2<-matrix(NA,n.sim,n.scen)
Power.scen2<-matrix(NA,n.sim, n.scen)
PowerTRUE.scen2<-matrix(NA,n.sim, n.scen)
T1ER.scen2<-matrix(0, n.sim, n.scen)

# Start simulations looping over different scenarios

for (s in 1:n.scen) {
  
  formula.1<-get(paste("formula",scenarios[s],".4", sep=""))
  
  #Calculate expected outcome given scenario
  y.dur<-formula.1(x.dur)
  max.p<-formula.1(max.dur)
  
   
  # Calculate real minimum acceptable duration 
  t=length(x.dur)
  flag=1
  if (s==6) gradients<-c(0,grad(formula.1, x.dur[2:length(x.dur)])) else   gradients<-grad(formula.1, x.dur)
  real.min.duration[s]<-max.dur
  while ((t>0)&(flag==1)) {
    if (abs(gradients[t])>0.02) {
      flag=0
    } else {
      real.min.duration[s]<-x.dur[t]
      t=t-1
    }
  }
  
  flag=t=1
  while ((t<length(poss.durations))&(flag==1)) {
    if ((real.min.duration[s])<=poss.durations[t]) {
      flag=0
      real.min.duration.disc[s]<-poss.durations[t]
    }
    t=t+1
  }
  if (is.na(real.min.duration.disc[s])) real.min.duration.disc[s]<-max.dur
  
  # Run n.sim simulations:
  for (i in 1:n.sim) {

    # outcome data
    y<-whole.data[[s]][,2,i]
    durlong<-whole.data[[s]][,1,i]

    # Define data
    data.mfp<-data.frame(y, durlong)

    # Function to bootstrap:

    min.duration.stack<-NULL
    find.min.dur<- function (data.mfp, indices) {
      # Select bootstrap sampel:
      da <- data.mfp[indices,]

      # Fit Fractional Polynomial regression model
      fit.i<-gamlss(y~fp(durlong), data=da, trace=F, family = BI)

      # Define formula to predict curve and associated SE:
      predicted<-function(x) {
        return(predict(object=fit.i,newdata=data.frame(durlong=x), data=da, type="resp"))
      }

      # Predict duration-response curve and associated pointwise CI
      invisible(capture.output(y.dur.est<-grad(predicted, x.dur)))

      #Define acceptability curve. Constant at less than 15% gradient.
      # acceptability<-function(x) {
      #   return(0.02+0*x)
      # }
      # y.accept<-acceptability(x.dur)

      # What is point where predicted lower CI first crosses acceptability curve?
      flag=1
      t=length(x.dur)-1
      min.duration<-max.dur
      while ((t>0)&(flag==1)) {
        if (is.na(y.dur.est[t])||(abs(y.dur.est[t])-0.02)>0) {
          flag=0
        } else {
          min.duration<-x.dur[t]
          t=t+1
        }
      }
      output<-c(min.duration, y.dur.est[1:8])
      return(output)
    }

    results<-boot(data.mfp,find.min.dur,M.boot, parallel = "multicore", ncpus=32)
    if (length(unique(results$t[,1])) > 1) {
      res.ci<-boot.ci(results, conf=1-alpha*2, type="perc", index=1)
      min.duration<- res.ci$perc[5]
    } else {
      min.duration<-results$t0
    }


    flag=t=1
    while ((t<(length(all.durations)+1))&(flag==1)) {
      if ((min.duration)<=all.durations[t]) {
        flag=0
        min.duration<-all.durations[t]
      }
      t=t+1
    }

    if (min.duration==max.dur) {
      Test.accept[i,s]<-0
    } else {
      duration.recommended[i,s]<-min.duration
      Test.accept[i,s]<-1
    }
    if (min.duration<real.min.duration[s]) {
      T1ER.scen[i,s]<-1
      Power.scen[i,s]<-0
      PowerTRUE.scen[i,s]<-0
    } else {
      if (real.min.duration[s]!=max.dur) {
        if (min.duration<max.dur) {
          Power.scen[i,s]<-1
        } else {
          Power.scen[i,s]<-0
        }
        if (min.duration<=real.min.duration.disc[s] & min.duration>=real.min.duration[s]) {
          PowerTRUE.scen[i,s]<-1
        } else {
          PowerTRUE.scen[i,s]<-0
        }
      }
    }

 
    if (i%%10==0) cat("Simulation number ", i, " completed\n")
  }
  #if (real.min.duration[s]==max.dur) real.min.duration[s]<-NA
  
  cat("Scenario number ", s, " completed\n")
  
}

successes<-apply(Test.accept,2,mean)
T1ER<-apply(T1ER.scen,2,mean)
Power<-apply(Power.scen,2,mean)
PowerTRUE<-apply(PowerTRUE.scen,2,mean)

Table<-matrix(NA,n.scen,9)
for (s in 1:n.scen) {
  Table[s,1]<-paste("Scenario ", scenarios[s], sep="")
  Table[s,2]<-Power[s]
  Table[s,3]<-PowerTRUE[s]
  Table[s,4]<-T1ER[s]
  Table[s,5]<-n
  Table[s,6]<-real.min.duration[s]
  Table[s,7:9]<-quantile(duration.recommended[,s], c(0, 0.50, 1), na.rm = T)
}
colnames(Table)<-c("Scenario", "Power (any)", "Power (true)", "T1ER", "Sample size", "Real Min Duration" ,  "min",  "median",  "max")

successes2<-apply(Test.accept2,2,mean)
T1ER2<-apply(T1ER.scen2,2,mean)
Power2<-apply(Power.scen2,2,mean)
PowerTRUE2<-apply(PowerTRUE.scen2,2,mean)

Table2<-matrix(NA,n.scen,9)
for (s in 1:n.scen) {
  Table2[s,1]<-paste("Scenario ", scenarios[s], sep="")
  Table2[s,2]<-Power2[s]
  Table2[s,3]<-PowerTRUE2[s]
  Table2[s,4]<-T1ER2[s]
  Table2[s,5]<-n
  Table2[s,6]<-real.min.duration[s]
  Table2[s,7:9]<-quantile(duration.recommended2[,s], c(0, 0.50, 1), na.rm = T)
}
colnames(Table2)<-c("Scenario", "Power (any)", "Power (true)", "T1ER", "Sample size", "Real Min Duration" ,  "min",  "median",  "max")

#View(Table)
time.tot<-Sys.time()-time.start
#save.image("Bootstrap CI - Estimand GR.RData")
