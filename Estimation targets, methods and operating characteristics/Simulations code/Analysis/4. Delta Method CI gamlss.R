# Load relevant libraries
library(boot)
library(flux)
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

# Some matrix initializations:
Test.accept<-matrix(NA,n.sim,n.scen)
duration.recommended<-matrix(NA,n.sim,n.scen)
real.min.duration<-real.min.duration.disc<-rep(NA,n.scen)
Power.scen<-matrix(NA,n.sim, n.scen)
PowerTRUE.scen<-matrix(NA,n.sim, n.scen)
T1ER.scen<-matrix(0, n.sim, n.scen)

# Start simulations looping over different scenarios

for (s in 1:n.scen) {
  
  formula.1<-get(paste("formula",scenarios[s],".4", sep=""))
  
  #Calculate expected outcome given scenario
  y.dur<-formula.1(x.dur)
  max.p<-formula.1(max.dur)
  
  # Define formula to predict curve and associated SE:
  predicted<-function(x, fit) { 
    return(predict(fit,data.frame(durlong=x),type="resp"))
  }
  predicted.se<-function(x, fit) { 
    return(predict(fit,data.frame(durlong=x.dur), se.fit=T)$se.fit)
  }
  
  # Calculate real minimum acceptable duration 
  
  real.min.duration[s]<-max.dur
  flag=1
  t<-length(x.dur)-1
  while (t>0) {
    if ((formula.1(x.dur[t])+0.1)>max.p) {
      flag=0
      real.min.duration[s]<-x.dur[t]
    }
    t<-t-1
  }
  if (flag==1) real.min.duration[s]<-real.min.duration.disc[s]<-NA
  flag=t=1
  while ((t<length(poss.durations))&(flag==1)) {
    if ((real.min.duration[s])<=poss.durations[t]) {
      flag=0
      real.min.duration.disc[s]<-poss.durations[t]
    }
    t=t+1
  }
  
  # Run n.sim simulations:
  for (i in 1:n.sim) {
    
    # outcome data
    y<-whole.data[[s]][,2,i]
    durlong<-whole.data[[s]][,1,i]
    
    fit.gamlss<-gamlss(y~fp(durlong), trace=F, family = BI)
    powers.fp<-getSmo(fit.gamlss)$power # these are the selected powers
    
    # Fit the glm model with the selected powers (as in gamlss cannot recover vcov matrix)
    # We need to generate first transformed variables with selected powers
    if (0 %in% powers.fp) {
      durlong.p1<-log(durlong)
      dur.rand.p1<-log(all.durations)
      if (powers.fp[1]!=powers.fp[2]) {
        durlong.p2<-durlong^powers.fp[powers.fp!=0]
        dur.rand.p2<-all.durations^powers.fp[powers.fp!=0]
      } else {
        durlong.p2<-log(durlong)*log(durlong)
        dur.rand.p2<-log(all.durations)*log(all.durations)
      }
    } else {
      durlong.p1<-durlong^powers.fp[1]
      dur.rand.p1<-all.durations^powers.fp[1]
      if (powers.fp[1]!=powers.fp[2]) {
        durlong.p2<-durlong^powers.fp[2]
        dur.rand.p2<-all.durations^powers.fp[2]
      } else {
        durlong.p2<-log(durlong)*durlong^powers.fp[2]
        dur.rand.p2<-log(all.durations)*all.durations^powers.fp[2]
      }
    }
    
    fit.fp<-glm(y~durlong.p1+durlong.p2, family = binomial)
    
    # Store predictions on the logodds scale at each of the 7 durations:
    preds.logodds.fp<-predict(fit.fp, newdata=data.frame(durlong.p1=dur.rand.p1, durlong.p2=dur.rand.p2))
    # Store variance covariance matrix of parameters
    vcov.fit.fp<-vcov(fit.fp)
    
    up.bounds.CI<-rep(NA,length(poss.durations))
    
    for (j in 1:(length(poss.durations))) {
      
      # Need to calculate the Jacobian for f= invlogit(a+d1x^p1+d1x^p2)-inv.logit(a+d2x^p1+d2x^p2)
      Jacob.fp<-c((exp(preds.logodds.fp[n.dur])/(1+exp(preds.logodds.fp[n.dur]))^2)-(exp(preds.logodds.fp[j])/(1+exp(preds.logodds.fp[j]))^2), 
                  (dur.rand.p1[n.dur]*exp(preds.logodds.fp[n.dur])/(1+exp(preds.logodds.fp[n.dur]))^2)-(dur.rand.p1[j]*exp(preds.logodds.fp[j])/(1+exp(preds.logodds.fp[j]))^2),
                  (dur.rand.p2[n.dur]*exp(preds.logodds.fp[n.dur])/(1+exp(preds.logodds.fp[n.dur]))^2)-(dur.rand.p2[j]*exp(preds.logodds.fp[j])/(1+exp(preds.logodds.fp[j]))^2))
      
      # As usual, thanks to delta method:
      var.diff.fp <- t(Jacob.fp)%*%vcov.fit.fp%*%Jacob.fp 
      se.diff.fp<-sqrt(var.diff.fp)
      mean.diff.fp<-inv.logit(preds.logodds.fp[n.dur])-inv.logit(preds.logodds.fp[j])
      CI.diff<-c(mean.diff.fp-qnorm(1-alpha)*se.diff.fp, mean.diff.fp+qnorm(1-alpha)*se.diff.fp)
      
      up.bounds.CI[j]<-CI.diff[2]
      
    }
    
    
    #Calculate observed control event rate:
    po0<-mean(y[durlong==max.dur])
    if (po0==1) po0=0.999 #Logit of 1 is inf, so we modify to 99.9% the observed c.e.r. if = 100%
    
    
    # What is point where predicted lower CI first crosses acceptability curve?
    flag=t=1
    min.duration<-max.dur
    while ((t<length(poss.durations))&(flag==1)) {
      if ((up.bounds.CI[t]-0.1)<0) {
        flag=0
        min.duration<-poss.durations[t]
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
  if (real.min.duration[s]==max.dur) real.min.duration[s]<-NA
  
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
View(Table)

# save.image("Delta Method CI gamlss - Estimand RD.RData")
