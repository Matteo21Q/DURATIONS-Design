# Set working directory
setwd("~/Documents/Durations Design/Durations Design - Inference/Figures/")

# Load the results and boot library

load("~/Documents/Durations Design/Durations Design - Inference/Simulation Results/myriad/Bootstrap CI - Estimand RD.RData")
library(boot)

# When duration.recommended is na, it means the actual duration recommended is max.dur:

duration.recommended[is.na(duration.recommended)]<-max.dur
dur.percentile<-apply(duration.recommended,2,quantile, probs=0.025, na.rm=T)

# calculate actual maximum acceptable duration:

max.acc<-rep(NA,n.scen)
for (ss in 1:n.scen) {
  formula.1<-get(paste("formula",ss,".4", sep=""))
  max.acc[ss]<-formula.1(20)-0.1
  if (ss==4)  max.acc[ss]<-formula.1(20)
}

#Plot:

m <- matrix(c(1,2,3,3),nrow = 2,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.85,0.15))


par(mar = c(5,7,4,2))
plot(real.min.duration, 1:16, xlim=c(8,20), type="p", pch=124, xlab="Duration", ylab="", yaxt="n", main = "Durations")
axis(2, at = 1:16, labels=paste("Scenario", 1:16), las=2)
color<-rep(NA,n.scen)
pch.type<-rep(NA,n.scen)
for (cc in 1:n.scen) {
  if (dur.percentile[cc]>=real.min.duration[cc]) color[cc]<-"blue" else color[cc]<-"red" 
  if (dur.percentile[cc]>real.min.duration[cc]) pch.type[cc]<-(-9668)  
  if (dur.percentile[cc]==real.min.duration[cc]) pch.type[cc]<-(-9632)  
  if (dur.percentile[cc]<real.min.duration[cc]) pch.type[cc]<-(-9658) 
}
points(dur.percentile,1:16, col=color, pch=pch.type, lwd=3)

# Calculate cure rate at 2.5th percentile of recommended durations

recom.cur<-rep(NA,n.scen)
for (ss in 1:n.scen) {
  formula.1<-get(paste("formula",ss,".4", sep=""))
  recom.cur[ss]<-formula.1(dur.percentile[ss])
}

plot(max.acc, 1:16, xlim=c(0.70,1), type="p", pch=124, xlab="Cure rate", ylab="", yaxt="n", main = "Cure rate")
axis(2, at = 1:16, labels=paste("Scenario", 1:16), las=2)
points(recom.cur,1:16, col=color, pch=pch.type, lwd=3)


# Legend:

par(mar = c(0.5,0.5,0.5,0.5))
plot(1, type = "p", axes=FALSE, xlab="", ylab="")
plot_colors <- c("black", "blue","blue","red")
legend(x = "top",inset = 0,
       legend = c("Actual optimal duration / cure rate", "2.5th Percentile Recommended Duration / Cure Rate (acceptable)", "2.5th Percentile Recommended Duration / Cure Rate (correct)", "2.5th Percentile Recommended Duration / Cure Rate (not acceptable)"), 
       col=c("black", "blue","blue", "red"),  pch=c(124,-9668,-9632,-9658))
par(mar = c(5.1, 4.1, 4.1, 2.1))

