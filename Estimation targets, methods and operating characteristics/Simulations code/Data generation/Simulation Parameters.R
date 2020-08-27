# Clear R environment
rm(list=ls())

# Set Working directory
setwd("C:/Users/rmjlmqu/Documents/Durations Design/Durations Design - Inference/Simulation Results")

# Parameter values

n.sim<-1000
n.obj<-n<-500
n.arms<-7
#npergroup<-ceiling(n.obj/n.arms)
#n<-npergroup*n.arms
scenarios<-c(1:16)
n.scen<-length(scenarios)
min.dur<-8
max.dur<-20
durations<-seq(min.dur,max.dur,length.out = n.arms) # We choose equidistant duration arms, between the minimum and the maximum
poss.durations<-seq(min.dur,max.dur-1)
all.durations<-seq(min.dur,max.dur)
n.dur<-length(all.durations)
#durlong<-rep(durations, each=npergroup)
x.dur<-seq(min.dur,max.dur, length.out = 100)
alpha<-0.025
M.boot<-500

# Save Parameters

save.image(file="Simulation Parameters.RData")
