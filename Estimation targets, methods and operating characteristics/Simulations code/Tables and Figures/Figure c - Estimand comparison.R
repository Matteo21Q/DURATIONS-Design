# Set working directory
setwd("~/Documents/Durations Design/Durations Design - Inference/Figures/")

# Load the results 

# Now Fixed rate
load("~/Documents/Durations Design/Durations Design - Inference/Simulation Results/myriad/Bootstrap CI - Estimand FR.RData")
duration.recommended[is.na(duration.recommended)]<-max.dur
Bootstrap.dur.CI<-data.frame(Table)
Bootstrap.dur.CI[,-1]<-sapply(Bootstrap.dur.CI[,-1], as.character)
Bootstrap.dur.CI[,-1]<-sapply(Bootstrap.dur.CI[,-1], as.numeric)
Bootstrap.dur.CI[,6]<-round(Bootstrap.dur.CI[,6], digits=3)
Bootstrap.dur.CI[,2:4]<-Bootstrap.dur.CI[,2:4]*100
Bootstrap.dur.CI<-cbind(Bootstrap.dur.CI, apply(duration.recommended,2,quantile, probs=0.025, na.rm=T))
Bootstrap.dur.CI<-Bootstrap.dur.CI[,c(1:4,6,7,10,8)]
colnames(Bootstrap.dur.CI)<-c("Scenario", "Power (any)", "Power (true)", "T1ER",
                              "Real Min Duration","min", "2.5th Perc", "median")

# Now Fixed risk rate
load("~/Documents/Durations Design/Durations Design - Inference/Simulation Results/myriad/Bootstrap CI - Estimand RR.RData")
duration.recommended[is.na(duration.recommended)]<-max.dur
Bootstrap.dur.CI2<-data.frame(Table)
Bootstrap.dur.CI2[,-1]<-sapply(Bootstrap.dur.CI2[,-1], as.character)
Bootstrap.dur.CI2[,-1]<-sapply(Bootstrap.dur.CI2[,-1], as.numeric)
Bootstrap.dur.CI2[,6]<-round(Bootstrap.dur.CI2[,6], digits=3)
Bootstrap.dur.CI2[,2:4]<-Bootstrap.dur.CI2[,2:4]*100
Bootstrap.dur.CI2<-cbind(Bootstrap.dur.CI2, apply(duration.recommended,2,quantile, probs=0.025, na.rm=T))
Bootstrap.dur.CI2<-Bootstrap.dur.CI2[,c(1:4,6,7,10,8)]
colnames(Bootstrap.dur.CI2)<-c("Scenario", "Power (any)", "Power (true)", "T1ER",
                              "Real Min Duration","min", "2.5th Perc", "median")

# Now acceptability frontier
load("~/Documents/Durations Design/Durations Design - Inference/Simulation Results/myriad/Bootstrap CI - Estimand AF.RData")
duration.recommended[is.na(duration.recommended)]<-max.dur
Bootstrap.dur.CI3<-data.frame(Table)
Bootstrap.dur.CI3[,-1]<-sapply(Bootstrap.dur.CI3[,-1], as.character)
Bootstrap.dur.CI3[,-1]<-sapply(Bootstrap.dur.CI3[,-1], as.numeric)
Bootstrap.dur.CI3[,6]<-round(Bootstrap.dur.CI3[,6], digits=3)
Bootstrap.dur.CI3[,2:4]<-Bootstrap.dur.CI3[,2:4]*100
Bootstrap.dur.CI3<-cbind(Bootstrap.dur.CI3, apply(duration.recommended,2,quantile, probs=0.025, na.rm=T))
Bootstrap.dur.CI3<-Bootstrap.dur.CI3[,c(1:4,6,7,10,8)]
colnames(Bootstrap.dur.CI3)<-c("Scenario", "Power (any)", "Power (true)", "T1ER",
                              "Real Min Duration","min", "2.5th Perc", "median")



# Matrix of powers and Type 1 errors:

Pow.Any.Mat<-data.frame(c(Bootstrap.dur.CI[,"Power (any)"], Bootstrap.dur.CI2[,"Power (any)"],
                          Bootstrap.dur.CI3[,"Power (any)"]), rep(1:3,each=n.scen))
colnames(Pow.Any.Mat)<-c("Power (any)", "Method")
T1ER.Mat<-data.frame(c(Bootstrap.dur.CI[,"T1ER"], Bootstrap.dur.CI2[,"T1ER"],
                          Bootstrap.dur.CI3[,"T1ER"]), rep(1:3,each=n.scen))
colnames(T1ER.Mat)<-c("T1ER", "Method")
T1ER.Mat$scen<-1:16
Pow.Any.Mat$scen<-1:16

#Plot:

pdf(file="Estimands comparison.pdf",width=12,height=7)

par(mar=c(9,4,4,2), mfrow=c(1,2))
plot(as.numeric(as.character(T1ER.Mat[T1ER.Mat$Method==1,1])), 
     T1ER.Mat[T1ER.Mat$Method==1,3],   type="p", pch=20, 
     ylim=c(0,17), xlab="", ylab="", xaxt="n", yaxt="n", main = "Type 1 error")
points(as.numeric(as.character(T1ER.Mat[T1ER.Mat$Method==2,1])), 
       T1ER.Mat[T1ER.Mat$Method==2,3], pch=4)
points(as.numeric(as.character(T1ER.Mat[T1ER.Mat$Method==3,1])), 
       T1ER.Mat[T1ER.Mat$Method==3,3], pch=2)

axis(1, at = seq(1,10,1), labels=paste(seq(1,10,1), "%", sep=""), las=2)
axis(2, at = 1:16, labels=paste("Sc.", 1:16), las=2)
abline(v=2.5, col="red")

plot(as.numeric(as.character(Pow.Any.Mat[Pow.Any.Mat$Method==1,1])), 
     Pow.Any.Mat[Pow.Any.Mat$Method==1,3],   type="p", pch=20, 
     ylim=c(0,17), xlab="", ylab="", xaxt="n", yaxt="n", main = "Acceptable Power")
points(as.numeric(as.character(Pow.Any.Mat[Pow.Any.Mat$Method==2,1])), 
       Pow.Any.Mat[Pow.Any.Mat$Method==2,3], pch=4)
points(as.numeric(as.character(Pow.Any.Mat[Pow.Any.Mat$Method==3,1])), 
       Pow.Any.Mat[Pow.Any.Mat$Method==3,3], pch=2)
axis(1, at = seq(5,100,5), labels=paste(seq(5,100,5), "%", sep=""), las=2)
axis(2, at = 1:16, labels=paste("Sc.", 1:16), las=2)
abline(v=2.5, col="red")



dev.off()


