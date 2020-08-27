# Set working directory
setwd("~/Documents/Durations Design/Durations Design - Inference/Figures/")

# Load the results 

# Now bootstrap CI
load("~/Documents/Durations Design/Durations Design - Inference/Simulation Results/myriad/Bootstrap CI - Estimand RD.RData")
duration.recommended2[is.na(duration.recommended2)]<-max.dur
Bootstrap.CI<-data.frame(Table2)
Bootstrap.CI[,-1]<-sapply(Bootstrap.CI[,-1], as.character)
Bootstrap.CI[,-1]<-sapply(Bootstrap.CI[,-1], as.numeric)
Bootstrap.CI[,6]<-round(Bootstrap.CI[,6], digits=3)
Bootstrap.CI[,2:4]<-Bootstrap.CI[,2:4]*100
Bootstrap.CI<-cbind(Bootstrap.CI, apply(duration.recommended2,2,quantile, probs=0.025, na.rm=T))
Bootstrap.CI<-Bootstrap.CI[,c(1:4,6,7,10,8)]
colnames(Bootstrap.CI)<-c("Scenario", "Power (any)", "Power (true)", "T1ER",
                          "Real Min Duration","min", "2.5th Perc", "median")

# Now bootstrap duration CI
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


# Matrix of powers and Type 1 errors:

Pow.Any.Mat<-data.frame(c(Bootstrap.CI[,"Power (any)"], Bootstrap.dur.CI[,"Power (any)"]),
                          rep(1:2,each=n.scen))
colnames(Pow.Any.Mat)<-c("Power (any)", "Method")
Pow.True.Mat<-data.frame(c(Bootstrap.CI[,"Power (true)"], Bootstrap.dur.CI[,"Power (true)"]),
                           rep(1:2,each=n.scen))
colnames(Pow.True.Mat)<-c("Power (true)", "Method")
T1ER.Mat<-data.frame(c(Bootstrap.CI[,"T1ER"], Bootstrap.dur.CI[,"T1ER"]),
                       rep(1:2,each=n.scen))
colnames(T1ER.Mat)<-c("T1ER", "Method")
T1ER.Mat$scen<-1:16
Pow.Any.Mat$scen<-1:16

#Plot:

pdf(file="Bootstrap comparison.pdf",width=12,height=7)

par(mar=c(9,4,4,2), mfrow=c(1,2))
plot(as.numeric(as.character(T1ER.Mat[T1ER.Mat$Method==1,1])), 
     T1ER.Mat[T1ER.Mat$Method==1,3],   type="p", pch=20, 
     ylim=c(0,17), xlab="", ylab="", xaxt="n", yaxt="n", main = "Type 1 error")
points(as.numeric(as.character(T1ER.Mat[T1ER.Mat$Method==2,1])), 
       T1ER.Mat[T1ER.Mat$Method==2,3], pch=4)
axis(1, at = seq(1,10,1), labels=paste(seq(1,10,1), "%", sep=""), las=2)
axis(2, at = 1:16, labels=paste("Sc.", 1:16), las=2)
abline(v=2.5, col="red")

plot(as.numeric(as.character(Pow.Any.Mat[Pow.Any.Mat$Method==1,1])), 
     Pow.Any.Mat[Pow.Any.Mat$Method==1,3],   type="p", pch=20, 
     ylim=c(0,17), xlab="", ylab="", xaxt="n", yaxt="n", main = "Acceptable Power")
points(as.numeric(as.character(Pow.Any.Mat[Pow.Any.Mat$Method==2,1])), 
       Pow.Any.Mat[Pow.Any.Mat$Method==2,3], pch=4)
axis(1, at = seq(5,100,5), labels=paste(seq(5,100,5), "%", sep=""), las=2)
axis(2, at = 1:16, labels=paste("Sc.", 1:16), las=2)
abline(v=2.5, col="red")



dev.off()


