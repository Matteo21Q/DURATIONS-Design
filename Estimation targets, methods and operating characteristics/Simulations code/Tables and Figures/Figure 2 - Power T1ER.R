# Set working directory
setwd("~/Documents/Durations Design/Durations Design - Inference/Figures/")

# Load the results 

# Start with model confidence bands method
load("~/Documents/Durations Design/Durations Design - Inference/Simulation Results/myriad/Model Confidence Bands - Estimand RD.RData")
duration.recommended[is.na(duration.recommended)]<-max.dur
Model.CB<-data.frame(Table)
Model.CB[,-1]<-sapply(Model.CB[,-1], as.character)
Model.CB[,-1]<-sapply(Model.CB[,-1], as.numeric)
Model.CB[,6]<-round(Model.CB[,6], digits=3)
Model.CB[,2:4]<-Model.CB[,2:4]*100
Model.CB<-cbind(Model.CB, apply(duration.recommended,2,quantile, probs=0.025, na.rm=T))
Model.CB<-Model.CB[,c(1:4,6,7,10,8)]
colnames(Model.CB)<-c("Scenario", "Power (any)", "Power (true)", "T1ER",
                      "Real Min Duration","min", "2.5th Perc", "median")

# Now delta method mfp
load("~/Documents/Durations Design/Durations Design - Inference/Simulation Results/myriad/Delta Method CI - Estimand RD.RData")
duration.recommended[is.na(duration.recommended)]<-max.dur
Delta.Method<-data.frame(Table)
Delta.Method[,-1]<-sapply(Delta.Method[,-1], as.character)
Delta.Method[,-1]<-sapply(Delta.Method[,-1], as.numeric)
Delta.Method[,6]<-round(Delta.Method[,6], digits=3)
Delta.Method[,2:4]<-Delta.Method[,2:4]*100
Delta.Method<-cbind(Delta.Method, apply(duration.recommended,2,quantile, probs=0.025, na.rm=T))
Delta.Method<-cbind(Delta.Method, apply(duration.recommended,2,quantile, probs=0.025, na.rm=T))
Delta.Method<-Delta.Method[,c(1:4,6,7,10,8)]
colnames(Delta.Method)<-c("Scenario", "Power (any)", "Power (true)", "T1ER",
                       "Real Min Duration","min", "2.5th Perc", "median")

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

# Now bootstrap duration MFP
load("~/Documents/Durations Design/Durations Design - Inference/Simulation Results/myriad/Bootstrap CI MFP - Estimand RD.RData")
duration.recommended[is.na(duration.recommended)]<-max.dur
Bootstrap.dur.MFP<-data.frame(Table)
Bootstrap.dur.MFP[,-1]<-sapply(Bootstrap.dur.MFP[,-1], as.character)
Bootstrap.dur.MFP[,-1]<-sapply(Bootstrap.dur.MFP[,-1], as.numeric)
Bootstrap.dur.MFP[,6]<-round(Bootstrap.dur.MFP[,6], digits=3)
Bootstrap.dur.MFP[,2:4]<-Bootstrap.dur.MFP[,2:4]*100
Bootstrap.dur.MFP<-cbind(Bootstrap.dur.MFP, apply(duration.recommended,2,quantile, probs=0.025, na.rm=T))
Bootstrap.dur.MFP<-cbind(Bootstrap.dur.MFP, apply(duration.recommended,2,quantile, probs=0.025, na.rm=T))
Bootstrap.dur.MFP<-Bootstrap.dur.MFP[,c(1:4,6,7,10,8)]
colnames(Bootstrap.dur.MFP)<-c("Scenario", "Power (any)", "Power (true)", "T1ER",
                          "Real Min Duration","min", "2.5th Perc", "median")

# Matrix of powers and Type 1 errors:

Pow.Any.Mat<-data.frame(c(Model.CB[,"Power (any)"], Delta.Method[,"Power (any)"],
                          Bootstrap.CI[,"Power (any)"], Bootstrap.dur.CI[,"Power (any)"],
                        Bootstrap.dur.MFP[,"Power (any)"]), rep(1:5,each=n.scen))
colnames(Pow.Any.Mat)<-c("Power (any)", "Method")
Pow.True.Mat<-data.frame(c(Model.CB[,"Power (true)"], Delta.Method[,"Power (true)"],
                           Bootstrap.CI[,"Power (true)"], Bootstrap.dur.CI[,"Power (true)"],
                           Bootstrap.dur.MFP[,"Power (true)"]), rep(1:5,each=n.scen))
colnames(Pow.True.Mat)<-c("Power (true)", "Method")
T1ER.Mat<-data.frame(c(Model.CB[,"T1ER"],  Delta.Method[,"T1ER"],
                       Bootstrap.CI[,"T1ER"], Bootstrap.dur.CI[,"T1ER"],
                       Bootstrap.dur.MFP[,"T1ER"]), rep(1:5,each=n.scen))
colnames(T1ER.Mat)<-c("T1ER", "Method")
T1ER.Mat$scen<-1:16
Pow.Any.Mat$scen<-1:16
T1ER.Mat$colo<-c("black", "black", "red", rep("black",3), "red", "red", 
                 rep("black",5), "red", "black", "black")
Pow.Any.Mat$colo<-rep(c("black", "red"), each=8)

#Outliers
Outliers<-T1ER.Mat[T1ER.Mat$T1ER>15,]
OutliersPA<-Pow.Any.Mat[Pow.Any.Mat[,1]<70,]

#Plot:

pdf(file="T1ER - Power.pdf",width=12,height=7)

par(mar=c(9,4,4,2), mfrow=c(1,2))
plot(T1ER.Mat[,2], as.numeric(as.character(T1ER.Mat[,1])),  
     type="p", pch=20, ylim=c(0,60), xlab="", ylab="", xaxt="n", yaxt="n", 
     main = "Type 1 error", col=T1ER.Mat[,"colo"])
axis(1, at = 1:5, labels=c("Confidence Bands",  "Delta Method", "Bootstrap CI", "Boostrap Duration", "Bootstrap MFP"), las=2)
axis(2, at = seq(5,60,5), labels=paste(seq(5,60,5), "%", sep=""), las=2)
abline(h=2.5, col="red")
for (i in 1:nrow(Outliers)) {
  text(Outliers$Method[i],Outliers$T1ER[i]+1.5, Outliers$scen[i], col="red")
  
}

# Calculate cure rate at 2.5th percentile of recommended durations

plot(Pow.Any.Mat[,2], as.numeric(as.character(Pow.Any.Mat[,1])),  
     type="p", pch=18, ylim=c(0,100), xlab="", ylab="", xaxt="n", yaxt="n", 
     main = "Acceptable Power", col=Pow.Any.Mat[,"colo"])
axis(1, at = 1:5, labels=c("Confidence Bands",  "Delta Method", "Bootstrap CI", "Boostrap Duration", "Bootstrap MFP"), las=2)
axis(2, at = seq(0,100,10), labels=paste(seq(0,100,10), "%", sep=""), las=2)
for (i in 1:nrow(OutliersPA)) {
  if (OutliersPA$scen[i]==14) {
    text(OutliersPA$Method[i],OutliersPA[i,1]+2.5, OutliersPA$scen[i], col="red")
  } else {
    text(OutliersPA$Method[i],OutliersPA[i,1]-2.5, OutliersPA$scen[i])
  }
  
}

dev.off()

# Write tables as .txt

write.table(Model.CB, file = "Model.CB.txt", sep = ",", quote = FALSE, row.names = F)
write.table(Delta.Method, file = "Delta.Method.txt", sep = ",", quote = FALSE, row.names = F)
write.table(Bootstrap.dur.MFP, file = "Bootstrap.dur.MFP.txt", sep = ",", quote = FALSE, row.names = F)
write.table(Bootstrap.CI, file = "Bootstrap.CI.txt", sep = ",", quote = FALSE, row.names = F)
write.table(Bootstrap.dur.CI, file = "Bootstrap.dur.CI.txt", sep = ",", quote = FALSE, row.names = F)

