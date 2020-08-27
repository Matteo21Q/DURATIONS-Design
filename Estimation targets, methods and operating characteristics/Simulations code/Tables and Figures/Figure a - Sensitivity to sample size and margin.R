# Set working directory
setwd("//ad.ucl.ac.uk/homeu/rmjlmqu/Documents/Durations Design/Durations Design - Inference/Figures")
# Load the results 

# Start with base case (NI 10, 500):
load("//ad.ucl.ac.uk/homeu/rmjlmqu/Documents/Durations Design/Durations Design - Inference/Simulation Results/myriad/Bootstrap CI - Estimand RD.RData")
T1ER<-apply(T1ER.scen[1:200,],2,mean)
Power<-apply(Power.scen[1:200,],2,mean)
PowerTRUE<-apply(PowerTRUE.scen[1:200,],2,mean)

# Allocate space for matrix of results:
matrix.bp.pp<-matrix(NA,n.scen,6)
matrix.bp.fp<-matrix(NA,n.scen,6)
matrix.bp.t1e<-matrix(NA,n.scen,6)

matrix.bp.pp[,1]<-as.numeric(Power)
matrix.bp.fp[,1]<-as.numeric(PowerTRUE)
matrix.bp.t1e[,1]<-as.numeric(T1ER)

# Now NI 10, 750:
load("//ad.ucl.ac.uk/homeu/rmjlmqu/Documents/Durations Design/Durations Design - Inference/Simulation Results/myriad/Bootstrap CI 750 - Estimand RD.RData")
matrix.bp.pp[,2]<-as.numeric(Table[,2])
matrix.bp.fp[,2]<-as.numeric(Table[,3])
matrix.bp.t1e[,2]<-as.numeric(Table[,4])

# Now NI 10, 1000:
load("//ad.ucl.ac.uk/homeu/rmjlmqu/Documents/Durations Design/Durations Design - Inference/Simulation Results/myriad/Bootstrap CI 1000 - Estimand RD.RData")
matrix.bp.pp[,3]<-as.numeric(Table[,2])
matrix.bp.fp[,3]<-as.numeric(Table[,3])
matrix.bp.t1e[,3]<-as.numeric(Table[,4])

# Now NI 5, 500:
load("//ad.ucl.ac.uk/homeu/rmjlmqu/Documents/Durations Design/Durations Design - Inference/Simulation Results/myriad/Bootstrap CI 5NI - Estimand RD.RData")
T1ER<-apply(T1ER.scen[1:200,],2,mean)
Power<-apply(Power.scen[1:200,],2,mean)
PowerTRUE<-apply(PowerTRUE.scen[1:200,],2,mean)
matrix.bp.pp[,4]<-as.numeric(Power)
matrix.bp.fp[,4]<-as.numeric(PowerTRUE)
matrix.bp.t1e[,4]<-as.numeric(T1ER)

# Now NI 5, 750:
load("//ad.ucl.ac.uk/homeu/rmjlmqu/Documents/Durations Design/Durations Design - Inference/Simulation Results/myriad/Bootstrap CI 5NI750 - Estimand RD.RData")
matrix.bp.pp[,5]<-as.numeric(Table[,2])
matrix.bp.fp[,5]<-as.numeric(Table[,3])
matrix.bp.t1e[,5]<-as.numeric(Table[,4])

# Now NI 5, 1000:
load("//ad.ucl.ac.uk/homeu/rmjlmqu/Documents/Durations Design/Durations Design - Inference/Simulation Results/myriad/Bootstrap CI 5NI1000 - Estimand RD.RData")
matrix.bp.pp[,6]<-as.numeric(Table[,2])
matrix.bp.fp[,6]<-as.numeric(Table[,3])
matrix.bp.t1e[,6]<-as.numeric(Table[,4])

# Boxplot:
boxplot.matrix(matrix.bp.pp,las=2, ylab="Partial Power", xlab="Design")
boxplot.matrix(matrix.bp.fp,las=2, ylab="Full Power", xlab="Design")
boxplot.matrix(matrix.bp.t1e,las=2, ylab="Type 1 Error", xlab="Design")

# simple scatterplot:
pdf("Sensitivity ssNI.pdf", 13,7)
par(mfrow=c(1,2))
par(mar=c(8,4,3,2))
mat.pp<-c(as.matrix(matrix.bp.pp))*100
meth.pp<-rep(1:6, each=n.scen)
plot(meth.pp, mat.pp, xaxt = "n", pch=20, las=2, main="Acceptable Power", xlab="", ylab="Acceptable Power", xlim=c(0.5,6.3))
text(1.15,matrix.bp.pp[8,1]*100+0.3, "8")
text(2.15,matrix.bp.pp[7,2]*100+0.3, "7")
text(3.15,matrix.bp.pp[8,3]*100+0.3, "8")
text(4.15,matrix.bp.pp[14,4]*100+0.3, "14")
text(5.15,matrix.bp.pp[14,5]*100+0.3, "14")
text(6.15,matrix.bp.pp[14,6]*100+0.3, "14")
axis(1, at=1:6, labels= c(expression(paste(delta," = 10%, N=500")),expression(paste(delta," = 10%, N=750")), 
                          expression(paste(delta," = 10%, N=1000")),expression(paste(delta," = 5%, N=500")),
                          expression(paste(delta," = 5%, N=750")),expression(paste(delta," = 5%, N=1000"))), las=2)


par(mar=c(8,4,3,2))
mat.fp<-c(as.matrix(matrix.bp.fp))*100
meth.fp<-rep(1:6, each=n.scen)
plot(meth.fp, mat.fp, xaxt = "n", pch=20, las=2, main="Optimal Power", xlab="", ylab="Optimal Power", xlim=c(0.5,6.3))
text(1.15,matrix.bp.fp[4,1]*100+0.3, "4")
text(2.15,matrix.bp.fp[4,2]*100+0.3, "4")
text(3.15,matrix.bp.fp[4,3]*100+0.3, "4")
text(4.15,matrix.bp.fp[4,4]*100+0.3, "4")
text(5.15,matrix.bp.fp[4,5]*100+0.3, "4")
text(6.15,matrix.bp.fp[4,6]*100+0.3, "4")
axis(1, at=1:6, labels= c(expression(paste(delta," = 10%, N=500")),expression(paste(delta," = 10%, N=750")), 
                          expression(paste(delta," = 10%, N=1000")),expression(paste(delta," = 5%, N=500")),
                          expression(paste(delta," = 5%, N=750")),expression(paste(delta," = 5%, N=1000"))), las=2)
dev.off()

par(mar=c(8,4,3,2))
mat.t1e<-c(as.matrix(matrix.bp.t1e))*100
meth.t1e<-rep(1:6, each=n.scen)
plot(meth.t1e, mat.t1e, xaxt = "n", pch=20, las=2, main="Type 1 Error", xlab="", ylab="Type 1 Error", xlim=c(0.5,6.3))
# text(1.15,matrix.bp.t1e[8,1]*100+0.3, "8")
# text(2.15,matrix.bp.t1e[7,2]*100+0.3, "7")
# text(3.15,matrix.bp.t1e[8,3]*100+0.3, "8")
# text(4.15,matrix.bp.t1e[14,4]*100+0.3, "14")
# text(5.15,matrix.bp.t1e[14,5]*100+0.3, "14")
# text(6.15,matrix.bp.t1e[14,6]*100+0.3, "14")
axis(1, at=1:6, labels= c(expression(paste(delta,"=10%, N=500")),expression(paste(delta,"=10%, N=750")), 
                          expression(paste(delta,"=10%, N=1000")),expression(paste(delta,"=5%, N=500")),
                          expression(paste(delta,"=5%, N=750")),expression(paste(delta,"=5%, N=1000"))),
                          las=2)
