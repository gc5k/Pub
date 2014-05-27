mydata=read.table("B_checkerboard.txt", header=T)
mat=matrix(0,11,11)
mat[c(1:5),c(7:11)]=1
mat[6,c(7:11)]=2
mat[7,c(7:11)]=3
mat[8,c(7:11)]=4
mat[9,c(7:11)]=5
mat[10,c(7:11)]=6
mat[11,c(7:11)]=7
mat[c(1:5),1]=13
mat[c(1:5),2]=12
mat[c(1:5),3]=11
mat[c(1:5),4]=10
mat[c(1:5),5]=9
mat[c(1:5),6]=8
mat[6,6]=14
mat[7,5]=15
mat[8,4]=16
mat[9,3]=17
mat[10,2]=18
mat[11,1]=19
layout(mat)
layout.show(19)
par(mar=c(0.3,0.3,4,4))
par(las=1)
par(ps=10)
par(pch=1)
plot(mydata$Rab_power.0.05/200,mydata$Ped_power.0.05/200,xlim=c(0,1),ylim=c(0,1),axes=FALSE, lwd=2,col="green")
abline(0,1,col="grey")
abline(0.2,1,col="gray",lty=2)
abline(-0.2,1, col="grey",lty=2)
axis(1,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,xlim=c(0,1),tck=0.015)
axis(2,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,ylim=c(1,0),tck=0.015)
axis(3,seq(0,1,0.2),tck=0.015, labels=FALSE,)
axis(3,seq(0,1,0.2),tck=-0.015, line=1)
mtext("Power of PII", side=3, line = 3)
axis(4,seq(0,1,0.2),tck=0.015, labels=FALSE)
axis(4,seq(0,1,0.2),tck=-0.015, line=1,las=3)
mtext("Power of PI", side=4, line = 3,las=0)
box()

par(mar=c(0.2,0.3,0.2,4))
par(pch=1)
plot(mydata$Rab_power.0.05/200,mydata$ALFreq,axes=FALSE,bty="l",col="brown",xlim=c(0,1),ylim=c(0.5,0.1))
axis(1,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,xlim=c(1,0),tck=0.05)
axis(3,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,xlim=c(1,0),tck=0.05)
axis(2,c(0.1,0.25,0.5),labels=FALSE,ylim=c(0.5,0.1),tck=0.05)
axis(4,c(0.1,0.25,0.5),labels=c("0.1","0.25","0.50"),ylim=c(0.5,0.1),tck=0.05)
box()

par(pch=1)
plot(mydata$Rab_power.0.05/200,mydata$MisRate,axes=FALSE,bty="l",col="cyan",xlim=c(0,1),ylim=c(1,0))
axis(1,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,xlim=c(1,0),tck=0.05)
axis(3,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,xlim=c(1,0),tck=0.05)
axis(2,c(0,0.25,0.50,0.75,1),labels=FALSE,ylim=c(1,0),tck=0.05)
axis(4,c(0,0.25,0.50,0.75,1),labels=c("0.00","0.25","0.50","0.75","1.00"),ylim=c(1,0),xlim=c(0,1),tck=0.05)
box()

par(pch=1)
plot(mydata$Rab_power.0.05/200,mydata$Cov,axes=FALSE,bty="l",col="red",ylim=c(1,0.25),xlim=c(0,1))
axis(1,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,xlim=c(1,0),tck=0.05)
axis(3,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,xlim=c(1,0),tck=0.05)
axis(2,c(0.25,0.5,0.75,1),labels=FALSE,tck=0.05)
axis(4,c(0.25,0.5,0.75,1),labels=c("0.25","0.50","0.75","1.00"),tck=0.05)
box()

par(pch=1)
plot(x=NULL, y=NULL, axes=FALSE,bty="l",col="blue",ylim=c(1,0.25),xlim=c(0,1))
polygon(c(-0.01,0.045,0.045,-0.01),c(0.20,0.20,0.35,0.35), col="gray", border=FALSE)
polygon(c(0.02,0.26,0.26,0.02),c(0.40,0.40,0.6,0.6), col="gray", border=FALSE)
polygon(c(0.3,0.725,0.725,0.3),c(0.65,0.65,0.85,0.85), col="gray", border=FALSE)
polygon(c(0.685,0.975,0.975,0.685),c(0.9,0.9,1.1,1.1), col="gray", border=FALSE)
points(mydata$Rab_power.0.05/200,mydata$Gene,col="blue")
#plot(mydata$Rab_power.0.05/200,mydata$Gene,axes=FALSE,bty="l",col="blue",ylim=c(1,0.25),xlim=c(0,1))
axis(1,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,xlim=c(1,0),tck=0.05)
axis(3,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,xlim=c(1,0),tck=0.05)
axis(2,c(0.25,0.5,0.75,1), labels=FALSE,tck=0.05)
axis(4,c(0.25,0.5,0.75,1), labels=c("0.25","0.50","0.75","1.00"),tck=0.05)
box()

par(pch=1)
plot(mydata$Rab_power.0.05/200,mydata$scheme,axes=FALSE,bty="l",col="purple",ylim=c(3,0),xlim=c(0,1))
axis(1,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,xlim=c(1,0),tck=0.05)
axis(1,c(0,0.2,0.4,0.6,0.8,1),xlim=c(1,0),tck=-0.05, line=1)
axis(3,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,xlim=c(1,0),tck=0.05)
axis(2,c(0,1,2,3), labels=FALSE,tck=0.05)
axis(4,c(0,1,2,3), labels=c(0,1,2,3),tck=0.05)
box()

plot.new()



par(las=2)
par(mar=c(0.3,0.2,4,0.2))
par(pch=1)

plot(mydata$ALFreq,mydata$Ped_power.0.05/200,axes=FALSE,bty="l",col="brown",xlim=c(0.5,0.1),ylim=c(0,1))
axis(2,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,ylim=c(1,0),tck=0.05)
axis(4,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,ylim=c(1,0),tck=0.05)
axis(1,c(0.1,0.25,0.5),labels=FALSE,tck=0.05)
axis(3,c(0.1,0.25,0.5),labels=c("0.10","0.25","0.50"),xlim=c(0.5,0.1),tck=0.05)
box()

plot(mydata$MisRate,mydata$Ped_power.0.05/200,axes=FALSE,bty="l",col="cyan",xlim=c(1,0),ylim=c(0,1))
axis(2,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,ylim=c(1,0),tck=0.05)
axis(4,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,ylim=c(1,0),tck=0.05)
axis(1,c(0,0.25,0.50,0.75,1),labels=FALSE,tck=0.05)
axis(3,c(0,0.25,0.50,0.75,1),labels=c("0.00","0.25","0.50","0.75","1.00"),tck=0.05)
box()

par(pch=1)
plot(mydata$Cov,mydata$Ped_power.0.05/200,axes=FALSE,bty="l",col="red",xlim=c(1,0.25),ylim=c(0,1))
axis(2,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,ylim=c(1,0),tck=0.05)
axis(4,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,ylim=c(1,0),tck=0.05)
axis(1,c(0,0.25,0.5,0.75,1),labels=FALSE,tck=0.05)
axis(3,c(0,0.25,0.5,0.75,1),labels=c("0.00","0.25","0.50","0.75","1.00"),tck=0.05)
box()

par(pch=1)
plot(mydata$Gene,mydata$Ped_power.0.05/200,axes=FALSE,bty="l",col="blue",xlim=c(1,0.25),ylim=c(0,1))
axis(2,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,ylim=c(1,0),tck=0.05)
axis(4,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,ylim=c(1,0),tck=0.05)
axis(1,c(0,0.25,0.5,0.75,1),labels=FALSE,tck=0.05)
axis(3,c(0,0.25,0.5,0.75,1),labels=c("0.00","0.25","0.50","0.75","1.00"),tck=0.05)
box()

par(pch=1)
plot(mydata$scheme,mydata$Ped_power.0.05/200,axes=FALSE,bty="l",col="purple",xlim=c(3,0),ylim=c(0,1))
axis(2,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,ylim=c(1,0),tck=0.05)
axis(2,c(0,0.2,0.4,0.6,0.8,1),ylim=c(1,0),tck=-0.05, line=1)
axis(4,c(0,0.2,0.4,0.6,0.8,1),labels=FALSE,ylim=c(1,0),tck=0.05)
axis(1,c(0,1,2,3),labels=FALSE,tck=0.05)
axis(3,c(0,1,2,3),labels=c(0,1,2,3),tck=0.05)
box()
plot.new();

par(mar=c(0.2,0.2,0.2,0.2))
plot.new()
text(0.5,y=0.5,"MAF", col="brown")
box();
plot.new();
text(0.5,y=0.5,"Missing\nRate",col="cyan")
box()
plot.new()
text(0.5,0.5,"Covariate",col="red")
box()
plot.new()
text(0.5,0.5,"Interaction",col="blue")
box()
plot.new()
text(0.5,0.5,"Adjustment\nScheme",col="purple")
box()

