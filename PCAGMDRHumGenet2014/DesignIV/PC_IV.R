score=read.table("0score.txt", header=T)
phe=read.table("0phe.txt", header=T)
ped=read.table("0ped.txt", header=T)
mat=matrix(1:5, 5, 1)
mat[1:2,1]=1
mat[3:4,1]=2
mat[5,1]=3

layout(mat)
par(mar=c(0.5, 7, 1, 0.5))

d1=scale(-1*score[,5])
d2=scale(-1*score[,6])

s1=min(d1)
r1=max(d1)-min(d1)
s2=min(d2)
r2=max(d2)-min(d2)

par(ps=12)
f1=which(ped[,3]==0)
plot(ylim=c(-4, 4), xlim=c(-2, 6), d1[f1], d2[f1],col=rgb(1-(d1[f1]-s1)/r1, (d1[f1]-s1)/r1, (d1[f1]-s1)/r1), axes=F, xlab="", ylab="")
axis(side=1, at=seq(-2, 6, 1), xlim=c(-2, 6), labels=F, tck=0.02)
axis(side=3, at=seq(-2, 6, 1), xlim=c(-2, 6), labels=F, tck=0.02)
axis(side=2, at=seq(-4, 4, 2), xlim=c(-4, 4), labels=F, tck=0.02)
axis(side=4, at=seq(-4, 4, 2), xlim=c(-4, 4), labels=F, tck=0.02)

axis(side=2, at=seq(-4, 4, 2), xlim=c(-4, 4), labels=F, tck=-0.02, line=1)
mtext(side=2, at=seq(-4, 4, 2), text=seq(-4, 4, 2), las=1, line=1.5)
mtext(side=2, text=c("PC2"), las=3, line=3.5)
mtext(side=4, at=c(3), text="Design IV: unrelated", las=1, line=-10)
box()

f2=which(ped[,3]!=0)
plot(ylim=c(-4, 4), xlim=c(-2, 6), d1[f2], d2[f2], col=rgb(1-(d1[f2]-s1)/r1, (d1[f2]-s1)/r1, (d1[f2]-s1)/r1), axes=F, xlab="", ylab="")
axis(side=1, at=seq(-2, 6, 1), xlim=c(-2, 6), labels=F, tck=0.02)
axis(side=3, at=seq(-2, 6, 1), xlim=c(-2, 6), labels=F, tck=0.02)
axis(side=2, at=seq(-4, 4, 2), xlim=c(-4, 4), labels=F, tck=0.02)
axis(side=4, at=seq(-4, 4, 2), xlim=c(-4, 4), labels=F, tck=0.02)

axis(side=2, at=seq(-4, 4, 2), xlim=c(-4, 4), labels=F, tck=-0.02, line=1)
mtext(side=2, at=seq(-4, 4, 2), text=seq(-4, 4, 2), las=1, line=1.5)
mtext(side=2, text=c("PC2"), las=3, line=3.5)
mtext(side=4, at=c(3), text="Design IV: children", las=1, line=-10)
box()

plot(x=NULL, y=NULL, xlim=c(-2, 6), ylim=c(-4, 4), axes=FALSE, type="n", ylab="")
axis(side=3, at=seq(-2, 6, 1), xlim=c(-2, 6), labels=F, tck=0.05, line=0.5)
mtext(side=3, at=seq(-2, 6, 1), text=seq(-2, 6, 1), las=1, line=-1.2)
mtext(side=3, text=c("PC1"), las=1, line=-2.5)
