postscript("Fig2.eps", width=6.3, height=4, horizontal=F, paper="a4")
par(cex=0.5)

dat=read.table("Fig2.txt", header=T, as.is=T)
COL=c("grey90", "grey70", "grey50", "grey30")
barplot(axes=F, as.matrix(dat)[,1:2], beside=T, ylim=c(0, 1), names.arg=c("",""), col=COL, border=NA, ylab="Proportion of variance in liability")
text((1:4+0.2), dat[,1]+0.05, dat[,1])
text((6:9+0.2), dat[,2]+0.05, dat[,2])
axis(side=1, at=c(1, 2, 3, 4, 5), labels=c("", "", "", "", ""), line=1, lwd=0.1)
mtext("CD", side=1, line=2.5, at=3, cex=0.5)

axis(side=1, at=c(6, 7, 8, 9, 10), labels=c("", "", "", "", ""), line=1, lwd=0.1)
mtext("UC", side=1, line=2.5, at=8, cex=0.5)
axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1), labels=c(0, 0.2, 0.4, 0.6, 0.8,1), lwd=0.1)
LX1=matrix(c(c(1:4+0.5),c(1:4+0.5)), nrow=4, ncol=2)
LX2=matrix(c(c(6:9+0.5),c(6:9+0.5)), nrow=4, ncol=2)
LY1=matrix(c(dat[,1]-dat[,3], dat[,1]+dat[,3]), nrow=4, ncol=2)
LY2=matrix(c(dat[,2]-dat[,4], dat[,2]+dat[,4]), nrow=4, ncol=2)

for(i in 2:nrow(LX1))
{
  lines(LX1[i,], LY1[i,], lwd=0.4)
  lines(LX2[i,], LY2[i,], lwd=0.4)
}
legend(x=1, y=0.95, bty="n", pch=15, legend=c("GWAS hits (Jostins's)", "iChip", "Generic GWAS array", "Twin studies"), col=COL, horiz=F)

dev.off()
