source("~/R/MyLib/shotgun.R")
postscript("Fig3.eps", width=6.5, height=4, horizontal=F, paper="a4", pointsize=7)


COL=c(rep("gray30", 22))
mat=matrix(1:2, 2, 1)
layout(mat)
#cl=c(247,243,200,191,181,171,159,146,140,135,134,132,114,106,100,89,79,76,64,62,47,50)
set1CD=read.table("cd_set1.txt", as.is=T)
set2CD=read.table("cd_set2.txt", as.is=T)

avg=set1CD
avg$V2=avg$V2/1000
avg$V4=Hong23(0.005, 7615, 12053)*(set1CD$V3+set2CD$V3)/2
plot(cex.main=0.8, x=NULL, y=NULL, axes=F, xlab="", ylab="", xlim=c(0, max(avg$V2)), ylim=c(-0.002, max(avg$V4)*1.2), main="Joint analysis for all chromosomes simulataneously for iChip CD")
reg=lm(avg$V4~avg$V2)
abline(a=reg$coefficients[1], b=reg$coefficients[2], lwd=0.2)
points(avg$V2, avg$V4, pch=19,  col=COL, cex=4)
text(avg$V2, avg$V4, avg$V1, col="white")
text(x=2, y=max(avg$V4)*1.1, paste("y=", format(reg$coefficients[1], digits=2), "+", format(reg$coefficients[2], digits=2), "x+e, ", "R-sq=", format(summary(reg)$r.squared, digits=2), sep=""))

axis(side=1, at=c(1, 5, 10, 16), labels=c("1", "5", "10", "16"), lwd=0.2)
mtext("Number of SNPs (X 1000)", side=1, line=2.5)
axis(side=2, at=c(-0.02, 0, 0.01, 0.02, 0.03, 0.04), labels=c("", "0", "0.01", "0.02", "0.03", "0.04"), lwd=0.2)
mtext("SNP-heritability in liability", side=2, line=3)


set1UC=read.table("uc_set1.txt", as.is=T)
set2UC=read.table("uc_set2.txt", as.is=T)

avg=set1UC
avg$V2=avg$V2/1000
avg$V4=Hong23(0.0025, 5904, 12053)*(set1UC$V3+set2UC$V3)/2
plot(cex.main=0.8, x=NULL, y=NULL, axes=F, xlab="", ylab="", xlim=c(0, max(avg$V2)), ylim=c(-0.004, max(avg$V4)*1.2), main="Joint analysis for all chromosomes simulataneously for iChip UC")
reg=lm(avg$V4~avg$V2)
abline(a=reg$coefficients[1], b=reg$coefficients[2], lwd=0.2)
points(avg$V2, avg$V4, pch=19, col=COL, cex=4)
text(avg$V2, avg$V4, avg$V1, col="white")
text(x=2, y=max(avg$V4)*1.1, paste("y=", format(reg$coefficients[1], digits=2), "+", format(reg$coefficients[2], digits=2), "x+e, ", "R-sq=", format(summary(reg)$r.squared, digits=2), sep=""))
#legend(0, max(avg$V4), c("Chr 1-5", "Chr 6-10", "Chr 11-15", "Chr 16-20", "Chr 21-22"), pt.bg=CC, col=CC, pch=21, pt.lwd=5, border=NULL)

axis(side=1, at=c(1, 5, 10, 16), labels=c("1", "5", "10", "16"), lwd=0.2)
mtext("Number of SNPs (X 1000)", side=1, line=2.5)
axis(side=2, at=c(-0.02, 0, 0.01, 0.02, 0.03, 0.04), labels=c("", "0", "0.01", "0.02", "0.03", "0.04"), lwd=0.2)
mtext("SNP-heritability in liability", side=2, line=3)


dev.off()