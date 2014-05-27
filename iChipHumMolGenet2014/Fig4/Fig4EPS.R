dat=read.table("plot.txt", header=T, as.is=T)
idx=which(dat$Component==1)
dat=dat[idx,]
dat$DATA=paste0(dat$Chip, dat$Disease)

mat=matrix(dat$Va, 3, 4)

postscript("Fig4.eps", width=6.3, height=4, horizontal=F, paper="a4")

par(cex=0.5)
COL=c("grey90", "grey60", "grey30")
barplot(mat, beside=T, col=COL, border=NA, ylim=c(0, 0.8), axes=F, ylab="Proportion of SNP-heritability explained")
axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8), labels=c("0", "0.2", "0.4", "0.6", "0.8"), lwd=0.2)

axis(side=1, at=c(2.5, 6.5, 10.5, 14.5), labels=c("GWAS CD", "GWAS UC", "iChip CD", "iChip UC"), line=1, lwd=0.2)

legend(x=1, y=0.78, bty="n", pch=15, legend=c("Type 1 (High-density regions)", "Type 2 (GWAS-hit regions)", "Type 3 (HD-GWAS regions)"), col=COL, horiz=T)

dev.off()
