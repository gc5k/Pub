require(graphics)
mat=matrix(1:25, 5, 5)
mat[1, 1]=1
mat[1, 2:5]=2

mat[2, 1]=3
mat[2, 2:5]=4

mat[3, 1]=5
mat[3, 2:5]=6

mat[4, 1]=7
mat[4, 2:5]=8

mat[5, 1]=9
mat[5, 2:5]=10

layout(mat)
layout.show(10)
TX=c("Design I", "Design II", "Design III", "Design IV")
SC=c("0scoreDesignI.txt","0scoreDesignII.txt","0scoreDesignIII.txt","0scoreDesignIV.txt")
PH=c("0pheDesignI.txt","0pheDesignII.txt","0pheDesignIII.txt","0pheDesignIV.txt")
par(mar=c(0.5, 0.5, 0.5, 0.5))
par(ps=11)
for(i in 1:4) {
  plot.new()
  score=read.table(SC[i], header=T)
  phe=read.table(PH[i], header=T)
  spc1=-1*scale(score$pc1)
  s1=min(spc1)
  r1=max(spc1)-min(spc1)
  model1=lm(phe$phe1~spc1)
  plot(spc1, phe$phe1, col=rgb(1-(spc1-s1)/r1, (spc1-s1)/r1, (spc1-s1)/r1), xlim=c(-2, 6), ylim=c(0,1), axes=F)
  axis(side=1, at=seq(-2, 6, 1), xlim=c(-2, 6), labels=F, tck=0.03)
  axis(side=3, at=seq(-2, 6, 1), xlim=c(-2, 6), labels=F, tck=0.03)
  axis(side=2, at=seq(0, 1, 0.5), xlim=c(0, 1), labels=F, tck=0.03)
  axis(side=4, at=seq(0, 1, 0.5), xlim=c(0, 1), labels=F, tck=0.03)

  axis(side=2, at=seq(0, 1, 0.5), xlim=c(0, 1), labels=F, tck=-0.03, line=1)
  model=lm(phe$phe1~spc1)
  mtext(side=2, at=seq(0, 1, 0.5), text=seq(0, 1, 0.5), las=1, line=1.5)
  mtext(side=2, text=c("True Ancestry"), las=1, line=3.5)
  mtext(side=4, at=c(0.8), text=TX[i], las=1, line=-5)
  
  mtext(side=4, at=c(0.5), text=c(expression(R^2), paste("    =", format(summary(model)$r.squared,digits=3))), las=1, line=-5)
#  mtext(side=4, at=c(0.5), text=paste(eval(t), "=", format(summary(model)$r.squared,digits=3)), las=1, line=-3)
  box()
}
plot.new()
plot(x=NULL, y=NULL,xlim=c(-2, 6), ylim=c(0, 1), axes=FALSE, type="n", ylab="")
axis(side=3, at=seq(-2, 6, 1), xlim=c(-2, 6), labels=F, tck=0.05, line=0)
mtext(side=3, at=seq(-2, 6, 1), text=seq(-2, 6, 1), las=1, line=-1.2)
mtext(side=3, text=c("PC1"), las=1, line=-2.5)
