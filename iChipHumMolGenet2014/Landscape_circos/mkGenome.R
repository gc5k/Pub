library(ggbio)
library(grid)
library(gridExtra)
library(plyr)
library(GenomicRanges)
library(qgraph)

LEN=matrix(0, 22, 1)
Threshold=1.2
for(i in 1:22) {
  fn=read.table(paste0("Chr",i, ".txt"), as.is=T)
  fn$V8=seq(1, nrow(fn))
  fn$V9=rep(nrow(fn), nrow(fn))
  idxM=which(fn$V7 > Threshold*mean(fn$V7))
  fn$V10=0
  fn$V10[idxM]=1
  LEN[i,1]=nrow(fn)
  if(i == 1) {
    MM=as.matrix(fn)
  } else {
    MM=rbind(MM, as.matrix(fn))
  }
}

seqinfo <- Seqinfo(paste("Chr", 1:22), LEN[,1], NA, "iChip")
gr <- GRanges(seqnames =
          Rle(paste("Chr", 1:22), LEN[,1]),
          ranges = IRanges(start = as.vector(MM[,8]),width = 1),
          strand = rep("+", nrow(MM)),
          gscore = MM[,6],
          iscore = MM[,7],
          flag = MM[,10],
          seqinfo=seqinfo)

seqinfo <- Seqinfo(paste0("", 1:22), LEN[,1], NA, "iChip")
gr1 <- GRanges(seqnames = paste0("", 1:22),
              ranges = IRanges(start = rep(1,22),width = LEN[,1]),
              strand = rep("+", 22),
              bin=paste0("",LEN[,1]),
              seqinfo=seqinfo)

FM=MM[MM[,10]==1,]
seqinfo <- Seqinfo(paste0("", 1:22), LEN[,1], NA, "iChip")
gr2 <- GRanges(seqnames = Rle(paste0("", 1:22), as.vector(table(FM[,1]))),
               ranges = IRanges(start = as.vector(FM[,8]), width = 1),
               strand = rep("+", nrow(FM)),
               iscore = FM[,7],
               seqinfo=seqinfo)

ggplot() + layout_circle(gr, geom = "ideo", color="gray", fill = "gray", radius = 30, trackWidth = 4) +
  layout_circle(gr1, geom="text", aes(label=seqnames), vjust = 0, radius = 32, trackWidth = 7) +
  layout_circle(gr, geom="bar", radius=15, trackWidth=15, color="black", fill="black", aes(y=iscore)) +
  layout_circle(gr, geom="bar", radius=12, trackWidth=3, color="blue", fill="blue", aes(y=gscore)) +
  layout_circle(gr2, geom="bar", radius=15, trackWidth=15, color="red", fill="red", aes(y=iscore))
