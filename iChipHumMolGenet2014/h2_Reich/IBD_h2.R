args=commandArgs(TRUE)

mat=matrix(0, 3, 4)
colnames(mat)=c("Prevalence", "Threshold", "Density", "IntensitySel")
rownames(mat)=c("Population", "MZ", "DZ")

if(length(args) >=3 ) {
  mat[1,1] = as.numeric(args[1])
  mat[2,1] = as.numeric(args[2])
  mat[3,1] = as.numeric(args[3])
  
  EffMZ = as.numeric(args[4])
  EffDZ = as.numeric(args[5])
} else {
  #using the example in Guo-Bo Chen et al HumMolGenet 2014, Table S11
  mat[1,1] = 0.0025
  mat[2,1] = 0.218
  mat[3,1] = 0.039
  
  EffMZ = 22
  EffDZ = 9
}

for(i in 1:nrow(mat)) {
  mat[i,2] = qnorm(1-mat[i,1]) #calculate threshold
  mat[i,3] = dnorm(mat[i,2]) #calculate density
  mat[i,4] = mat[i,3]/mat[i,1] #intensity of selection
}

#intraclass correlation using Reich's Method (Ann Hum Genet, 1972, 36:163-184)
tMZ=(mat[1,2] - mat[2,2] * sqrt( 1 - (mat[1,2]^2 - mat[2,2]^2)*(1 - mat[1,2]/mat[1,4]) ))/(mat[1,4] + mat[2,2]^2 * (mat[1,4] - mat[1,2]))
sdMZ=sqrt( (1-mat[2,1]) / (mat[1,4]^2 * mat[2,4]^2 * EffMZ ) )
tDZ=(mat[1,2] - mat[3,2] * sqrt( 1 - (mat[1,2]^2 - mat[3,2]^2)*(1 - mat[1,2]/mat[1,4]) ))/(mat[1,4] + mat[3,2]^2 * (mat[1,4] - mat[1,2]))
sdDZ=sqrt((1-mat[3,1])/(mat[1,4]^2 * mat[3,4]^2 * EffDZ) )

h2=2*(tMZ-tDZ)
sdh2=2*sqrt(sdMZ^2+sdDZ^2)
#intraclass correlation Falconer's (Ann Hum Genet, 1966, 29:51-76)
tMZF=(mat[1,2] - mat[2,2])/mat[1,4]
tDZF=(mat[1,2] - mat[3,2])/mat[1,4]
h2F=2*(tMZF - tDZF)

print(mat)
print(paste0("The estimated heritability is: ", h2, "(", sdh2, ") by Reich's method"))
print(paste0("The estimated heritability is: ", h2F, "(", sdh2, ") by Falconer's method"))
