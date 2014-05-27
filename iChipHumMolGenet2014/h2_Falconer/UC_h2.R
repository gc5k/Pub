A=6
B=1

#Items in Table S11 (Prevalence, x, z, i)
pp=0.0025
#prevalence was assumed to be 0.0025 for UC; it was the prevalence that was used in Luke Jostin's Nature IBD Paper (Nat 2012, 491:119-24)
xp=abs(qnorm(0.0025))
#threshold given prevalence
zp=dnorm(xp)
#density of the standard normal given threshold
ip=zp/pp
#intensity of selection 

mat=matrix(0, (A+1)*(B+1), 7)
Cn=1
for(i in 0:A) {  
  for(j in 0:B) {
    MZ=(1+i)*2 + (8+(A-i))
    pMZ=MZ/(MZ+85)
    xMZ=abs(qnorm(pMZ))
    zMZ=dnorm(xMZ)
    iMZ=zMZ/pMZ
    
    DZ=(1+j)*2 + (4+(B-j))
    pDZ=DZ/(DZ+120)
    xDZ=abs(qnorm(pDZ))
    zDZ=dnorm(xDZ)
    iDZ=zDZ/pDZ
    
    tMZ=(xp-xMZ)/ip
    tDZ=(xp-xDZ)/ip
    h2=2*(tMZ-tDZ)
    mat[Cn, 1]=tMZ
    mat[Cn, 2]=tDZ
    if(h2>1)
    {
      h2=1
    }
    mat[Cn, 3]=h2
    mat[Cn, 4]=(1-pMZ)/(ip^2 * iMZ^2 * 15)
    mat[Cn, 5]=(1-pDZ)/(ip^2 * iDZ^2 * 6)
    
    mat[Cn, 6]=(tMZ/mat[Cn,4] + (2*tDZ)/(4*mat[Cn,5]))/(1/mat[Cn,4]+1/(4*mat[Cn,5]))
    mat[Cn, 7]=sqrt(1/(1/mat[Cn,4]+1/(4*mat[Cn,5])))
    print(paste(tMZ, tDZ, h2))
    Cn=Cn+1
  }
}
