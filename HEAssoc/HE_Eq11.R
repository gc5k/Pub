#Equation 11
##############parameters######################
rho=0.75 #rho
h2=0.5 #heritability
L=100 #number of markers, which are actually QTLs.
f=0.5 #frequency
##############################################
d=0
if(rho==0)
{
  d=10
} else
{
  d=-log(rho)/2
}
e=0.5/L
interval=seq(e, 1-e, length.out=L)
beta=qnorm(interval)
Vg=0
Vp=0
V1=0
V2=0
###########additive variance components
Vg_d=0
Vg_od=0
for(i in 1:L)
{
  for(j in 1:L)
  {
    r_ij=exp(-2*abs(i-j)*d)
    Vg = Vg+2*sqrt(f*(1-f)*f*(1-f))*r_ij*beta[i]*beta[j]
    if(i == j)
    {
      Vg_d =Vg_d + 2*sqrt(f*(1-f)*f*(1-f))*r_ij*beta[i]*beta[j]
    } else {
      Vg_od = Vg_od + 2*sqrt(f*(1-f)*f*(1-f))*r_ij*beta[i]*beta[j]
    }
  }
}
Vp=Vg/h2
##########HE estimate
b_num=0
b_den=0
for(i in 1:L)
{
  for(l1 in 1:L)
  {
    for(l2 in 1:L)
    {
      r1=exp(-2*abs(l1-i)*d)
      r2=exp(-2*abs(l2-i)*d)
      b_num = b_num + 2 * sqrt(f*(1-f)*f*(1-f)) * r1 * r2 * beta[l1] * beta[l2]
    }
  }
}
b_num = -2 * b_num / L
M=matrix(0, L, L)
for(l1 in 1:L)
{
  for(l2 in 1:L)
  {
    M[l1, l2] = exp(-2*abs(l1-l2)*d) ^2
  }
}
b_den = mean(M)
HE_b=b_num / b_den
HE_h2 = -1 * b_num / b_den / 2
out=paste0("True h2=", h2, ", rho=", rho, ", HE b1=", HE_b, ", Estimated HE heritability=", HE_h2/Vp)
print(paste("Vg = Vg_d (within-locus) + Vg_od (between locus): ", Vg , "=", Vg_d, "+" , Vg_od))
print(paste("Vp=Vg/h2 :", Vp, "=", Vg, "/", h2))
print(paste("b(HE)=", HE_b))
print(paste("h2_hat=-0.5*b/Vp: ", HE_h2/Vp, "=-0.5*", HE_b, "/", Vp))
