# --------- Product Sales example ----------
TSm=read.table("male7.ts",header=TRUE,sep=",")
TSf=read.table("female7.ts",header=TRUE,sep=",")
TSa=read.table("all7.ts",header=TRUE,sep=",")
source("hill.R")

mc_daily_mat = function(w,x,z)
{
eval=function(x) -profit(x)
Pm = round(TSm,0)
Vm=Pm$x
Pf = round(TSf,0)
Vf=Pf$x
Pa = round(TSa,0)
Va=Pa$x
mat <- do.call(rbind,list(Vm,Vf,Va))
cm = c(100,100,100,100,100,125,125)
cf = c(150,150,150,150,150,175,175)
ca = c(350,350,350,350,350,375,375)
custos <- do.call(rbind,list(cm,cf,ca))
profit=function(x) 
{ 
  limit = z
  x = round(x,0)
  exp <- do.call(rbind,list(x[1:7],x[8:14],x[15:21]))
  mat[1,] = (((mat[1,]<1800)*0.05*mat[1,]) + ((mat[1,]>=1800)*0.06*mat[1,])) * x[1:7]
  mat[2,] = (((mat[2,]<1800)*0.09*mat[2,]) + ((mat[2,]>=1800)*0.10*mat[2,])) * x[8:14]
  mat[3,] = (((mat[3,]<3600)*0.06*mat[3,]) + ((mat[3,]>=3600)*0.07*mat[3,])) * x[15:21]
  mat[1,] = ((mat[1,]!=0)*(mat[1,]-custos[1,]))
  mat[2,] = ((mat[2,]!=0)*(mat[2,]-custos[2,]))
  mat[3,] = ((mat[3,]!=0)*(mat[3,]-custos[3,]))
  profit = sum(mat)
  co = sum(custos*exp)
  if(co>limit){
  profit = -10000
  }
  return(profit)
}

cost=function(a)
{
exp2 <- do.call(rbind,list(a[1:7],a[8:14],a[15:21]))
cost=sum(custos*exp2)
return(cost)
}


# dimension
D=1


N=100 # number of searches numero de procuras que tenho que fazer
# monte carlo search with D=2 and x in [-10.4,10.4]
lower=rep(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),D) # lower bounds
upper=rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),D) #  upper bounds
MC=mcsearch(N=N,lower=round(lower,0),upper=round(upper,0),FUN=profit,type="max") #funcao mcsearch para minimizar
cat("\n Monte Carlo with ",N,"searches \n")
cat(" Best Solution: ","\n","solution: S T Q Q S S D","\n","Males:   ",round(MC$sol[1:7],0),"\n","Females: ",round(MC$sol[8:14],0),
"\n","UniSex:  ",round(MC$sol[15:21],0),"\n","profit:",MC$eval,"\n","Cost:",cost(round(MC$sol,0)),"\n")
if(w!=3){
  pm =  (((Vm<1800)*0.05*Vm) + ((Vm>=1800)*0.06*Vm)) * round(MC$sol[1:7],0)
  cm = c(100,100,100,100,100,125,125)
  pm = (pm!=0) * (pm-cm)
  
  pf =  (((Vf<1800)*0.09*Vf) + ((Vf>=1800)*0.10*Vf)) * round(MC$sol[8:14],0)
  cf = c(150,150,150,150,150,175,175)
  pf = (pf!=0) *(pf-cf)	
  
  pa =  (((Va<3600)*0.06*Va) + ((Va>=3600)*0.07*Va)) * round(MC$sol[15:21],0)
  ca = c(350,350,350,350,350,375,375)
  pa = (pa!=0) * (pa-ca)
  
  p = pm + pf + pa
  g_range <- range(-100,MC$eval+100)
  i = 1
  while(i<=7)
  {
  if(i!=1){
  pm[i] = pm[i-1] + pm[i]
  pf[i] = pf[i-1] + pf[i]
  pa[i] = pa[i-1] + pa[i]
  p[i] = p[i-1] + p[i]
  }
  i = i+1
  }
  plot(pm, type = "o", col="blue", ylim = g_range,axes=FALSE, ann=FALSE)
  lines(pf, type="o", pch=22, lty=2, col="red")
  lines(pa, type="o", pch=22, lty=2, col="green")
  lines(p, type="o", pch=22, lty=2, col="black")
  axis(1, at=1:7, lab=c("Seg","Ter","Qua","Qui","Sex","Sab","Dom"))
  axis(2, las=1, at=50*0:g_range[2])
  box()
  legend(1, g_range[2], c("Male","Female","UniSex","All"), cex=0.8, 
   col=c("blue","red","green","black"), pch=21:22, lty=1:2);
   }
   if(w==3){
   write.table(round(MC$sol,0),"opt_mc.ts",row.names=FALSE,sep=",")
  }
}
