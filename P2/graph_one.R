
graph_one = function(x)
{
MC=read.table("opt_mc_one.ts",header=TRUE,sep=",")
SA=read.table("opt_sa_one.ts",header=TRUE,sep=",")
TS=read.table("pred.ts",header=TRUE,sep=",")

P = round(TS,0)
Vm=P$x
mc = c(MC)
sa = c(SA)
if(x==1){
  p_mc =  (((Vm<1800)*0.05*Vm) + ((Vm>=1800)*0.06*Vm)) * round(mc$x,0)
  p_sa =  (((Vm<1800)*0.05*Vm) + ((Vm>=1800)*0.06*Vm)) * round(sa$x,0)
  cm = c(100,100,100,100,100,125,125)
  p_mc = (p_mc!=0) * (p_mc-cm)
  p_sa = (p_sa!=0) * (p_sa-cm)
}
if(x==2){  
  p_mc =  (((Vm<1800)*0.09*Vm) + ((Vm>=1800)*0.10*Vm)) * round(mc$x,0)
  p_sa =  (((Vm<1800)*0.09*Vm) + ((Vm>=1800)*0.10*Vm)) * round(sa$x,0)
  cm = c(150,150,150,150,150,175,175)
  p_mc = (p_mc!=0) *(p_mc-cm)
  p_sa = (p_sa!=0) *(p_sa-cm)  
}
if(x==3){  
  p_mc =  (((Vm<3600)*0.06*Vm) + ((Vm>=3600)*0.07*Vm)) * round(mc$x,0)
  p_sa =  (((Vm<3600)*0.06*Vm) + ((Vm>=3600)*0.07*Vm)) * round(sa$x,0)
  cm = c(350,350,350,350,350,375,375)
  p_mc = (p_mc!=0) * (p_mc-cm)
  p_sa = (p_sa!=0) * (p_sa-cm)
}  

  g_range <- range(-100,sum(p_sa)+100)
  i = 1
  while(i<=7)
  {
  if(i!=1){
  p_mc[i] = p_mc[i-1] + p_mc[i]
  p_sa[i] = p_sa[i-1] + p_sa[i]
  }
  i = i+1
  }

  plot(p_sa, type = "o", col="blue", ylim = g_range,axes=FALSE, ann=FALSE)
  lines(p_mc, type="o", pch=22, lty=2, col="red")
  axis(1, at=1:7, lab=c("Seg","Ter","Qua","Qui","Sex","Sab","Dom"))
  axis(2, las=1, at=50*0:g_range[2])
  box()
  legend(1, g_range[2], c("Simulated Annealing","Monte Carlo"), cex=0.8, 
   col=c("blue","red"), pch=21:22, lty=1:2);
 }