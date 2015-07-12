
graph_all = function()
{
MC=read.table("opt_mc.ts",header=TRUE,sep=",")
SA=read.table("opt_sa.ts",header=TRUE,sep=",")
TSm=read.table("male7.ts",header=TRUE,sep=",")
TSf=read.table("female7.ts",header=TRUE,sep=",")
TSa=read.table("all7.ts",header=TRUE,sep=",")

Pm = round(TSm,0)
Vm=Pm$x
Pf = round(TSf,0)
Vf=Pf$x
Pa = round(TSa,0)
Va=Pa$x

mc = c(MC)
sa = c(SA)

  pm_mc =  (((Vm<1800)*0.05*Vm) + ((Vm>=1800)*0.06*Vm)) * round(mc$x[1:7],0)
  pm_sa =  (((Vm<1800)*0.05*Vm) + ((Vm>=1800)*0.06*Vm)) * round(sa$x[1:7],0)
  cm = c(100,100,100,100,100,125,125)
  pm_mc = (pm_mc!=0) * (pm_mc-cm)
  pm_sa = (pm_sa!=0) * (pm_sa-cm)
  
  pf_mc =  (((Vf<1800)*0.09*Vf) + ((Vf>=1800)*0.10*Vf)) * round(mc$x[8:14],0)
  pf_sa =  (((Vf<1800)*0.09*Vf) + ((Vf>=1800)*0.10*Vf)) * round(sa$x[8:14],0)
  cf = c(150,150,150,150,150,175,175)
  pf_mc = (pf_mc!=0) *(pf_mc-cf)
  pf_sa = (pf_sa!=0) *(pf_sa-cf)  
  
  pa_mc =  (((Va<3600)*0.06*Va) + ((Va>=3600)*0.07*Va)) * round(mc$x[15:21],0)
  pa_sa =  (((Va<3600)*0.06*Va) + ((Va>=3600)*0.07*Va)) * round(sa$x[15:21],0)
  ca = c(350,350,350,350,350,375,375)
  pa_mc = (pa_mc!=0) * (pa_mc-ca)
  pa_sa = (pa_sa!=0) * (pa_sa-ca)
  
  p_mc = pm_mc + pf_mc + pa_mc
  p_sa = pm_sa + pf_sa + pa_sa
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