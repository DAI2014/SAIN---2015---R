# --------- Product Sales example ----------
source("hill.R")


mc_hourly_mat = function(y)
{
TSm=read.table("pred_h_m.ts",header=TRUE,sep=",")
TSf=read.table("pred_h_f.ts",header=TRUE,sep=",")
TSa=read.table("pred_h_a.ts",header=TRUE,sep=",")
Pm = round(TSm,0)
Vm=Pm$x
Pf = round(TSf,0)
Vf=Pf$x
Pa = round(TSa,0)
Va=Pa$x
entradas <- do.call(rbind,list(Vm,Vf,Va))
cm = c(rep(20,60),rep(21,24))
cf = c(rep(25,60),rep(26,24))
ca = c(rep(60,60),rep(61,24))
custos <- do.call(rbind,list(cm,cf,ca))
vem = c(rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6))
vef = c(rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6))
vea = c(rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6))
vendas <- do.call(rbind,list(vem,vef,vea))
eval=function(x) -profit(x)
profit=function(x) 
{ 
  limit = y
  exp <- do.call(rbind,list(x[1:84],x[85:168],x[169:252]))
  pm = exp * vendas * entradas
  a = ((pm!=0)*(pm-cm))
  profit = sum(a)
  cost_=sum(cm*x)
  if(cost_>limit){
  profit = -10000
  }
  return(profit)
}


cost=function(a)
{
cost=sum(custos*a)
return(cost)
}


# dimension
D=1


N=100 # number of searches numero de procuras que tenho que fazer
# monte carlo search with D=2 and x in [-10.4,10.4]
if(y<=2000)
{
lower=rep(rep(0,252),D) # lower bounds
upper=rep(rep(0.6,252),D) #  upper bounds
}
if(y>2000 & y<=3000)
{
lower=rep(rep(0,252),D) # lower bounds
upper=rep(rep(0.7,252),D) #  upper bounds
}
if(y>3000 & y<=4000)
{
lower=rep(rep(0,252),D) # lower bounds
upper=rep(rep(0.8,252),D) #  upper bounds
}
if(y>4000){
lower=rep(rep(0,252),D) # lower bounds
upper=rep(rep(1,252),D) #  upper bounds
}
MC=mcsearch(N=N,lower=lower,upper=upper,FUN=profit,type="max") #funcao mcsearch para minimizar
cat("\n Monte Carlo with ",N,"searches \n")


#cat(" Best Solution: \n","Male  ","\n","       10-------------------21 \n      S",round(MC$sol[1:12],0),"\n      T",round(MC$sol[13:24],0),"\n      Q",
#round(MC$sol[25:36],0),"\n      Q",round(MC$sol[37:48],0),"\n      S",round(MC$sol[49:60],0),"\n      S",round(MC$sol[61:72],0), "\n      D",
#round(MC$sol[73:84],0),"\n")
#cat(" Best Solution: \n","Female","\n","       10-------------------21 \n      S",round(MC$sol[85:96],0),"\n      T",round(MC$sol[97:108],0),"\n      Q",
#round(MC$sol[109:120],0),"\n      Q",round(MC$sol[121:132],0),"\n      S",round(MC$sol[133:144],0),"\n      S",round(MC$sol[145:156],0),
#"\n      D",round(MC$sol[157:168],0),"\n")
#cat(" Best Solution: \n","UniSex","\n","       10-------------------21 \n      S",round(MC$sol[169:180]),"\n      T",round(MC$sol[181:192]),"\n      Q",
#round(MC$sol[193:204],0),"\n      Q",round(MC$sol[205:216],0),"\n      S",round(MC$sol[217:228],0),"\n      S",round(MC$sol[229:240],0),
#"\n      D",round(MC$sol[241:252],0),"\n      \n      Profit:",MC$eval," Cost:", cost(round(MC$sol,0)),"\n")

cat(" Best Solution: \n","\n","                10-------------------21 \n Segunda  Male  ",round(MC$sol[1:12],0),"\n          Female",round(MC$sol[85:96],0),
"\n          UniSex",round(MC$sol[169:180]),"\n")
cat("\n           ----------------------------- \n")
cat(" Terca    Male  ",round(MC$sol[13:24],0),"\n          Female",round(MC$sol[97:108],0),"\n          UniSex",round(MC$sol[181:192]),"\n      ")
cat("\n           ----------------------------- \n")
cat(" Quarta   Male  ",round(MC$sol[25:36],0),"\n          Female",round(MC$sol[109:120],0),"\n          UniSex",round(MC$sol[193:204],0),"\n      ")
cat("\n           ----------------------------- \n")
cat(" Quinta   Male  ",round(MC$sol[37:48],0),"\n          Female",round(MC$sol[121:132],0),"\n          UniSex",round(MC$sol[205:216],0),"\n      ")
cat("\n           ----------------------------- \n")
cat(" Sexta    Male  ",round(MC$sol[49:60],0),"\n          Female",round(MC$sol[133:144],0),"\n          UniSex",round(MC$sol[217:228],0),"\n      ")
cat("\n           ----------------------------- \n")
cat(" Sabado   Male  ",round(MC$sol[61:72],0),"\n          Female",round(MC$sol[145:156],0),"\n          UniSex",round(MC$sol[229:240],0),"\n      ")
cat("\n           ----------------------------- \n")
cat(" Domingo  Male  ",round(MC$sol[73:84],0),"\n          Female",round(MC$sol[157:168],0),"\n          UniSex",round(MC$sol[241:252],0),"\n      ")
cat(" \n Profit:",MC$eval," Cost:", cost(round(MC$sol,0)),"\n")
}
