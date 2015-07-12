source("hill.R")


mc_hourly_vec = function(z,y)
{

TS=read.table("pred_h.ts",header=TRUE,sep=",")
eval=function(x) -profit(x)
Pm = round(TS,0)
Vm=Pm$x

profit=function(x) 
{  
  limit = y
  if(z==1){
  vendas = c(rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6),
  rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6))
  pm =  vendas * Vm * x
  cm = c(rep(20,60),rep(21,24))
  }
  if(z==2){
  vendas = c(rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6),
  rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6))
  pm =  vendas * Vm * x
  cm = c(rep(25,60),rep(26,24))
  }
  if(z==3){
  vendas = c(rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6),
  rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6))
  pm =  vendas * Vm * x
  cm = c(rep(60,60),rep(61,24))
  }
  a = ((pm!=0)*(pm-cm))
  profit = sum(a)
  cost_=sum(cm*x)
  if(cost_>limit){
  profit=-10000
  }
  return(profit)
}

cost=function(a,b)
{
if(a==1){
  cm = c(rep(20,60),rep(21,24))
}
if(a==2){
  cm = c(rep(25,60),rep(26,24))
}
if(a==3){
  cm = c(rep(60,60),rep(61,24))
}
cost=sum(b*cm)
return(cost)
}

# dimension
D=1


N=100 # number of searches numero de procuras que tenho que fazer
# monte carlo search with D=2 and x in [-10.4,10.4]
lower=rep(rep(0,84),D) # lower bounds
upper=rep(rep(1,84),D) #  upper bounds
MC=mcsearch(N=N,lower=round(lower,0),upper=round(upper,0),FUN=profit,type="max") #funcao mcsearch para minimizar
cat("\n Monte Carlo with ",N,"searches \n")
if(z==1)
{
type="Male  "
}
if(z==2)
{
type = "Female"
}
if(z==3){
type="UniSex"
}
cat(" Best Solution: \n",type,"\n","       10-------------------21 \n      S",round(MC$sol[1:12],0),"\n      T",round(MC$sol[13:24],0),
"\n      Q",round(MC$sol[25:36],0),"\n      Q",round(MC$sol[37:48],0),"\n      S",round(MC$sol[49:60],0),"\n      S",
round(MC$sol[61:72],0),"\n      D",round(MC$sol[73:84],0),"\n      \n      Profit:",MC$eval," Cost:", cost(z,round(MC$sol,0)),"\n")


}