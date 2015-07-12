# --------- Product Sales example ----------
source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here

mc_daily_vec = function(z,y,w)
{

TS=read.table("pred.ts",header=TRUE,sep=",")

eval=function(x) -profit(x)
Pm = round(TS,0)
Vm=Pm$x

profit=function(x) 
{

  x = round(x,0)
  limit = y 
  if(z==1){
  pm =  (((Vm<1800)*0.05*Vm) + ((Vm>=1800)*0.06*Vm)) * x
  cm = c(100,100,100,100,100,125,125)
  }
  if(z==2){
  pm =  (((Vm<1800)*0.09*Vm) + ((Vm>=1800)*0.10*Vm)) * x
  cm = c(150,150,150,150,150,175,175)
  }
  if(z==3){
  pm =  (((Vm<3600)*0.06*Vm) + ((Vm>=3600)*0.07*Vm)) * x
  cm = c(350,350,350,350,350,375,375)
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
cm = c(100,100,100,100,100,125,125)
}
if(a==2){
cm = c(150,150,150,150,150,175,175)
}
if(a==3){
cm = c(350,350,350,350,350,375,375)
}
cost=sum(b*cm)
return(cost)
}

if(z==1)
{
type="Male  "
}
if(z==2){
type = "Female"
}

if(z==3){
type="UniSex"
}

# dimension
D=1


N=100 # number of searches numero de procuras que tenho que fazer
# monte carlo search with D=2 and x in [-10.4,10.4]
lower=rep(c(0,0,0,0,0,0,0),D) # lower bounds
upper=rep(c(1,1,1,1,1,1,1),D) #  upper bounds
MC=mcsearch(N=N,lower=round(lower,0),upper=round(upper,0),FUN=profit,type="max") #funcao mcsearch para minimizar
cat("\n Monte Carlo with ",N,"searches \n")
cat(" Best solution: \n        S T Q Q S S D \n",type,round(MC$sol,0),"\n Profit:",MC$eval,"Cost:", cost(z,round(MC$sol,0)),"\n")


if(w!=3){
if(z==1){
  pm =  (((Vm<1800)*0.05*Vm) + ((Vm>=1800)*0.06*Vm)) * round(MC$sol,0)
  cm = c(100,100,100,100,100,125,125)
  }
  if(z==2){
  pm =  (((Vm<1800)*0.09*Vm) + ((Vm>=1800)*0.10*Vm)) * round(MC$sol,0)
  cm = c(150,150,150,150,150,175,175)
  }
  if(z==3){
  pm =  (((Vm<3600)*0.06*Vm) + ((Vm>=3600)*0.07*Vm)) * round(MC$sol,0)
  cm = c(350,350,350,350,350,375,375)
  } 
  a = ((pm!=0)*(pm-cm))
  g_range <- range(-10,sum(a))
  i = 1
  while(i<=7)
  {
  if(i!=1){
  a[i] = a[i-1] + a[i]
  }
  i = i+1
  }
  plot(a, type = "o", col="blue", ylim = g_range,axes=FALSE, ann=FALSE)
  axis(1, at=1:7, lab=c("Seg","Ter","Qua","Qui","Sex","Sab","Dom"))
  axis(2, las=1, at=10*0:g_range[2])
  box()
}
if(w==3){
write.table(round(MC$sol,0),"opt_mc_one.ts",row.names=FALSE,sep=",")
}
}
