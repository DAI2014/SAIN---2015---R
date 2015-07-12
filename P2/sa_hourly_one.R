source("hill.R")


sa_hourly_vec = function(z,y)
{

TS=read.table("pred_h.ts",header=TRUE,sep=",")

eval=function(x) -profit(x)
Pm = round(TS,0)
Vm=Pm$x

profit=function(x) 
{  
  limit = y

  if(z==1){
  vendas = c(rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6),rep(0.06,6),rep(0.05,6))
  pm =  vendas * Vm * x
  cm = c(rep(20,60),rep(21,24))

  }
  if(z==2){
  vendas = c(rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6),rep(0.10,6),rep(0.09,6))
  pm =  vendas * Vm * x
  cm = c(rep(25,60),rep(26,24))
  }
  if(z==3){
  vendas = c(rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6),rep(0.07,6),rep(0.06,6))
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

# simulated annealing:
Runs=20


best= -Inf # - infinity

rchange2=function(par) # change for hclimbing
	{ hchange(par,lower=c(rep(0,84)),upper=c(rep(1,84)),rnorm,mean=0.0,sd=0.5,round=TRUE) }
	
for(i in 1:Runs)
{
	
 sa= optim(round(runif(1:84)),fn=eval,method="SANN",gr=rchange2,control=list(maxit=6000, temp=2000, trace=FALSE))
 L = profit(sa$par)
 #cat("execution:",i," solution:",sa$par," profit:",L,"\n")
 
 if(L>best) { BESTSA=sa; best=L; co = cost(z,BESTSA$par);}
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
cat("\n Simulated Annealing with",Runs,"runs \n")
cat(" Best Solution: \n",type,"\n","       10-------------------21 \n      S",round(BESTSA$par[1:12]),"\n      T",round(BESTSA$par[13:24]),"\n      Q",round(BESTSA$par[25:36]),"\n      Q",round(BESTSA$par[37:48]),"\n      S",round(BESTSA$par[49:60]),"\n      S",round(BESTSA$par[61:72]),"\n      D",round(BESTSA$par[73:84]),"\n      \n      Profit:",profit(BESTSA$par)," Cost:", co,"\n")


}