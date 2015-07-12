# --------- Product Sales example ----------
source("hill.R")

sa_daily_vec = function(z,y,w)
{
print(w)
TS=read.table("pred.ts",header=TRUE,sep=",")

eval=function(x) -profit(x)
Pm = round(TS,0)
Vm=Pm$x

profit=function(x) 
{
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



# simulated annealing:
Runs=20


round(runif(1:7))
best= -Inf # - infinity

rchange2=function(par) # change for hclimbing
	{ hchange(par,lower=c(0,0,0,0,0,0,0),upper=c(1,1,1,1,1,1,1),rnorm,mean=0.5,sd=0.5,round=TRUE) }
	
for(i in 1:Runs)
{
	
 sa= optim(round(runif(1:7)),fn=eval,method="SANN",gr=rchange2,control=list(maxit=6000, temp=2000, trace=FALSE))
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
cat(" Best Solution: \n        S T Q Q S S D \n",type,round(BESTSA$par),"\n profit:",profit(BESTSA$par),"cost:", co,"\n")
if(w!=3){
if(z==1){
  pm =  (((Vm<1800)*0.05*Vm) + ((Vm>=1800)*0.06*Vm)) * round(BESTSA$par,0)
  cm = c(100,100,100,100,100,125,125)
  }
  if(z==2){
  pm =  (((Vm<1800)*0.09*Vm) + ((Vm>=1800)*0.10*Vm)) * round(BESTSA$par,0)
  cm = c(150,150,150,150,150,175,175)
  }
  if(z==3){
  pm =  (((Vm<3600)*0.06*Vm) + ((Vm>=3600)*0.07*Vm)) * round(BESTSA$par,0)
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
  
  
write.table(round(BESTSA$par,0),"opt_sa_one.ts",row.names=FALSE,sep=",")

}
}

