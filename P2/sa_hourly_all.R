# --------- Product Sales example ----------
source("hill.R")


sa_hourly_mat = function(y)
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
  exp <- do.call(rbind,list(round(x[1:84],0),round(x[85:168],0),round(x[169:252],0)))
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



# simulated annealing:
Runs=20

best= -Inf # - infinity
#ml <- matrix(c(rep(0,7),rep(0,7),rep(0,7)),3)
#mu <- matrix(c(rep(1,7),rep(1,7),rep(1,7)),3)
#mt <-  matrix(c(round(runif(1:7)),round(runif(1:7)),round(runif(1:7))),3)
#rchange2=function(par) # change for hclimbing
	#{ hchange(par,lower=ml,upper=mu,rnorm,mean=0.5,sd=0.5,round=TRUE) }
	
	rchange2=function(par) # change for hclimbing
	{ hchange(par,lower=c(rep(0,252)),upper=c(rep(1,252)),rnorm,mean=0.0,sd=0.5,round=TRUE) }
	
for(i in 1:Runs)
{
	
 sa= optim(round(runif(1:252)),fn=eval,method="SANN",gr=rchange2,control=list(maxit=6000, temp=2000, trace=FALSE))
 L = profit(sa$par)
 #cat("execution:",i,"\n","solution: S T Q Q S S D","\n","Males:   ",sa$par[1:7],"\n","Females: ",sa$par[8:14],"\n","UniSex:  ",sa$par[15:21],"\n","profit:",L,"\n")
 
if(L>best) { BESTSA=sa; best=L; co = cost(BESTSA$par);}
}
cat("\n Simulated Annealing with",Runs,"runs \n")

#cat(" Best Solution: \n","Male  ","\n","       10-------------------21 \n      S",round(BESTSA$par[1:12]),"\n      T",round(BESTSA$par[13:24]),"\n      Q",
#round(BESTSA$par[25:36]),"\n      Q",round(BESTSA$par[37:48]),"\n      S",round(BESTSA$par[49:60]),"\n      S",round(BESTSA$par[61:72]), "\n      D",
#round(BESTSA$par[73:84]),"\n")
#cat(" Best Solution: \n","Female","\n","       10-------------------21 \n      S",round(BESTSA$par[85:96]),"\n      T",round(BESTSA$par[97:108]),"\n      Q",
#round(BESTSA$par[109:120]),"\n      Q",round(BESTSA$par[121:132]),"\n      S",round(BESTSA$par[133:144]),"\n      S",round(BESTSA$par[145:156]),
#"\n      D",round(BESTSA$par[157:168]),"\n")
#cat(" Best Solution: \n","UniSex","\n","       10-------------------21 \n      S",round(BESTSA$par[169:180]),"\n      T",round(BESTSA$par[181:192]),"\n      Q",
#round(BESTSA$par[193:204]),"\n      Q",round(BESTSA$par[205:216]),"\n      S",round(BESTSA$par[217:228]),"\n      S",round(BESTSA$par[229:240]),
#"\n      D",round(BESTSA$par[241:252]),"\n      \n      Profit:",profit(BESTSA$par)," Cost:", co,"\n")

cat(" Best Solution: \n","\n","                10-------------------21 \n Segunda  Male  ",round(BESTSA$par[1:12],0),"\n          Female",round(BESTSA$par[85:96],0),
"\n          UniSex",round(BESTSA$par[169:180]),"\n")
cat("\n           ----------------------------- \n")
cat(" Terca    Male  ",round(BESTSA$par[13:24],0),"\n          Female",round(BESTSA$par[97:108],0),"\n          UniSex",round(BESTSA$par[181:192]),"\n      ")
cat("\n           ----------------------------- \n")
cat(" Quarta   Male  ",round(BESTSA$par[25:36],0),"\n          Female",round(BESTSA$par[109:120],0),"\n          UniSex",round(BESTSA$par[193:204],0),"\n      ")
cat("\n           ----------------------------- \n")
cat(" Quinta   Male  ",round(BESTSA$par[37:48],0),"\n          Female",round(BESTSA$par[121:132],0),"\n          UniSex",round(BESTSA$par[205:216],0),"\n      ")
cat("\n           ----------------------------- \n")
cat(" Sexta    Male  ",round(BESTSA$par[49:60],0),"\n          Female",round(BESTSA$par[133:144],0),"\n          UniSex",round(BESTSA$par[217:228],0),"\n      ")
cat("\n           ----------------------------- \n")
cat(" Sabado   Male  ",round(BESTSA$par[61:72],0),"\n          Female",round(BESTSA$par[145:156],0),"\n          UniSex",round(BESTSA$par[229:240],0),"\n      ")
cat("\n           ----------------------------- \n")
cat(" Domingo  Male  ",round(BESTSA$par[73:84],0),"\n          Female",round(BESTSA$par[157:168],0),"\n          UniSex",round(BESTSA$par[241:252],0),"\n      ")
cat(" \n Profit:",profit(BESTSA$par)," Cost:", co,"\n")
}
