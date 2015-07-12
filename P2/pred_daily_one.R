library(forecast)
library(rminer)

pred_daily_vec = function(x,y)
{
if(y==1){
TS=read.table("daily-males.ts",header=TRUE,sep=",")
TS=TS$ts # vector of numeric
}
if(y==2){
TS=read.table("daily-females.ts",header=TRUE,sep=",")
TS=TS$ts # vector of numeric
}
if(y==3){
TS=read.table("daily-all.ts",header=TRUE,sep=",")
TS=TS$ts # vector of numeric
}

L=length(TS)-x

H=7 # number of predictions

# time series monthly object:
TR=ts(TS,frequency=7,start=1,end=L-H)


Target=TS[(L-H+1):L]


d=CasesSeries(TS,c(7,14,21)) # data.frame from time series
LD=nrow(d)
dtr=1:(LD-H) # train indices
NN=fit(y~.,d[dtr,],model="mlpe")
# from 1 to H ahead forecasts:
F2=lforecast(NN,d,start=(LD-H+1),horizon=H)
Pred2=F2
mgraph(Target,Pred2,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","predictions")))
P = round(Pred2,0)
write.table(P,"pred.ts",row.names=FALSE,sep=",")
}


