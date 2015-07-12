library(forecast)
library(rminer)

TSM=read.table("hourly-males.ts",header=TRUE,sep=",")
TSM=TSM$ts # vector of numeric
TSF=read.table("hourly-females.ts",header=TRUE,sep=",")
TSF=TSF$ts # vector of numeric
TSA=read.table("hourly-all.ts",header=TRUE,sep=",")
TSA=TSA$ts # vector of numeric


#recuar 0,84,168
pred_hourly_vec = function(x,y)
{
if(y==1){
TS=read.table("hourly-males.ts",header=TRUE,sep=",")
TS=TS$ts # vector of numeric
}
if(y==2){
TS=read.table("hourly-females.ts",header=TRUE,sep=",")
TS=TS$ts # vector of numeric
}
if(y==3){
TS=read.table("hourly-all.ts",header=TRUE,sep=",")
TS=TS$ts # vector of numeric
}




L=length(TS) - x
H=84 # number of predictions

# time series monthly object:
TR=ts(TS,frequency=84,start=1,end=L-H)


Target=TSM[(L-H+1):L]


d=CasesSeries(TS,c(12,84)) # data.frame from time series
LD=nrow(d)
dtr=1:(LD-H) # train indices
NN=fit(y~.,d[dtr,],model="mlpe")
# from 1 to H ahead forecasts:
F2=lforecast(NN,d,start=(LD-H+1),horizon=H)
#print(F2)
Pred2=F2
#mgraph(Target,Pred2,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","predictions")))
#cat("MAE:",mmetric(Target,Pred2,metric="MAE"),"\n")
P = round(Pred2,0)
write.table(P,"pred_h.ts",row.names=FALSE,sep=",")

}
