library(forecast)
library(rminer)
TSM=read.table("daily-males.ts",header=TRUE,sep=",")
TSM=TSM$ts # vector of numeric
TSF=read.table("daily-females.ts",header=TRUE,sep=",")
TSF=TSF$ts # vector of numeric
TSA=read.table("daily-all.ts",header=TRUE,sep=",")
TSA=TSA$ts # vector of numeric
pred_daily_mat = function(x)
{

L=length(TSM)-x
H=7 # number of predictions

# time series monthly object:
TR=ts(TSM,frequency=7,start=1,end=L-H)


Target=TSM[(L-H+1):L]


d=CasesSeries(TSM,c(7)) # data.frame from time series
LD=nrow(d)
dtr=1:(LD-H) # train indices
NN=fit(y~.,d[dtr,],model="mlpe")
# from 1 to H ahead forecasts:
F2=lforecast(NN,d,start=(LD-H+1),horizon=H)
Pred2=F2
#mgraph(Target,Pred2,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","predictions")))
P = round(Pred2,0)
write.table(P,"pred_m.ts",row.names=FALSE,sep=",")

TR=ts(TSF,frequency=7,start=1,end=L-H)


Target=TSF[(L-H+1):L]


d=CasesSeries(TSF,c(7)) # data.frame from time series
LD=nrow(d)
dtr=1:(LD-H) # train indices
NN=fit(y~.,d[dtr,],model="mlpe")
# from 1 to H ahead forecasts:
F2=lforecast(NN,d,start=(LD-H+1),horizon=H)
Pred2=F2
#mgraph(Target,Pred2,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","predictions")))
P = round(Pred2,0)
write.table(P,"pred_f.ts",row.names=FALSE,sep=",")

TR=ts(TSA,frequency=7,start=1,end=L-H)


Target=TSA[(L-H+1):L]


d=CasesSeries(TSA,c(7)) # data.frame from time series
LD=nrow(d)
dtr=1:(LD-H) # train indices
NN=fit(y~.,d[dtr,],model="mlpe")
# from 1 to H ahead forecasts:
F2=lforecast(NN,d,start=(LD-H+1),horizon=H)
Pred2=F2
#mgraph(Target,Pred2,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","predictions")))
P = round(Pred2,0)
write.table(P,"pred_a.ts",row.names=FALSE,sep=",")
}


