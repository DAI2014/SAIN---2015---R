library(forecast)
library(rminer)

cat("read time series:")
TS=read.table("hourly-unknown.ts",header=TRUE,sep=",")
TS=TS$ts # vector of numeric
mpause("show graph")
tsdisplay(TS)

L=length(TS)
H=12 # number of predictions

# time series monthly object:
TR=ts(TS,frequency=84,start=1,end=L-H)

# holt winters:
mpause("model holt winters:")
HW=HoltWinters(TR)
plot(HW)
mpause("show holt winters forecasts:")
F=forecast(HW,h=H)
print(F)
Pred=F$mean[1:H]
Target=TS[(L-H+1):L]
mgraph(Target,Pred,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","predictions")))
cat("MAE:",mmetric(Target,Pred,metric="MAE"),"\n")

# arima modeling:
mpause("model ARIMA:")
AR=auto.arima(TR)
mpause("show ARIMA forecasts:")
F1=forecast(AR,h=H)
print(F1)
Pred1=F1$mean[1:H]
mgraph(Target,Pred1,graph="REG",Grid=10,col=c("red","blue"),leg=list(pos="topleft",leg=c("target","predictions")))
cat("MAE:",mmetric(Target,Pred1,metric="MAE"),"\n")

# neural network modeling:
mpause("model NN (mlpe):")
d=CasesSeries(TS,c(12,84)) # data.frame from time series
LD=nrow(d)
dtr=1:(LD-H) # train indices
NN=fit(y~.,d[dtr,],model="mlpe")
# from 1 to H ahead forecasts:
F2=lforecast(NN,d,start=(LD-H+1),horizon=H)
print(F2)
Pred2=F2
mgraph(Target,Pred2,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","predictions")))
cat("MAE:",mmetric(Target,Pred2,metric="MAE"),"\n")

#bats
mpause("model BATS:")
x <- ts(TS, frequency=84)
fit <- tbats(x)
plot(forecast(fit,h=12))

#nnetar
mpause("model NNETar:")
fit <- nnetar(TS)
plot(forecast(fit,h=12))
