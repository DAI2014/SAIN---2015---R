# if needed, install first this package:
library(lubridate)

# 2013: domingo 31 de Março e domingo 27 de Outubro

# read dataset into data.frame d:
d=read.table("pre-store.csv",sep=",",header=TRUE)

# convert $event_date field to date object (POSIXlt) 
T=as.POSIXlt(d$event_date,origin="1970-01-01",tz="GMT")

# auxiliary functions:

# return difference in units:
tdiff=function(time1,time2,units="days") as.numeric(difftime(time1,time2,units=units))

# return a hourly time series related with filtered t time object with events (from d)
# freq="hour" or "day"
# note: this is an improved version of seriesfromstore, puts 0 when no events appear at the period
seriesfromstore=function(t,freq="day",start=NULL,end=NULL,HINI=10,HEND=22,HADD=12)
{
 if(is.null(start)) start=t[1] # first period (hour or day)
 if(is.null(end)) end=t[length(t)] # last period (hour or day)

 if(freq=="hour") { UNITS="hours"; UNIT="hour"; FUNCTIME=hours} else { UNITS="days"; UNIT="day"; FUNCTIME=days}

 start=floor_date(start,unit=UNIT) # round date to period
 end=floor_date(end,unit=UNIT)     # round date to period

 if(freq=="day") DF=ceiling(tdiff(end,start,units=UNITS))+1 # number of elements in the series
 else { 
       DF=floor(tdiff(end,start,units="days"))*HADD-HADD
       if(HEND-hour(start)>=0) dini=HEND-hour(start) else dini=0
       if(hour(end) <= HEND) dend=hour(end)-HINI else dend=HADD
       DF=dini+DF+dend
      } 
 ts=rep(0,DF) # series to return
 dts=rep("",DF) # date series

 i=1 # time period of the series
 j=1 # cycle throught t
 itime=start
 L=length(t) # size of t
sum = 0
 # adapt this code!!!
 while(i<=DF)
 {
  #sum=0
  # advance j if t[j] < itime:
  while(j<L && tdiff(t[j],itime,units=UNITS)<0.0) j=j+1

  while(j<L && tdiff(t[j],itime,units=UNITS)<1.0) 
   { j=j+1;sum=sum+1; } # increment sum and j
   
   b<-substr(itime,12,19)
  
   if(b=="13:00:00" || b=="17:00:00" || b=="21:00:00"){
	if(b=="13:00:00"){
		dts[i]=gsub(b,"Manha",as.character(itime))
	}
	if(b=="17:00:00"){
		dts[i]=gsub(b,"Tarde",as.character(itime))
		}
	if(b=="21:00:00"){
		dts[i]=gsub(b,"Noite",as.character(itime))
		}
  ts[i]=sum # add element to series
  sum=0
  }

  
  i=i+1
  itime=itime+FUNCTIME(1) # next time period
  
  if(freq=="hour" && hour(itime)==HEND) itime=itime+FUNCTIME(HADD) # hour
 }
 # first day/hour or last day/hour may have incomplete number of records, delete them:
 RES=data.frame(time=dts[2:(DF-1)],ts=ts[2:(DF-1)] )
 return (RES)
}

# example of selection of males:
Imale=which(d$gender==1) 
Tmale=T[Imale]  # time filtered with only male events

# create time series from male entrances at each day:
dm=seriesfromstore(Tmale,freq="day",start=T[1],end=T[length(T)])

# days with missing values:
Imiss=which(dm$ts==0)
print(dm[Imiss,])

# create time series from male entrances at each hour:
hm=seriesfromstore(Tmale,freq="hour",start=T[1],end=T[length(T)])

# note: dm and hm are now data.frames with time and ts (date and observations)

# show visually the time series:
#library(forecast)
#tsdisplay(dm$ts)
#tsdisplay(hm$ts)

# save time series into file:
write.table(hm,"hourly-males-4.ts",row.names=FALSE,sep=",")
