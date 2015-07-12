a = read.table("hourly-all.ts",header=TRUE,sep=",")
m = read.table("hourly-males.ts",header=TRUE,sep=",")
f = read.table("hourly-females.ts",header=TRUE,sep=",")
u = read.table("hourly-unknown.ts",header=TRUE,sep=",")

allts = a$ts 
malets = m$ts
femalets = f$ts
unknownts = u$ts

sum_all_10 = 0
sum_all_11 = 0
sum_all_12 = 0
sum_all_13 = 0
sum_all_14 = 0
sum_all_15 = 0
sum_all_16 = 0
sum_all_17 = 0
sum_all_18 = 0
sum_all_19 = 0
sum_all_20 = 0
sum_all_21 = 0

sum_m_10 = 0
sum_m_11 = 0
sum_m_12 = 0
sum_m_13 = 0
sum_m_14 = 0
sum_m_15 = 0
sum_m_16 = 0
sum_m_17 = 0
sum_m_18 = 0
sum_m_19 = 0
sum_m_20 = 0
sum_m_21 = 0

sum_f_10 = 0
sum_f_11 = 0
sum_f_12 = 0
sum_f_13 = 0
sum_f_14 = 0
sum_f_15 = 0
sum_f_16 = 0
sum_f_17 = 0
sum_f_18 = 0
sum_f_19 = 0
sum_f_20 = 0
sum_f_21 = 0

sum_u_10 = 0
sum_u_11 = 0
sum_u_12 = 0
sum_u_13 = 0
sum_u_14 = 0
sum_u_15 = 0
sum_u_16 = 0
sum_u_17 = 0
sum_u_18 = 0
sum_u_19 = 0
sum_u_20 = 0
sum_u_21 = 0

i=1
fim = length(allts)

while(i<fim){

#mudar isto
b<-substr(a$time[i],12,19)

if(b=='10:00:00'){
	sum_all_10 = sum_all_10 + allts[i]

}
if(b=='11:00:00'){
	sum_all_11 = sum_all_11 + allts[i]
}
if(b=='12:00:00'){
	sum_all_12 = sum_all_12 + allts[i]
}
if(b=='13:00:00'){
	sum_all_13 = sum_all_13 + allts[i]
}
if(b=='14:00:00'){
	sum_all_14 = sum_all_14 + allts[i]
}
if(b=='15:00:00'){
	sum_all_15 = sum_all_15 + allts[i]
}
if(b=='16:00:00'){
	sum_all_16 = sum_all_16 + allts[i]
}
if(b=='17:00:00'){
	sum_all_17 = sum_all_17 + allts[i]
}
if(b=='18:00:00'){
	sum_all_18 = sum_all_18 + allts[i]
}
if(b=='19:00:00'){
	sum_all_19 = sum_all_19 + allts[i]
}
if(b=='20:00:00'){
	sum_all_20 = sum_all_20 + allts[i]
}
if(b=='21:00:00'){
	sum_all_21 = sum_all_21 + allts[i]
}


i = i +1

}

# medias de entrada por hora
media_all_10 = sum_all_10 / 258
media_all_11 = sum_all_11 / 258
media_all_12 = sum_all_12 / 258
media_all_13 = sum_all_13 / 258
media_all_14 = sum_all_14 / 258
media_all_15 = sum_all_15 / 258
media_all_16 = sum_all_16 / 258
media_all_17 = sum_all_17 / 258
media_all_18 = sum_all_18 / 258
media_all_19 = sum_all_19 / 258
media_all_20 = sum_all_20 / 258
media_all_21 = sum_all_21 / 258

z=c(media_all_10,media_all_11,media_all_12,media_all_13,media_all_14,media_all_15,media_all_16,media_all_17,media_all_18,media_all_19,media_all_20,media_all_21)

barplot(z, main="Media de entradas por hora ", names.arg=c("10","11","12", "13", "14", "15", "16", "17", "18", "19", "20", "21"),ylim=c(0,600))