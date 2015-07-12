library(forecast)
library(rminer)

a = read.table("daily-all.ts",header=TRUE,sep=",")
m = read.table("daily-males.ts",header=TRUE,sep=",")
f = read.table("daily-females.ts",header=TRUE,sep=",")
u = read.table("daily-unknown.ts",header=TRUE,sep=",")

allts = a$ts 
malets = m$ts
femalets = f$ts
unknownts = u$ts

sum_all_2 = 0
sum_all_3 = 0
sum_all_4 = 0
sum_all_5 = 0
sum_all_6 = 0
sum_all_7 = 0
sum_all_8 = 0
i=1
fim = length(allts)
j=0
while(i<fim){
j=j+1
if(j==1){
sum_all_2 = sum_all_2 + allts[i]
}
if(j==2){
sum_all_3 = sum_all_3 + allts[i]
}
if(j==3){
sum_all_4 = sum_all_4 + allts[i]
}
if(j==4){
sum_all_5 = sum_all_5 + allts[i]
}
if(j==5){
sum_all_6 = sum_all_6 + allts[i]
}
if(j==6){
sum_all_7 = sum_all_7 + allts[i]
}
if(j==7){
sum_all_8 = sum_all_8 + allts[i]
j=0
}
i = i +1
}

# medias de entrada por dia
media_all_2 = sum_all_2 / 37
media_all_3 = sum_all_3 / 37
media_all_4 = sum_all_4 / 37
media_all_5 = sum_all_5 / 37
media_all_6 = sum_all_6 / 37
media_all_7 = sum_all_7 / 37
media_all_8 = sum_all_8 / 37
print(media_all_2)
print(sum_all_2)

d=c(media_all_2,media_all_3,media_all_4,media_all_5,media_all_6,media_all_7,media_all_8)

#media de entradas por dia da semana

barplot(d, main="Media de entradas por dia da Semana", names.arg=c("Segunda", "Terca", "Quarta","Quinta","Sexta","Sabado","Domingo"), cex.names=0.8, ylim=c(0,8000))