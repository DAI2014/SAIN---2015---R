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

sum_all = 0
sum_male = 0
sum_female = 0
sum_unknown = 0
i=1
fim = length(allts)

while(i<fim){

sum_all = sum_all + allts[i]
sum_male = sum_male + malets[i]
sum_female = sum_female + femalets[i]
sum_unknown = sum_unknown + unknownts[i]
i = i +1

}

# medias de entrada por dia
media_all = sum_all / fim
media_male = sum_male / fim
media_female = sum_female / fim
media_unknown = sum_unknown / fim

# percentagem de entradas baseadas em sexo

p_male = sum_male / sum_all
p_female = sum_female / sum_all
p_unknown = sum_unknown / sum_all

mpause("Entradas medias por dia:")

a = c(media_all,media_male,media_female,media_unknown)
barplot(a, main="Media de entradas diarias", names.arg=c("All", "Males", "Females", "Unknown"), cex.names=0.8, ylim=c(0,4000))
text(4.3,300, round(media_unknown, digits = 0))
text(3.1,1800, round(media_female, digits=0))
text(1.9,1900, round(media_male, digits=0))
text(0.7,3800, round(media_all,digits=0))

mpause("Percentagem por dia:")

b = c(p_male,p_female,p_unknown)
barplot(b, main="Percentagem de entradas diarias ", names.arg=c("Males", "Females", "Unknown"), cex.names=0.8, ylim=c(0,0.6))
text(3.1,0.07, paste(round(p_unknown, digits=2)*100,"%"))
text(1.9,0.48, paste(round(p_female,digits=2)*100,"%"))
text(0.7,0.5, paste(round(p_male,digits=2)*100,"%"))