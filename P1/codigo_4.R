a = read.table("hourly-all-4.ts",header=TRUE,sep=",")
m = read.table("hourly-male-4.ts",header=TRUE,sep=",")
f = read.table("hourly-female-4.ts",header=TRUE,sep=",")
u = read.table("hourly-unknown-4.ts",header=TRUE,sep=",")

allts = a$ts 
malets = m$ts
femalets = f$ts
unknownts = u$ts

sum_all_manha = 0
sum_all_tarde = 0
sum_all_noite = 0
sum_male_manha = 0
sum_male_tarde = 0
sum_male_noite = 0
sum_female_manha = 0
sum_female_tarde = 0
sum_female_noite = 0
sum_unknown_manha = 0
sum_unknown_tarde = 0
sum_unknown_noite = 0
i=1
fim = length(allts)

while(i<fim){

b<-substr(a$time[i],12,17)

if(b=='Manha'){
	sum_all_manha = sum_all_manha + allts[i]
	sum_male_manha = sum_male_manha + malets[i]
	sum_female_manha = sum_female_manha + femalets[i]
	sum_unknown_manha = sum_unknown_manha + unknownts[i]
}

if(b=='Tarde'){
	sum_all_tarde = sum_all_tarde + allts[i]
	sum_male_tarde = sum_male_tarde + malets[i]
	sum_female_tarde = sum_female_tarde + femalets[i]
	sum_unknown_tarde = sum_unknown_tarde + unknownts[i]
}

if(b=='Noite'){
	sum_all_noite = sum_all_noite + allts[i]
	sum_male_noite = sum_male_noite + malets[i]
	sum_female_noite = sum_female_noite + femalets[i]
	sum_unknown_noite = sum_unknown_noite + unknownts[i]
}

i = i +1

}

# medias de entrada por periodo
media_all_manha = sum_all_manha / (fim/3)
media_all_tarde = sum_all_tarde / (fim/3)
media_all_noite = sum_all_noite / (fim/3)
media_male_manha = sum_male_manha / (fim/3)
media_male_tarde = sum_male_tarde / (fim/3)
media_male_noite = sum_male_noite / (fim/3)
media_female_manha = sum_female_manha / (fim/3)
media_female_tarde = sum_female_tarde / (fim/3)
media_female_noite = sum_female_noite / (fim/3)
media_unknown_manha = sum_unknown_manha / (fim/3)
media_unknown_tarde = sum_unknown_tarde / (fim/3)
media_unknown_noite = sum_unknown_noite / (fim/3)

# percentagem de entradas baseadas no sexo

p_male_manha = sum_male_manha / sum_all_manha
p_male_tarde = sum_male_tarde / sum_all_tarde
p_male_noite = sum_male_noite / sum_all_noite
p_female_manha = sum_female_manha / sum_all_manha
p_female_tarde = sum_female_tarde / sum_all_tarde
p_female_noite = sum_female_noite / sum_all_noite
p_unknown_manha = sum_unknown_manha / sum_all_manha
p_unknown_tarde = sum_unknown_tarde / sum_all_tarde
p_unknown_noite = sum_unknown_noite / sum_all_noite


print(media_all_manha)
print(media_all_tarde)
print(media_all_noite)
print(media_male_manha)
print(media_male_tarde)
print(media_male_noite)
print(media_female_manha)
print(media_female_tarde)
print(media_female_noite)
print(media_unknown_manha)
print(media_unknown_tarde)
print(media_unknown_noite)
print(p_male_manha)
print(p_male_tarde)
print(p_male_noite)
print(p_female_manha)
print(p_female_tarde)
print(p_female_noite)
print(p_unknown_manha)
print(p_unknown_tarde)
print(p_unknown_noite)

a=c(media_all_manha,media_all_tarde,media_all_noite)
b=c(media_male_manha,media_male_tarde,media_male_noite)
c=c(media_female_manha,media_female_tarde,media_female_noite)
d=c(media_unknown_manha,media_unknown_tarde,media_unknown_noite)

e=c(p_male_manha,p_male_tarde,p_male_noite)
f=c(p_female_manha,p_female_tarde,p_female_noite)
g=c(p_unknown_manha,p_unknown_tarde,p_unknown_noite)

z=c(a,b,c,d)
x=c(b,c,d)
y=c(e,f,g)


mpause("Entradas medias por periodo - All:")

barplot(a, main="Media de entradas por periodo horario - All", names.arg=c("Manha", "Tarde", "Noite"), cex.names=0.8, ylim=c(0,2000))
text(3.1,1100, round(media_all_manha, digits=0))
text(1.9,1700, round(media_all_tarde, digits=0))
text(0.7,1200, round(media_all_noite,digits=0))

mpause("Entradas medias por periodo - Male:")

barplot(b, main="Media de entradas por periodo horario - Male", names.arg=c("Manha", "Tarde", "Noite"), cex.names=0.8, ylim=c(0,2000))
text(3.1,600, round(media_male_manha, digits=0))
text(1.9,800, round(media_male_tarde, digits=0))
text(0.7,600, round(media_male_noite,digits=0))

mpause("Entradas medias por periodo - Female:")

barplot(c, main="Media de entradas por periodo horario - All", names.arg=c("Manha", "Tarde", "Noite"), cex.names=0.8, ylim=c(0,2000))
text(3.1,550, round(media_female_manha, digits=0))
text(1.9,850, round(media_female_tarde, digits=0))
text(0.7,550, round(media_female_noite,digits=0))

mpause("Entradas medias por periodo - Unknown:")

barplot(d, main="Media de entradas por periodo horario - Unknown", names.arg=c("Manha", "Tarde", "Noite"), cex.names=0.8, ylim=c(0,2000))
text(3.1,100, round(media_unknown_manha, digits=0))
text(1.9,150, round(media_unknown_tarde, digits=0))
text(0.7,100, round(media_unknown_noite,digits=0))

x = matrix(x, ncol = 3, byrow = T)
colnames(x) = c("Manha","Tarde","Noite")
rownames(x) = c("Male","Female","Unknown")
barplot(x, beside=TRUE)
par(mar = c(5.1, 4.1, 4.1, 7.1), xpd = TRUE)
       barplot(x, col = heat.colors(length(rownames(x))), width = 2, beside = TRUE,ylim=c(0,1000))
       legend("topright", inset = c(-0.25, 0), fill = heat.colors(length(rownames(x))), 
       legend = rownames(x))

y=matrix(y, ncol = 3, byrow = T)  
colnames(y) = c("Manha","Tarde","Noite")
rownames(y) = c("Male","Female","Unknown")
par(mar = c(5.1, 4.1, 4.1, 7.1), xpd = TRUE)
       barplot(y, col = heat.colors(length(rownames(y))), width = 2, beside = TRUE,ylim=c(0,1))
       legend("topright", inset = c(-0.25, 0), fill = heat.colors(length(rownames(y))), 
       legend = rownames(y))
