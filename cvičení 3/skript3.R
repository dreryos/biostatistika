#Potřebujete Kojeni.RData a skript3.R
#spustte Rcommander
library(Rcmdr)

#vyvolejte skript jménem skript3.R
file-open script file

#přesměrujte se do složky biostat
setwd("C:\\Users\\marek\\Desktop\\biostatistika\\cvičení 3\\skript3.R")

#načtení připravených dat již ve formátu RData
load("C:\\Users\\marek\\Desktop\\biostatistika\\cvičení 1\\Kojeni.RData")
#nebo pomocí Commanderu: Data-Load data set

#prohlížení struktury dat
str(Kojeni)

#Znovu si vyrobíme faktor ftrvani (0-3,4-11,12-23,24)
Kojeni <- within(Kojeni, {
  ftrvani <- Recode(trvani, '0:3="1"; 4:11="2"; 12:23="3"; 24="4"', as.factor=TRUE)
})

#zadejte Kojeni jako aktivní data set
attach(Kojeni)


#1.Vztah kvalitativní a kvantitativní proměnné
#summaries-numerical summaries-summarize by groups
numSummary(Kojeni[,"hmotnost"], groups=Kojeni$ftrvani, statistics=c("mean", 
  "sd", "quantiles"), quantiles=c(0,.25,.5,.75,1))
#možné použít funkci tapply
tapply(hmotnost, ftrvani, summary)
tapply(hmotnost, ftrvani, sd)

#grafické znázornění
#graphs-boxplot-plot by groups
boxplot(hmotnost~ftrvani, ylab="hmotnost", xlab="ftrvani", data=Kojeni)

#Obrázek průměrů dle podskupin
#Graphs-Plot of means (sd)
plotMeans(Kojeni$hmotnost, Kojeni$ftrvani, error.bars="sd")

#histogramy dle podskupin
#Grapgs-Histogram- plot by groups
with(Kojeni, Hist(hmotnost, groups=ftrvani, scale="frequency", breaks="Sturges", col="darkgray"))

#1.B.Vyšetřete závislost mezi věkem matky a dobou kojení (kategoricky)
numSummary(Kojeni[,"vekM", drop=FALSE], groups=Kojeni$ftrvani, statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,
  .75,1))
Boxplot(vekM~ftrvani, data=Kojeni, id=list(method="y"))

#2.Závislost mezi dvěma kvalitativními proměnnými


#2.A.Vyšetřete závislost mezi dobou kojení (kategoricky) a vzděláním matky
#statistics-contingency table-two way table (zvolte marginální četnosti)

#možné vytvořit pomocí příkazů (margin - řádky nebo sloupec, okrajový součet)
table(ftrvani,Vzdelani)
prop.table(table(ftrvani,Vzdelani), margin=1)*100
prop.table(table(ftrvani,Vzdelani), margin=2)*100
prop.table(table(ftrvani,Vzdelani))*100

#grafické zobrazení
plot(ftrvani~Vzdelani)
plot(ftrvani~Vzdelani, col=terrain.colors(4))
plot(ftrvani~Vzdelani, col=rainbow(4))
#obracene
plot(Vzdelani~ftrvani, col=rainbow(3))

#barploty vedle sebe
barplot(table(ftrvani,Vzdelani),beside=T,col=rainbow(4))
barplot(table(Vzdelani,ftrvani),beside=T,col=rainbow(3))

#Commander barplot

#2.B.Vyšetřete závislost mezi vzděláním matky a tím, zda byl otec u porodu  

#2.C.Vyšetřete závislost mezi trváním kojení (kategoricky) a tím, zda mělo
#miminko dudlík

#3. Co jsou to z-skory? Šikmost, špičatost.
#spočítejte z-skory porodní hmotnosti, zkontrolujte průměr, rozptyl
zph<-(porHmotnost-mean(porHmotnost))/sd(porHmotnost)
mean(zph)
var(zph)
#nebo
scale(porHmotnost)
#Spočítejte šikmost a špičatost, o čem tyto charakteristiky vypovídají?
mean(zph^3)
mean(zph^4)-3
#Získáte pomocí numSummary (typ 3)

numSummary(Kojeni[,"porHmotnost"], statistics=c("skewness", "kurtosis"), 
  quantiles=c(0,.25,.5,.75,1,0.1), type="3")

#Nakreslete histogram porodní hmotnosti
Hist(Kojeni$porHmotnost, scale="frequency", breaks="Sturges", 
  col="darkgray")
#Spočítejte šikmost a špičatost pro vekM, vekO, hmotnost, vyskaO, delka
#Porovnejte s histogramy veličin.

detach(Kojeni)

library(abind, pos=17)
library(e1071, pos=18)



