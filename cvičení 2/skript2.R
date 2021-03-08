#Potřebujete Kojeni.RData a skript2.R
#spustte Rcommander
library(Rcmdr)

#vyvolejte skript jménem skript2.R
file-open script file

#přesměrujte se do složky biostat
setwd("C:/biostat")

#načtení připravených dat již ve formátu Rdata
load("C:/home/monika/přf/statistika1314/data/Kojeni.RData")
#nebo pomocí Commanderu: Data-Load data set

#prohlížení struktury dat
str(Kojeni)
#zjednodušte vyvolávání proměnných dat Kojení
delka
Kojeni$delka
attach(Kojeni)
#zadejte Kojeni jako aktivní data set


#1.Grafické znázornění kvantitativní veličiny
#1.A Histogram
#Sestavte histogram věku matek, použijte nabídku Rcmdr
#Graphs-Histogram
with(Kojeni, Hist(vekM, scale="frequency", breaks="Sturges", 
  col="darkgray"))
#Co znamenají parametry: scale, breaks, col?
#Upravte parametry
with(Kojeni, Hist(vekM, scale="density", breaks="Sturges", 
  col="darkgray"))
with(Kojeni, Hist(vekM, scale="density", breaks=seq(18,39,3), 
  col="darkgray"))
#Co je seq? Vyzkoušejte.
with(Kojeni, Hist(vekM, scale="density", breaks=c(18,20,22,24,26,28,32,38), 
  col="darkgray"))
with(Kojeni, Hist(vekM, scale="density", breaks=c(18,20,22,24,26,28,32,38), 
  col="red"))


#Podobně sestavte histogram věku otců
#porovnejte 
par(mfrow=c(2,2))
par(mfrow=c(1,1))



#1.B Krabicový diagram (boxplot)
#Nakreslete boxplot vekM. Kde hledat popisné statistiky?
#Graphs-Boxplot
#med,kvartily, 3/2 kvart. rozpětí, odlehlá poz.
Boxplot( ~ vekM, data=Kojeni, id.method="y")

#Nakreslete boxplot věku otců
#Nakreslete boxplot porHmotnost
#Vylepšete obrázek (vybarvit, udělat hlavní titulek)

#1.C Empirická distribuční funkce
plot(ecdf(vekM))
plot(ecdf(porHmotnost))


#porovnejte různé metody zobrazování kvantitativních veličin
par(mfrow=c(2,2))



#2. Shrnutí kategorických proměnných
#2.A tabulka
#Zjistěte, kolik matek je v kategoriích podle vzdělání
table(Vzdelani) 
#Sestavte tabulku pro počet otců u porodu

#Vytvořme kategorickou proměnnou z trvání kojení (0-3,4-11,12-23,24)
table(trvani)
#Data-manage variable in active data set-recode variable
Kojeni <- within(Kojeni, {
    ftrvani <- Recode(trvani, "0:3=\"do mesice\"; 4:11=\"1-3\"; 12:23=\"3-6\"; 24=\"6 a vice\"", 
        as.factor = TRUE)
})
#Sestavte tabulku
#Vytvořte uspořádaný faktor
#data-manage variables in active data set-reorder factor levels
Kojeni$ftrvani <- with(Kojeni, factor(ftrvani, levels = c("do mesice", "1-3", "3-6", 
    "6 a vice"), ordered = TRUE))
#znovu sestavme tabulku
#Zkuste vyvolat ftrvani
ftrvani
#Proč ji R nenašel?
detach(Kojeni)
attach(Kojeni)
ftrvani

#2.B Grafické znázornění kategorických proměnných
#Sestavte graf Vzdelani, Otec, ftrvani
#použijte Graphs-bar graph

#Sestavte koláčový diagram (pie chart) těchto veličin
#Graphs-pie chart
library(colorspace, pos=17)
with(Kojeni, pie(table(Vzdelani), labels=levels(Vzdelani), xlab="", ylab="",
   main="Vzdelani", col=rainbow_hcl(length(levels(Vzdelani)))))

#Jiné barvy?
with(Kojeni, pie(table(ftrvani), labels=levels(ftrvani), xlab="", ylab="", 
  main="ftrvani", col=heat.colors(length(levels(ftrvani)))))
with(Kojeni, pie(table(ftrvani), labels=levels(ftrvani), xlab="", ylab="", 
  main="ftrvani", col=rainbow(length(levels(ftrvani)))))
with(Kojeni, pie(table(ftrvani), labels=levels(ftrvani), xlab="", ylab="", 
  main="ftrvani", col=terrain.colors(length(levels(ftrvani)))))
with(Kojeni, pie(table(ftrvani), labels=levels(ftrvani), xlab="", ylab="", 
  main="ftrvani", col=c("lightskyblue2","turquoise3","green2","green4")))

#3.Míry variability: počítejte výběrový rozptyl, směrodatnou odchylku, 
#kvartilové rozpětí, rozpětí
var(vekO)
sd(vekO)
sqrt(var(vekO))
(rozpeti<-max(vekO)-min(vekO))
(qrozpeti<-quantile(vekO,0.75)-quantile(vekO,0.25))

#Spočítejte výběrový rozptyl vekO bez použití funkcí 
#var, sd, mean (použijte sum, length)
(prumer<-sum(vekO)/length(vekO))
(rozptyl<-sum((vekO-prumer)^2)/(length(vekO)-1))
(stodch<-sqrt(rozptyl))
#variační koeficient
(varco<-stodch/prumer)

#použitím nabídky Rcommanderu
#Statistics-Summaries-Numerical Summaries

#4.Souvislost dvou kvantitativních veličin
#4.A.Souvisí věk matky s věkem otce? Spočítejte kovarianci, korelaci.
cov(vekM,vekO)
cor(vekM,vekO)
#Co kdybychom chtěli počítat bez funkcí cov, cor?
(kovariance<-sum((vekM-mean(vekM))*(vekO-mean(vekO)))/(length(vekM)-1))
(korelace<-kovariance/(sd(vekM)*sd(vekO)))
#Pomocí commanderu
#Statistics-summaries-cor matrix

#Znázorněte graficky souvislost vekO a vekM
#Graphs-scatterplot
#znázorněte různými znaky pro různé úrovně vzdělání matky
#jak doplnit čísla některých pozorování

#4.B.Spočítejte korelaci mezi porodní váhou a výškou matky
#nakreslete scatterplot, podívejte se na odlehlá pozorování, označte zvlášť
#dívky a chlapce

#4.C.Souvisí doba trvání kojení s váhou dítěte v 6 měsících?
#Co když použijeme zkategorizovanou dobu kojení?

#5.Vztah kvalitativní a kvantitativní proměnné
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

#5.B.Vyšetřete závislost mezi věkem matky a dobou kojení (kategoricky)


#6.Závislost mezi dvěma kvalitativními proměnnými

#6.A.Vyšetřete závislost mezi dobou kojení (kategoricky) a vzděláním matky
#statistics-contingency table-two way table (zvolte marginální četnosti)

#možné vytvořit pomocí příkazů
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

#6.B.Vyšetřete závislost mezi vzděláním matky a tím, zda byl otec u porodu  


#6.C.Vyšetřete závislost mezi trváním kojení (kategoricky) a tím, zda mělo
#miminko dudlík

#7. Co jsou to z-skory? Šikmost, špičatost.
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


