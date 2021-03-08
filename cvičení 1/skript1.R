#Založte si složku biostat, (C:\biostat)
#kde budete ukládat svou práci, apod.

#Zavoláme R, objeví se >

#R používá různé znaky (#&$^<>"!), dostanete se k 
#nim buď přepnutím na anglickou klávesnici nebo
#se musíte naučit je vytvářet v české.

#1.R jako kalkulačka
#sčítání
3+5
#odčítání
7-15
#násobení
10*15
#dělení
5/12
#umocnění
6^3
#odmocnění
6^(1/2)
6^0.5
sqrt(6)
#exponenciála (e^x)
exp(2)
#logaritmus přirozený!!!
log(2)
#log s jiným základem
log(2,base=10)
log10(2)
#goniometrické funkce
pi
sin(pi/2)
cos(pi)
tan(pi/4)
#faktoriál
factorial(5)
#kombinační číslo (5 nad 2)
choose(5,2)

#2.Spustte R commander
library(Rcmdr)
#vyvolejte skript jménem skript1.R
#file-open script file

# znak # znamená komentář, R takový řádek ignoruje
# příkazy se píší každý na zvláštní řádek (nebo se musí oddělit ;)
# upravené skripty ukládejte


#zjistíme, ve které složce pracujeme
getwd()
#přesměrujeme do složky biostat
setwd("C:/biostat")
#nebo file-change dir
#vyzkoušejte, kde pracujete teď
getwd()


#Ctrl-R (Submit) vyvolá a provede řádek, na kterém je kursor
#Ctrl-C Ctrl-R (Submit) vyvolá a provede označený blok
#Vyzkoušejte!


#pokud něco nevíte, je možno najít v manuálu přes Help 
#(Help v commanderu a na consoli)

#3.Zavedení proměnných
#přiřazení
a<-5
#nebo
a=5
b<-3
#provedlo se to?
a
b
#můžeme počítat
a+b
a*b
#pojmenujme výsledek
soucet<-a+b
soucet
#chceme-li vidět součet hned
(soucet<-a+b)
#můžeme vytvořit vektor
cisla<-c(1,2,3,4,5)
cisla
7*cisla
cisla+a


#4.Vytvořme data
vyska<-c(41,65,57,82,73)
dobak<-c(7,10,12,38,27)

#spočítáme rychlost růstu
rych<-vyska/(120-dobak)
rych
#zaokrouhlení na 1 desetinné místo
round(rych,2)
rych<-round(rych,2)
rych

#jména
(druh<-c("fazole","cocka","hrach","zito","jecmen"))
#znakové výrazy v uvozovkách
(names(vyska)<-druh)
names(dobak)<-druh 
#aby měla jméno i rych, musíme je buď znovu vytvořit nebo pojmenovat
names(rych)<-druh
rych


#dataframe
(kliceni<-data.frame(vyska,dobak,rych))
#můžeme vkládat i nečíselné vektory
#textové
barva<-c("tmava","stredni","tmava","svetla","svetla")
#logické (T=pravda, F=nepravda)
(lustenina<-c(T,T,T,F,F))
(kliceni<-data.frame(kliceni,barva,lustenina))

#prohlížení dat
class(kliceni)
kliceni$vyska
vyska
#proč tam vyska je?
class(kliceni$barva)
class(kliceni$rych)
class(kliceni$lustenina)
dim(kliceni)

kliceni[2,3]
#první řádek
kliceni[1,]
#řádek hrach
kliceni["hrach",]
#čtvrtý sloupec
kliceni[,4]
#sloupec barva
kliceni[,"barva"]
#první a druhý řádek
kliceni[c(1,2),]
#řádky zito a jecmen
kliceni[c("zito","jecmen"),]
#vše bez prvního řádku
kliceni[-1,]
#druhý až čtvrtý řádek
kliceni[2:4,]
#třetí až pátý řádek bez pátého sloupce
kliceni[3:5,-5]

#co jsme vytvořili?
ls()
#smažte co nechcete
rm(a,b,vyska,barva,dobak,rych,lustenina)

#vyvolává se s dolarem
vyska
kliceni$vyska
#nebo můžeme použít
attach(kliceni)
#pozor, když data měníme, může dojít ke zmatku!
vyska


#obrazky
plot(rych~vyska,data=kliceni, xlab="výška v mm", ylab="rychlost v mm/hod")
plot(rych~vyska,data=kliceni, xlab="výška v mm", ylab="rychlost v mm/hod",
main="Rychlost růstu")
plot(rych~vyska,data=kliceni[3:5,], xlab="výška v mm", ylab="rychlost v mm/hod",
main="Rychlost růstu")

detach(kliceni)
#uložení dat
save(kliceni,file="C:/biostat/data/kliceni.RData")

#5.Nahrajte data Kojeni.RData
load("C:/home/monika/přf/statistika1314/data/Kojeni.RData")
#nebo Data-load data set

#prohlížení dat
str(Kojeni)
attach(Kojeni)
dim(Kojeni)
names(Kojeni)

#6.Míry polohy pro kvantitativní veličinu
#Zabývejme se proměnnou věk otce (vekO)
#spočítejte průměr, medián, kvartily, jiné kvantily 
#(např první a poslední decil), minimum, maximum

#přímými příkazy
min(vekO)
max(vekO)
mean(vekO)
median(vekO)
quantile(vekO,0.25)
sort(vekO)

summary(vekO)


#Jak najít modus?
table(vekO)

#7.Spočítejte míry polohy  
#porodní hmotnosti pomocí Commanderu
#Statistics-Summaries-Numerical Summaries

#8.Grafické znázornění kvantitativní veličiny
#8.A Histogram
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

#8.B Krabicový diagram (boxplot)
#Nakreslete boxplot vekM. Kde hledat popisné statistiky?
#Graphs-Boxplot
#med,kvartily, 3/2 kvart. rozpětí, odlehlá poz.
Boxplot( ~ vekM, data=Kojeni, id.method="y")

#Nakreslete boxplot věku otců
#Nakreslete boxplot porHmotnost
#Vylepšete obrázek (vybarvit, udělat hlavní titulek)

#Všechny barvy
colors()

#9.C Empirická distribuční funkce
plot(ecdf(vekM))
plot(ecdf(vekO))


#porovnejte různé metody zobrazování kvantitativních veličin
par(mfrow=c(2,2))

#9. Shrnutí kategorických proměnných
#9.A tabulka
#Zjistěte, kolik matek je v kategoriích podle vzdělání
table(Vzdelani) 
#Pouzijte Summaries-frequency distributions
library(abind, pos=15)
library(e1071, pos=16)
local({
  .Table <- with(Kojeni, table(Vzdelani))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})

#Sestavte tabulku pro počet otců u porodu

#Vytvořme kategorickou proměnnou z trvání kojení (0-3,4-11,12-23,24)
table(trvani)
#Data-manage variable in active data set-recode variable
Kojeni <- within(Kojeni, {
  ftrvani <- Recode(trvani, 
  '0:3="jeden"; 4:11="tri"; 12:23="pet"; 24="sest"; ;', as.factor.result=TRUE)
})
#Sestavte tabulku
#Vytvořte uspořádaný faktor
#data-manage variables in active data set-reorder factor levels
Kojeni$ftrvani <- with(Kojeni, factor(ftrvani, levels=c('jeden','tri','pet',
  'sest')))
#znovu sestavme tabulku
#Zkuste vyvolat ftrvani
ftrvani
#Proč ji R nenašel?
detach(Kojeni)
attach(Kojeni)
ftrvani

#9.B Grafické znázornění kategorických proměnných
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



detach(Kojeni)


#konec práce: uložte skript, práci.
