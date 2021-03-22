#přesměrujte se do složky biostat
setwd("C:/biostat")
#nebo file-change dir
#vyzkoušejte, kde pracujete teď
getwd()

#spustte Rcommander
library(Rcmdr)

#vyvolejte skript jménem skript4.R
file-open script file


#1. Binomické rozdělení
#1.A Hod 10 mincemi Bi(10,0.5)
#Distributions-Discrete distributions-Binomial distributions
#Plot
#Binomial probabilities
(prsti<-dbinom(0:10, size = 10, prob = 0.5))

#střední hodnota a rozptyl pomocí spočítaných pravděpodobností
(hodnoty<-seq(0,10,1))
(str<-sum(hodnoty*prsti))
sum((hodnoty-str)^2*prsti)

#Jaká je pravděpodobnost, že počet líců je větší než 7?
pbinom(c(7), size=10, prob=0.5, lower.tail=F)
#Jaká je pravděpodobnost, že po4et líců je menší než 2?
pbinom(c(1), size=10, prob=0.5, lower.tail=T)

#1.B 19 studentů, každého nezávisle s pravděpodobností 0.05 chytí revizor.
#Rozdělení počtu chycených studentů?
#Obrázky rozdělení, distribuční funkce

#Jaká je pravděpodobnost, že není chycen žádný student?
dbinom(0, size = 19, prob = 0.05)
#Jaká je pravděpodobnost, že jsou chyceni více než 4 studenti?
pbinom(c(4), size=19, prob=0.05, lower.tail=FALSE)
#Střední hodnota a rozptyl počtu chycených studentů?
#Chceme najít minimální počet studentů m tak, aby pravděpodobnost, že jich bude 
#chyceno více byla menší než 0.05 
qbinom(c(0.95), size=19, prob=0.05, lower.tail=T)
#generátor binomického rozdělení
(vyber<-rbinom(1000,19,0.05))
table(vyber)
mean(vyber)
var(vyber)

#2.Poissonovo rozdělení
#Distributions-Discrete distributions-Poisson distribution
#Zkuste zkoumat Poissonovo rozdělení s různými 
#středními hodnotami (1,2,10,0.2)
#Obrázky
par(mfrow=c(2,2))

#Kdy se podobá Poissonovo binomickému se stejnou stř. hodnotou?
#Zkuste (n,p,lambda) (10,0.5,5) (10,0.2,2) (20,0.1,2) (100,0.1,10)
par(mfrow=c(2,2))
#Generátor Poissonova rozdělení

#3. Normální rozdělení
#Distributions-Continuous distributions-Normal distribution
#Jak vypadá distribuční funkce N(0,1), jak hustota?
par(mfrow=c(2,2))

#Jak se mění hustota se změnou mu, sigma?

#3.A Přijímací testy na střední školy N(50,18^2).
#Kolik % žáků je lepších, pokud můj výsledek je 70.
pnorm(c(70), mean=50, sd=18, lower.tail=FALSE)

#Kolika bodů je třeba dosáhnout, abych byl mezi 30% nejlepších?
qnorm(c(0.3), mean=50, sd=18, lower.tail=FALSE)

#Přihlásilo se 150 studentů, ředitel chce přijmout 30. 
#Kolik bodů bude přibližně potřeba na přijetí? 
qnorm(c(0.2), mean=50, sd=18, lower.tail=FALSE)

#3.B Výšky 10letých dívek N(140,7^2)
#Kolik % dívek je větších než 146?

#Kolik % dívek je menších než 130 cm?

#Jaká je pravděpodobnost, že výška náhodně vybrané dívky leží 
#v intervalu (135,145)?
pnorm(c(135,145), mean=140, sd=7, lower.tail=TRUE)

#Chci-li přijímat do basketbalového týmu jen 10% nejvyšších dívek, jak aspoň 
#vysoké musí být?

#3.C Jak bychom počítali, kdybychom měli pouze tabulky N(0,1)?
#P(X<130)=P((X-140)/7<(130-140)/7)=Fi(-10/7)

#3.D Generátor normálního rozdělení
(vyber<-rnorm(100,140,7))
mean(vyber)
var(vyber)


#4. Centrální limitní věta
#Poissonovo rozdělení Po(2)
par(mfrow=c(2,2))
po<-rpois(200,2)
hist(po)

#rozdělení průměru z 12 Pois(2) 
povel<-array(rpois(2400,2),dim=c(200,12))
povel[1:20,]
poprum<-rowMeans(povel)
hist(poprum)

#rozdělení průměru z 20 Pois(2) 
povel<-array(rpois(4000,2),dim=c(200,20))
poprum<-rowMeans(povel)
hist(poprum)

#rozdělení průměru z 50 Pois(2) 
povel<-array(rpois(10000,2),dim=c(200,50))
poprum<-rowMeans(povel)
hist(poprum)


#5.Interpretace intervalu spolehlivosti
#Generujeme výběry z normálního rozdělení N(100,15^2) o rozsahu 10,20,50,100
#počítáme intervaly spolehlivosti
par(mfrow=c(1,1))
nvec<-array(rnorm(1000,100,15),dim=c(100,10))
nvecprum<-rowMeans(nvec)
nvecd<-nvecprum-qnorm(0.975)*15/sqrt(10)
nvech<-nvecprum+qnorm(0.975)*15/sqrt(10)
int<-cbind(nvecd,nvecprum,nvech)
rada<-c(1:100)
sto<-rep(100,100)
matplot(rada,int,pch=c(1,4,1),col=c("red","black","green"),xlab="100 výběrů",
ylab="Interval spolehlivosti", main="Velikost výběru 10")
matlines(rada,sto)
segments(rada,nvecd,rada,nvech)

nvec<-array(rnorm(2000,100,15),dim=c(100,20))
nvecprum<-rowMeans(nvec)
nvecd<-nvecprum-qnorm(0.975)*15/sqrt(20)
nvech<-nvecprum+qnorm(0.975)*15/sqrt(20)
int<-cbind(nvecd,nvecprum,nvech)
rada<-c(1:100)
sto<-rep(100,100)
matplot(rada,int,pch=c(1,4,1),col=c("red","black","green"),xlab="100 výběrů",
ylab="Interval spolehlivosti", main="Velikost výběru 20")
matlines(rada,sto)
segments(rada,nvecd,rada,nvech)

nvec<-array(rnorm(5000,100,15),dim=c(100,50))
nvecprum<-rowMeans(nvec)
nvecd<-nvecprum-qnorm(0.975)*15/sqrt(50)
nvech<-nvecprum+qnorm(0.975)*15/sqrt(50)
int<-cbind(nvecd,nvecprum,nvech)
rada<-c(1:100)
sto<-rep(100,100)
matplot(rada,int,pch=c(1,4,1),col=c("red","black","green"),xlab="100 výběrů",
ylab="Interval spolehlivosti", main="Velikost výběru 50")
matlines(rada,sto)
segments(rada,nvecd,rada,nvech)

nvec<-array(rnorm(10000,100,15),dim=c(100,100))
nvecprum<-rowMeans(nvec)
nvecd<-nvecprum-qnorm(0.975)*15/sqrt(100)
nvech<-nvecprum+qnorm(0.975)*15/sqrt(100)
int<-cbind(nvecd,nvecprum,nvech)
rada<-c(1:100)
sto<-rep(100,100)
matplot(rada,int,pch=c(1,4,1),col=c("red","black","green"), xlab="100 výběrů",
ylab="Interval spolehlivosti", main="Velikost výběru 100")
matlines(rada,sto)
segments(rada,nvecd,rada,nvech)


#6.Intervaly spolehlivosti pro stř. hodnotu
#Načtěte data Kojení
load("C:/home/monika/přf/statistika1314/Data/Kojeni.RData")

#zjednodušte vyvolávání proměnných dat Kojení
attach(Kojeni)
#zadejte Kojeni jako aktivní data set

#6.A Zkoumejme proměnnou hmotnost (hmotnost ve 24 týdnu)
#Udělejte boxplot,histogram,normální diagram
#Mohla by mít hmotnost normální rozdělení?
par(mfrow=c(2,2))
boxplot(Kojeni$hmotnost, ylab="hmotnost",col="purple")
Hist(Kojeni$hmotnost, scale="frequency", breaks="Sturges", col="purple")
#Normální diagram
qqnorm(hmotnost)
qqline(hmotnost,col="red")
#pomocí Commanderu Graph-quantile comp plot

#Odhadněte jakou má rozdělení hmotnosti střední hodnotu 
#(bodově i intervalově)
mean(hmotnost)
mean(hmotnost)-qt(c(0.975), df=98, lower.tail=TRUE)*sd(hmotnost)/sqrt(99)
mean(hmotnost)+qt(c(0.975), df=98, lower.tail=TRUE)*sd(hmotnost)/sqrt(99)
#Zvolte Statistics-means-single sample t-test
t.test(Kojeni$hmotnost, alternative='two.sided', mu=0.0, conf.level=.95)

#Uložme meze intervalu spolehlivosti
tt<-t.test(Kojeni$hmotnost, alternative='two.sided', mu=0.0, conf.level=.95)
(isp<-tt$conf.int)
#Vyznačme interval spolehlivosti v boxplotu
boxplot(Kojeni$hmotnost, ylab="hmotnost",col="purple")
abline(h=isp)
#v histogramu
Hist(Kojeni$hmotnost, scale="frequency", breaks="Sturges", col="purple")
abline(v=isp)

#Kolik pozorování leží uvnitř intervalu spolehlivosti pro stř. hodnotu?
sum(hmotnost>isp[1]&hmotnost<isp[2])


#6.B Může mít také porodní hmotnost normální rozdělení?
par(mfrow=c(2,2))

#Najděte bodový a intervalový odhad střední hodnoty

