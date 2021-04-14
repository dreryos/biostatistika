#přesměrujte se do složky biostat
setwd("C:/biostat")
#nebo file-change dir
#vyzkoušejte, kde pracujete teď
getwd()

#spustte Rcommander
library(Rcmdr)

#vyvolejte skript jménem skript5.R
file-open script file


#Načtěte data Kojení
data(Kojeni)
#pokud jsou jinde, použijeme celou adresu souboru
load("C:/home/monika/přf/statistika1314/Data/Kojeni.RData")


#zjednodušte vyvolávání proměnných dat Kojení
attach(Kojeni)
#zadejte Kojeni jako aktivní data set

#1.Interval spolehlivosti pro stř. hodnotu N rozdělení 
#1.A Zkoumejme proměnnou hmotnost (hmotnost ve 24 týdnu)
#Udělejte boxplot,histogram,normální diagram
#Mohla by mít hmotnost normální rozdělení?
par(mfrow=c(2,2))
boxplot(Kojeni$hmotnost, ylab="hmotnost",col="purple")
Hist(Kojeni$hmotnost, scale="frequency", breaks="Sturges", col="purple")
qqPlot(Kojeni$hmotnost)
qqnorm(hmotnost)
qqline(hmotnost,col="purple")

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


#1.B Může mít také porodní hmotnost normální rozdělení?
par(mfrow=c(2,2))

#Najděte bodový a intervalový odhad střední hodnoty


#1.C. Prozkoumejte porodní hmotnost zvlášť pro dívky a chlapce
boxplot(porHmotnost~Hoch, ylab="porHmotnost", xlab="Hoch", data=Kojeni)
par(mfrow=c(2,2))
Hist(Kojeni$porHmotnost[Hoch=="hoch"], scale="frequency", breaks="Sturges", 
  col="skyblue")
Hist(Kojeni$porHmotnost[Hoch=="dívka"], scale="frequency", breaks="Sturges", 
  col="pink")

qqPlot(Kojeni$porHmotnost[Hoch=="hoch"], col="blue")
qqPlot(Kojeni$porHmotnost[Hoch=="dívka"], col="red")
tapply(porHmotnost,Hoch,qqPlot)

#Najděte intervalový a bodový odhad střední hodnoty pro dívky a 
#chlapce zvlášť
tapply(porHmotnost, Hoch,t.test)
#grafické zobrazení
plotMeans(Kojeni$porHmotnost, Kojeni$Hoch, error.bars="conf.int", 
  level=0.95)


#1.D Najděte bodový a intervalový odhad věku matky. Ověřte předpoklady.

#2.Intervaly spolehlivosti pro pravděpodobnost
#Mám výběr 19 studentů Přf. Chci odhadnout, jaká je pravděpodobnost, že student
#Přf UK umí (aspoň trochu) německy:
en<-19
yps<-12
(odh<-yps/en)
(isp1<-odh-sqrt(odh*(1-odh)/en)*qnorm(0.975))
(isp2<-odh+sqrt(odh*(1-odh)/en)*qnorm(0.975))


#3.Testování pravděpodobnosti jevu
#Nulová hypotéza p=1/2, oboustranná alternativa
binom.test(12,19,p=0.5,alternative="two.sided")


#4.Jednovýběrový t-test
#4.A. Chceme rozhodnout, zda střední hodnota hmotnosti je 7900 gramů, 
#oproti alternativě, že se tomuto číslu nerovná
#hladinu významnosti volme 0.05
t.test(Kojeni$hmotnost, alternative='two.sided', mu=7900, conf.level=.95)
#Co když změníme hladinu na 0.01. Musíme celý příklad přepočítat?

#Jak souvisí výsledek testu s intervalem spolehlivosti?

#Zkusme testovat, proti alternativě, že střední hodnota je menší než 7900
t.test(Kojeni$hmotnost, alternative='less', mu=7900, conf.level=.95)

#Ověření předpokladů t-testu


#Zabývejme se podmnožinou chlapců
#4.B. Vytvořme nový datový soubor KojeniH
detach(Kojeni)
#Použijte Data-Active data set-subset
KojeniH <- subset(Kojeni, subset=Hoch=="hoch")
dim(KojeniH)
#Testujme, zda stř hodnota hmotnosti mezi chlapci je 7900, hladina 0.05
with(KojeniH, (t.test(hmotnost, alternative='two.sided', mu=7900, 
  conf.level=.95)))

#4.C. Zkusme totéž pro podmnožinu dívek

#ověřme předpoklady pro dívky i chlapce
attach(Kojeni)
par(mfrow=c(2,2))
tapply(hmotnost,Hoch,qqPlot)

#4.D. Má výška otce normální rozdělení?
#Vraťte Kojeni jako aktivní data set
#Testujte, zda je střední hodnota výšky otce 182 cm, ověřte předpoklady

#4.E.Testujme, zda porodní hmotnost má střední hodnotu 3500 g
#proti alternativě, že stř. hodnota je jiná.


#4.F.Testujte, zda věk matek má střední hodnotu 165 cm. Ověřte předpoklady.


#5.Síla jednovýběrového t-testu
#Chci testovat, zda porodní hmotnost má mu0=3500
#Na čem závisí síla?
#Jak zvolit rozsah výběru, aby síla na alternativu mu1-mu0=100g byla
#aspoň 0.8?
#volme směrodatnou odchylku 440
((qnorm(0.975)+qnorm(0.8))/100)^2*440^2
#Co když chceme sílu 0.9
((qnorm(0.975)+qnorm(0.9))/100)^2*440^2
#Co když zmenšíme rozdíl alternativ na 50?
((qnorm(0.975)+qnorm(0.8))/50)^2*440^2

#6. Dvouvýběrový t-test
#6.1.Závisí střední hodnota porodní hmotnosti na pohlaví?
#Obrázek
plotMeans(Kojeni$porHmotnost, Kojeni$Hoch, error.bars="conf.int", 
  level=0.95)
#Jaké jsou předpoklady dvouvýběrového t-testu?
#Nezávislost
#Normalita
par(mfrow=c(2,2))
qqPlot(Kojeni$porHmotnost[Hoch=="hoch"], dist= "norm")
qqPlot(Kojeni$porHmotnost[Hoch=="dívka"], dist= "norm")
shapiro.test(Kojeni$porHmotnost[Hoch=="hoch"])
shapiro.test(Kojeni$porHmotnost[Hoch=="dívka"])
#nebo
tapply(porHmotnost,Hoch, qqPlot)
tapply(porHmotnost,Hoch,shapiro.test)
#Shodné rozptyly
boxplot(porHmotnost~Hoch, ylab="porHmotnost", xlab="Hoch", data=Kojeni)
#Test na shodnost rozptylů: Statistics-Variances-F-test
tapply(Kojeni$porHmotnost, Kojeni$Hoch,  var, na.rm=TRUE)
var.test(porHmotnost ~ Hoch, alternative='two.sided', conf.level=.95, 
  data=Kojeni)
leveneTest(Kojeni$porHmotnost, Kojeni$Hoch)
bartlett.test(porHmotnost ~ Hoch, data=Kojeni)

#T-test se stejnými rozptyly
t.test(porHmotnost~Hoch, alternative='two.sided', conf.level=.95, 
  var.equal=TRUE, data=Kojeni)
#Jaký je závěr?
#Jaký je význam intervalu spolehlivosti?

#T-test bez požadavku shodných rozptylů
t.test(porHmotnost~Hoch, alternative='two.sided', conf.level=.95, 
  var.equal=FALSE, data=Kojeni)
#Co se změnilo?

#6.2.Závisí stř. hodnota hmotnosti na pohlaví?
#Obrázek?
#Ověřte předpoklady a testujte.
