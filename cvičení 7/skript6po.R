#přesměrujte se do složky biostat
setwd("C:/biostat")
#nebo file-change dir
#vyzkoušejte, kde pracujete teď
getwd()

#spustte Rcommander
library(Rcmdr)

#vyvolejte skript jménem skript6.R
file-open script file


#Načtěte data Kojení
#pokud jsou jinde, použijeme celou adresu souboru
load("C:/home/monika/přf/statistika1314/Data/Kojeni.RData")


#zjednodušte vyvolávání proměnných dat Kojení
attach(Kojeni)
#zadejte Kojeni jako aktivní data set

#1. Dvouvýběrový t-test
#1.1.Závisí střední hodnota porodní hmotnosti na pohlaví?
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
#Shapiro test v Commanderu: Statistics-Summaries-Test of normality
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

#1.2.Závisí stř. hodnota hmotnosti na pohlaví?
#Obrázek?
#Ověřte předpoklady a testujte.


#1.3.Liší se stř. hodnota porodní hmotnosti plánovaných a neplánovaných dětí?


#2.Párový t-test
#2.1.Chceme testovat, zda je střední hodnota výšky matky stejná jako výšky otce
#Testujme jednovýběrovým t-testem
#Vytvořme novou veličinu rozdílV
Kojeni$rozdilV <-  vyskaO-vyskaM
detach(Kojeni)
attach(Kojeni)
#Ověřme předpoklad jednovýběrového t-testu
qqPlot(Kojeni$rozdilV)
shapiro.test(Kojeni$rozdilV)
#Jednovýběrový t-test
with(Kojeni, (t.test(rozdilV, alternative='two.sided', mu=0.0, 
  conf.level=.95)))
#Párový t-test
#Použijme Statistics-Means-Paired t-test
with(Kojeni, (t.test(vyskao, vyskaM, alternative='two.sided', 
  conf.level=.95, paired=TRUE)))


#2.2 Co kdybychom chtěli testovat, zda je stř hodnota výšky matky o 10 cm menší než
#stř hodnota výšky otce?
#Vidíme hned výsledek?
#Jednovýběrový t-test
t.test(Kojeni$rozdilV, alternative='two.sided', mu=10, conf.level=.95)
#Použijme Statistics-Means-Paired t-test
t.test(Kojeni$vyskaO-10, Kojeni$vyskaM, alternative='two.sided', 
  conf.level=.95, paired=TRUE)
t.test(Kojeni$vyskaO, Kojeni$vyskaM, mu=10,alternative='two.sided', 
  conf.level=.95, paired=TRUE)

#3.Neparametrické testy dvouvýběrové
#3.1.Testujme, zda se střední hodnota porodní délky liší
#pro dívky a chlapce
#Obrázek
plotMeans(Kojeni$porDelka, Kojeni$Hoch, error.bars="conf.int", level=0.95)
#Ověření předpokladů

#Co když nemůžeme předpokládat normalitu?
#Wilcoxonův dvouvýběrový test Statistics-Nonparametric tests-
#Two sample Wilcoxon
tapply(Kojeni$porDelka, Kojeni$Hoch, median, na.rm=TRUE)
wilcox.test(porDelka ~ Hoch, alternative="two.sided", data=Kojeni)
#Jaký je závěr?

#Co by se stalo, kdybychom přesto použili t-test

#3.2. Zkoumejte rozdělení věku matek, které
#otěhotněly plánovaně a které otěhotněly neplánovaně. 
#Jsou rozdělení normální?
par(mfrow=c(2,2))
tapply(vekM,Plan,qqPlot)
tapply(vekM,Plan,shapiro.test)

#Zkuste udělat Wilcoxonův dvouvýběrový test

#Mají rozdělení stejný tvar?
with(Kojeni, Hist(vekM, groups=Plan, scale="frequency", breaks="Sturges", 
  col="darkgray"))

#empirické distribuční funkce
par(mfrow=c(2,2))
plot(ecdf(vekM[Plan==1]))
plot(ecdf(vekM[Plan==0]))
#Kolmogorov-Smirnovův test
ks.test(vekM[Plan==0],vekM[Plan==1])

#Zkuste provést také dvouvýběrový t-test


#3.3.Zkoumejte rozdělení věku matek, které kojily 6 měsíců a více a 
#věku matek, které kojily méně než 6 měsíců.
#Porovnejte tato rozdělení nějakým vhodným testem.


#4.Neparametrické testy párové
#4.A Testujme, zda je stř. hodnota věku otce stejná jako stř hodnota věku matky 
#alternativě, že stř hodnota věku matky je nižší.
#Ověřme normalitu
qqPlot(Kojeni$vekO-Kojeni$vekM)
with(Kojeni, shapiro.test(vekO-vekM))
#Znaménkový test
sum(vekO-vekM>0)
sum(vekO-vekM==0)
sum(vekO-vekM<0)

#použitím funkce pbinom
pbinom(c(81), size=85, prob=0.5, lower.tail=FALSE)
#použitím binom.test
binom.test(82,n=85,p=0.5,alternative="greater")
binom.test(82,n=85,p=0.5,alternative="two.sided")

#Wilcoxonův test Statistics-nonparametric tests-paired samples Wilcoxon
with(Kojeni, wilcox.test(vekO, vekM, alternative='two.sided', paired=TRUE))

#Co kdybychom přesto použili t-test

#4.B Co kdybychom chtěli testovat, že střední hodnota věku matek je o 3 roky nižší
#než otců proti alternativě, že rozdíl je jiný.
#Znaménkový test
sum(vekO-vekM-3>0)
sum(vekO-vekM-3==0)
sum(vekO-vekM-3<0)
binom.test(36,n=85,p=0.5,alternative="two.sided")

#Wilcoxonův test Statistics-nonparametric tests-paired samples Wilcoxon
with(Kojeni, wilcox.test(vekO, vekM, alternative='two.sided', mu=3, paired=TRUE))

#4.C Děti si měřily, zda mají širší levé nebo pravé zápěstí. Neměly měřítko,
#jen provázek. Z 20 dětí mělo 13 širší pravé, 3 měly obě stejná a 4 měly širší levé.
#Testujte, zda stř. hodnota levého a pravého jsou stejné.

