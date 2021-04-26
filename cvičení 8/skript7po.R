#přesměrujte se do složky biostat
setwd("C:/biostat")
#nebo file-change dir
#vyzkoušejte, kde pracujete teď
getwd()

#spustte Rcommander
library(Rcmdr)

#vyvolejte skript jménem skript7po.R
file-open script file


#Načtěte data Kojení
#pokud jsou jinde, použijeme celou adresu souboru
load("C:/home/monika/přf/statistika1314/Data/Kojeni.RData")


#zjednodušte vyvolávání proměnných dat Kojení
attach(Kojeni)
#zadejte Kojeni jako aktivní data set

#1.ANOVA-Jednoduché třídění
#1.1. Závisí hmotnost dítěte ve 24 týdnech na vzdělání matky?
#Spočítejte popisné statistiky
numSummary(Kojeni[,"hmotnost"], groups=Kojeni$Vzdelani, statistics=c("mean",
   "sd", "quantiles"), quantiles=c(0,.25,.5,.75,1))
#Nakreslete obrázek
plotMeans(Kojeni$hmotnost, Kojeni$Vzdelani, error.bars="conf.int", 
  level=0.95)
#Ověřme předpoklady ANOVy
#Normalita
par(mfrow=c(2,2))
tapply(hmotnost,Vzdelani,qqPlot)
#Shodné rozptyly
boxplot(hmotnost~Vzdelani, ylab="hmotnost", xlab="Vzdelani", data=Kojeni)
tapply(Kojeni$hmotnost, Kojeni$Vzdelani, var, na.rm=TRUE)
leveneTest(Kojeni$hmotnost, Kojeni$Vzdelani)
tapply(Kojeni$hmotnost, Kojeni$Vzdelani, var, na.rm=TRUE)
bartlett.test(hmotnost ~ Vzdelani, data=Kojeni)

#Provedeme analýzu rozptylu
#Statistics-Means-One way anova
mod1 <- aov(hmotnost ~ Vzdelani, data=Kojeni)
summary(mod1)
numSummary(Kojeni$hmotnost , groups=Kojeni$Vzdelani, statistics=c("mean", 
  "sd"))

#Jaký je závěr?

#Ověřme normalitu pomocí reziduí z modelu
qqPlot(mod1$residuals)
shapiro.test(mod1$residuals)


#1.2.Závisí výška otce na vzdělání matky?
numSummary(Kojeni[,"vyskaO"], groups=Kojeni$Vzdelani, statistics=c("mean", 
  "sd", "quantiles"), quantiles=c(0,.25,.5,.75,1))
plotMeans(Kojeni$vyskaO, Kojeni$Vzdelani, error.bars="conf.int", level=0.95)
boxplot(vyskaO~Vzdelani, ylab="porHmotnost", xlab="Vzdelani", 
  data=Kojeni)
#Ověření předpokladů
#Model
#Residua
qqPlot(mod2$residuals)
#Mnohonásobná porovnání v Commanderu Tukey nebo
TukeyHSD(mod2)

#1.3. Co se stane, když testujeme rozdíl mezi dvěma skupinami v ANOVě
#Porodní hmotnost chlapci a dívky
t.test(porHmotnost~Hoch, alternative='two.sided', conf.level=.95, 
  var.equal=TRUE, data=Kojeni)
mod3 <- aov(porHmotnost ~ Hoch, data=Kojeni)
summary(mod3)

#1.4. Závisí věk otce na vzdělání matky?
plotMeans(Kojeni$vekO, Kojeni$Vzdelani, error.bars="conf.int", level=0.95)
#Ověření předpokladů
par(mfrow=c(2,2))
tapply(vekO,Vzdelani,qqPlot)
tapply(vekO,Vzdelani,shapiro.test)
#Provedeme Kruskal-Wallisův test
#Statistics-Nonparametric-tests-Kruskal-Wallis test
tapply(Kojeni$vekO, Kojeni$Vzdelani, median, na.rm=TRUE)
kruskal.test(vekO ~ Vzdelani, data=Kojeni)
#Jaký je závěr?

#Co kdybychom přesto testovali v ANOVě?
#Normalita na reziduích
qqPlot(mod4$residuals)
shapiro.test(mod4$residuals)

#1.5. Závisí doba trvání kojení na vzdělání matky


#2.Dvojné třídění
#2.1 Závisí doba trvání kojení na vzdělání matky a na tom, zda dítě bylo hned po 
#porodu přiloženo k prsu?
#Obrázek
par(mfrow=c(1,1))
with(Kojeni, plotMeans(trvani, Vzdelani, Prs, error.bars="conf.int", 
  level=0.95))
#Model s interakcemi
mod6 <- (lm(trvani ~ Prs*Vzdelani, data=Kojeni))
Anova(mod6)
with(Kojeni, (tapply(trvani, list(Prs, Vzdelani), mean, na.rm=TRUE))) 
  # means
with(Kojeni, (tapply(trvani, list(Prs, Vzdelani), sd, na.rm=TRUE))) 
  # std. deviations
with(Kojeni, (tapply(trvani, list(Prs, Vzdelani), function(x) 
  sum(!is.na(x))))) # counts

#Ověření předpokladů
qqPlot(mod6$residuals)
#Shodné rozptyly
boxplot(trvani~Vzdelani:Prs)
table(Vzdelani:Prs)
with(Kojeni, tapply(trvani, list(Prs, Vzdelani), var, na.rm=TRUE))
leveneTest(trvani ~ Prs*Vzdelani, data=Kojeni, center="median")

#Model bez interakcí
mod7 <- (lm(trvani ~ Prs+Vzdelani, data=Kojeni))
Anova(mod7)

#Přejdeme k jednoduchému třídění -> mod5 (již spočítán)

detach(Kojeni)

load("C:/home/monika/přf/biostat1415/Data/Stulong_aov.RData")
str(Stulong)
attach(Stulong)

#2.2.Zkoumejme, zda hladina cholestrolu závisí na bmi (jako faktoru) a kouření.
#Obrázek
plotMeans(Stulong$chlst, Stulong$fbmi, Stulong$fkoureni, error.bars="conf.int")
#Model s interakcemi
with(Stulong, plotMeans(chlst, fbmi, fkoureni, error.bars="conf.int", 
  level=0.95))
mod8 <- (lm(chlst ~ fbmi*fkoureni, data=Stulong))
Anova(mod8)
with(Stulong, (tapply(chlst, list(fbmi, fkoureni), mean, na.rm=TRUE))) 
  # means
with(Stulong, (tapply(chlst, list(fbmi, fkoureni), sd, na.rm=TRUE))) 
  # std. deviations
with(Stulong, (tapply(chlst, list(fbmi, fkoureni), function(x) 
  sum(!is.na(x))))) # counts
#Ověření předpokladů
qq.plot(mod8$residuals)
shapiro.test(mod8$residuals)
#Shodné rozptyly
boxplot(chlst~fbmi:fkoureni, ylab="chlst", xlab="fbmi", data=Stulong)
table(fbmi:fkoureni)
tapply(Stulong$chlst, Stulong$fbmi:Stulong$fkoureni, var, na.rm=TRUE)
leveneTest(Stulong$chlst~Stulong$fbmi:Stulong$fkoureni)
#Ověření předpokladů pomocí diag. obrázků v Commanderu
#Models-Graphs-Basic diagnostic plots

#Proveďme logaritmickou transformaci
Stulong$logchlst <- log(chlst)
detach(Stulong)
attach(Stulong)
#Obrázek
#Model
mod9 <- (lm(logchlst ~ fbmi*fkoureni, data=Stulong))
Anova(mod9)
with(Stulong, (tapply(logchlst, list(fbmi, fkoureni), mean, na.rm=TRUE))) 
  # means
with(Stulong, (tapply(logchlst, list(fbmi, fkoureni), sd, na.rm=TRUE))) 
  # std. deviations
with(Stulong, (tapply(logchlst, list(fbmi, fkoureni), function(x) 
  sum(!is.na(x))))) # counts
#Ověření předpokladů
#Normalita
qq.plot(mod9$residuals)
#Shodné rozptyly
boxplot(logchlst~fbmi:fkoureni, ylab="chlst", xlab="fbmi", data=Stulong)
tapply(Stulong$logchlst, Stulong$fbmi:Stulong$fkoureni, var, na.rm=TRUE)
levene.test(Stulong$logchlst, Stulong$fbmi:Stulong$fkoureni)

#Model bez interakcí
mod10 <- (lm(logchlst ~ fbmi+fkoureni, data=Stulong))
Anova(mod10)
summary(mod10)

#Ověření předpokladů
#Normalita
qqPlot(mod10$residuals)


#2.3.Testujte závislost hladiny cholesterolu na kouření a pití vína.
#použijte log(cholesterol)
#obrázek
#model
#Ověření předpokladů

#přejít ke kombinovanému faktoru
Stulong$fkourvino<-fkoureni:fvino


#2.4.Zkoumejme, zda  systolický tlak závisí na fbmi a fkoureni
#Obrázek
#Model
#Ověření předpokladů
#Normalita
#Shodné rozptyly

#Vylepšení modelu transformací?
Stulong$logsyst1 <- log(syst1)
#Obrázek
#Model
#Ověření předpokladů

#model bez interakcí
#jednoduché třídění

#3.Korelace
#3.1.Souvisí výška matky s výškou otce?
#Obrázek
par(mfrow=c(1,1))
scatterplot(vyskaO~vyskaM, reg.line=lm, smooth=TRUE, labels=FALSE, 
  boxplots=FALSE, span=0.5, data=Kojeni)
#Korelační koeficient
cor(vyskaO,vyskaM)
#Test hypotézy nezávislosti
#Statistics-Summaries-Correlation test
cor.test(Kojeni$vyskaM, Kojeni$vyskaO, alternative="two.sided", 
  method="pearson")
#Jaký je závěr?

#3.2.Testujte, zda je významná závislost mezi hmotností a prorodní hmotností.

#3.3. Testujte, zda je významná závislost mezi por. hmotností dítěte a 
#výškou matky.

#3.4. Testujte, zda je významná závislost mezi hmotností ve 24 týdnech a
#výškou matky.

#3.5.Souvisí věk matky s věkem otce?
#Obrázek
#Jsou splněny předpoklady testu pomocí Pearsonova korelačního koeficientu?
#Spearmanův korelační koeficient
cor.test(Kojeni$vekM, Kojeni$vekO, alternative="two.sided", 
  method="spearman")
cor.test(Kojeni$vekM, Kojeni$vekO, alternative="two.sided", 
  method="pearson")
#Závěr?

#3.6.Testujte, zda je významná závislost mezi délkou dítěte ve 24 a týdnech
# a jeho hmotností.

#Nahrajte si data Stulong_aov
#Vyberte prvních 15 pozorování z dat Stulong:
Stu<-Stulong[1:15,]
#3.7.Zkoumejte závislost hladiny cholesterolu a druhého měření krevního tlaku
#pomocí Pearsonova i Spearmanova korelačního koeficientu v datech Stu

#3.8.Zkoumejte závislost hladiny cholesterolu a věku.
