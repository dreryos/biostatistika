#přesměrujte se do složky biostat
setwd("C:/biostat")
#nebo file-change dir
#vyzkoušejte, kde pracujete teď
getwd()

#spustte Rcommander
library(Rcmdr)

#vyvolejte skript jménem skript8.R
file-open script file


#Načtěte data Kojení
#pokud jsou jinde, použijeme celou adresu souboru
load("C:/home/monika/přf/statistika1314/Data/Kojeni.RData")


#zjednodušte vyvolávání proměnných dat Kojení
attach(Kojeni)
#zadejte Kojeni jako aktivní data set

#1.Korelace
#1.1.Souvisí výška matky s výškou otce?
#Obrázek - scatterplot
#Korelační koeficient
cor(vyskaO,vyskaM)
#Test hypotézy nezávislosti
#Statistics-Summaries-Correlation test
cor.test(Kojeni$vyskaM, Kojeni$vyskaO, alternative="two.sided", 
  method="pearson")
#Jaký je závěr?

#1.2.Testujte, zda je významná závislost mezi hmotností a porodní hmotností.

#1.3. Testujte, zda je významná závislost mezi por. hmotností dítěte a 
#výškou matky.

#1.4. Testujte, zda je významná závislost mezi hmotností ve 24 týdnech a
#výškou matky

#1.5.Souvisí věk matky s věkem otce?
#Obrázek
#Jsou splněny předpoklady testu pomocí Pearsonova korelačního koeficientu?
#Spearmanův korelační koeficient
cor.test(Kojeni$vekM, Kojeni$vekO, alternative="two.sided", 
  method="spearman")
cor.test(Kojeni$vekM, Kojeni$vekO, alternative="two.sided", 
  method="pearson")
#Závěr?

#Nahrajte si data Stulong_aov
#Vyberte prvních 15 pozorování z dat Stulong:
Stu<-Stulong[1:15,]
#1.6.Zkoumejte závislost hladiny cholesterolu a druhého měření krevního tlaku
#pomocí Pearsonova i Spearmanova korelačního koeficientu v datech Stu

#1.7.Zkoumejte závislost hladiny cholesterolu a věku.


#2.Lineární regrese (zpět na datech Kojeni)
#2.1. Zkoumejme závislost hmotnosti na porodní hmotnosti  
# pomocí lineární regrese.
#Obrázek
#Statistics-fit models-linear regression
mod1 <- lm(hmotnost~porHmotnost, data=Kojeni)
summary(mod1)
#Jaký je závěr? Je závislost hmotnosti a por. hmotnosti statisticky významná?
#Co kdybychom testovali korelaci
cor.test(porHmotnost,hmotnost)
#Jaké jsou intervaly spolehlivosti pro regresní koeficienty?
#Models-Confidence intervals
Confint(mod1, level=.95)
#Ověření předpokladů modelu
#Grafy
#Models-Graphs-Basic diagnostic plots
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(mod1)
par(oldpar)
#Testy
#Normalita reziduí
shapiro.test(mod1$residuals)
#Homoskedasticita-konstantnost rozptylu chyb
#Models-Numerical diagnostics-Breusch Pagan test
bptest(hmotnost ~ porHmotnost, varformula = ~ fitted.values(mod12), 
  studentize=TRUE, data=Kojeni)
#Součty čtverců
Anova(mod1)
#Odhadněte, jak těžké bude v půl roce miminko, jehož porodní váha
#byla 3500g

#2.2. Sestavte regresní model pro závislost délky na por. délce.
#Obrázek
#Model
#Ověření předpokladů


#2.3.Sestavte regresní model pro závislost porodní hmotnosti na výšce matky.
#Obrázek, model, ověření předpokladů
#Odhadněte, jaká je porodní hmotnost miminka matky 171 cm vysoké?
#Jak se změní odhad, pokud matka je o 10 cm menší?

#2.4.Sestavte regresní model pro závislost hmotnosti na trvání kojení
#Obrázek, model, ověření předpokladů

#2.5.Sestavte regresní model pro závislost porodní hmotnosti na porodní délce
#Obrázek, model, ověření předpokladů
#Odhadněte pomocí modelu porodní hmotnost dítěte, které měří 50 centimetrů.
#O kolik se odhadem zvýší porodní hmotnost, pokud se porodní délka zvýší o 1 cm?

#2.6.Sestavte regresní model pro závislost hmotnosti na délce
#Obrázek, model, ověření předpokladů
#Odhadněte pomocí modelu hmotnost dítěte, které měří 70 centimetrů.
#O kolik se odhadem zvýší hmotnost, pokud se délka zvýší o 1 cm?
#Bude tento model fungovat stejně dobře pro odhadování jako model pro
#porodní hmotnost?

#2.7.Sestavte regresní model pro závislost trvání kojení na věku matky
#Obrázek, model, ověření předpokladů


