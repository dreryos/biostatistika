#přesměrujte se do složky biostat
setwd("C:/biostat")
#nebo file-change dir
#vyzkoušejte, kde pracujete teď
getwd()

#spustte Rcommander
library(Rcmdr)

#vyvolejte skript jménem skript9po.R
file-open script file


#Načtěte data Kojení
#pokud jsou jinde, použijeme celou adresu souboru
load("C:/home/monika/přf/statistika1314/Data/Kojeni.RData")


#zjednodušte vyvolávání proměnných dat Kojení
attach(Kojeni)
#zadejte Kojeni jako aktivní data set


#1.Lineární regrese - pokračování
#1.1.Sestavte regresní model pro závislost porodní hmotnosti na výšce matky.
#Obrázek, model, ověření předpokladů
#Odhadněte, jaká je porodní hmotnost miminka matky 171 cm vysoké?
#Jak se změní odhad, pokud matka je o 10 cm menší?


#1.2.Sestavte regresní model pro závislost hmotnosti na trvání kojení
#Obrázek, model, ověření předpokladů

#1.3.Sestavte regresní model pro závislost porodní hmotnosti na porodní délce
#Obrázek, model, ověření předpokladů
#Odhadněte pomocí modelu porodní hmotnost dítěte, které měří 50 centimetrů.
#O kolik se odhadem zvýší porodní hmotnost, pokud se porodní délka zvýší o 1 cm?

#1.4.Sestavte regresní model pro závislost hmotnosti na délce
#Obrázek, model, ověření předpokladů
#Odhadněte pomocí modelu hmotnost dítěte, které měří 70 centimetrů.
#O kolik se odhadem zvýší hmotnost, pokud se délka zvýší o 1 cm?
#Bude tento model fungovat stejně dobře pro odhadování jako model pro
#porodní hmotnost?


#2.Lineární regrese s více vysvětlujícími proměnnými
#2.1.Zkoumejme závislost délky na porodní délce a výšce otce

#2.2.Zkoumejme závislost porodní hmotnosti na porodní délce a výšce matky
#Odhadněte porodní hmotnost miminka dlouhého 50 cm, jehož matka měří 171 cm. 

#2.3. Na čem by mohla záviset hmotnost miminka ve 24 týdnech? 
#Zkuste sestavit regresní model. Kolik variability se Vám podaří vysvětlit?

#2.4. Lze do lineárního modelu vložit jako vysvětlující proměnnou faktor?
#Zkuste se sestavit model pro hmotnost pomocí porodní hmotnosti a pohlaví.
#(Použijte Fit Models-Linear Model)
#Interakce? Co to znamená?

#2.5.Závisí trvání kojení na věku matky a na porodní hmotnosti?
#Mají také faktory Dudlík, Prs a Plan vliv na trvání kojení?
#Ověření předpokladů modelu?

#3.Testy dobré shody
#3.1.V naší populaci je přibližně 85% praváků, 12% leváků a 3% lidí s nevyhraněnou
#lateralitou. U 250 dyslektiků bylo zjištěno, že 197 z nich jsou praváci,
#47 leváci a 6 má nevyhraněnou lateralitu. Má tato skupina dyslektiků
#stejné rozdělení laterality jako celá populace?

(chikvadrat<-(197-250*0.85)^2/(250*0.85)+(47-250*0.12)^2/(250*0.12)+
(6-250*0.03)^2/(250*0.03))
pchisq(c(11.06), df=2, lower.tail=FALSE)

#Ověření očekávaných četností
250*0.03

#chi^2 test v R:
poz<-c(197,47,6)
pravd<-c(0.85,0.12,0.03)
chisq.test(poz,p=pravd) 

chkv<-chisq.test(poz,p=pravd)
chkv$expected


#3.2 V ČR má 45% populace krevní skupinu A, 32% krevní skupinu 0,
# 16% krevní skupinu B a 7% krevní skupinu AB.
#Ve skupině 120 dětí s onemocněním štítné žlázy mělo 58 dětí skupinu A,
# 43 dětí skupinu 0, 10 dětí skupinu B a 9 dětí AB. Mají děti
# s onemocněním štítné žlázy stejné rozdělení krevních skupin jako 
# obecná populace?  

#Jsou všechny očekávané četnosti dost velké?

#3.3. V ČR bylo v roce 2003 mezi všemi ženami 22.1% svobodných, 52.2% 
# vdaných, 11.2% rozvedených a 14.5% vdov. Náhodný výběr 150 žen z Prahy
# ukázal 59 svobodných, 58 vdaných, 19 rozvedených a 14 vdov. 
#Mají Pražanky stejné rozdělení jako celá ČR?

#Očekávané četnosti


#3.4. Testujte v datech Kojeni, že čtvrtina matek má vysokou školu a čtvrtina
#matek nemá ani maturitu.
#Summaries-Frequency dist.

