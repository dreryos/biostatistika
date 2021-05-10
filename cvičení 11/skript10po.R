#přesměrujte se do složky biostat
setwd("C:/biostat")
#nebo file-change dir
#vyzkoušejte, kde pracujete teď
getwd()

#spustte Rcommander
library(Rcmdr)

#vyvolejte skript jménem skript10po.R
file-open script file


#Načtěte data Kojení
#pokud jsou jinde, použijeme celou adresu souboru
load("C:/home/monika/přf/statistika1314/Data/Kojeni.RData")


#zjednodušte vyvolávání proměnných dat Kojení
attach(Kojeni)
#zadejte Kojeni jako aktivní data set


#1.Testy v kontingenčních tabulkách
#1.1. Vzdělání ženicha a nevěsty
#              Nevěsta
# Ženich    zákl   matur   vš
#zákl        24     12      3
#matur        7     24      3
#vš           3      9     15 


#Test nezávislosti
#Statistics-Cont tables-Enter and analyze...
#Chi kvadrát test
.Table <- matrix(c(24,12,3,7,24,3,3,9,15), 3, 3, byrow=TRUE)
.Test <- chisq.test(.Table, correct=FALSE)
.Test
remove(.Test)
remove(.Table)
#Očekávané četnosti?
.Table <- matrix(c(24,12,3,7,24,3,3,9,15), 3,3,byrow=TRUE)
chisq.test(.Table)$expected


#Test symetrie
#McNemarův
mcnemar.test(.Table)

#1.2.Souvisí přítomnost otce u porodu se vzděláním matky?
#Statistics-Cont tables-Two-way table
#Obrázky
plot(Otec,Vzdelani)
plot(Vzdelani,Otec)
#Ověření očekávaných četností


#1.3.Je struktura vzdělanosti matek stejná v Praze jako v okresní porodnici?
#Ověření očekávaných pravděpodobností.
#Obrázek
plot(Vzdelani,Porodnice)
plot(Porodnice, Vzdelani)

#1.4.Dávají matky s různým vzděláním dítěti dudlík se stejnou pravděpodobností?
#Co testujeme?
#Obrazek
#Ověření očekávaných četností


#2.Test symetrie (párový test na 0-1 veličiny) = McNemarův test
#2.1.Odpor k matematice
#Učitel matematiky dal na začátku studia studentům prvního ročníku
# na gymnáziu otázku, zda mají nebo nemají rádi matematiku.
#Znovu jim položil tuto otázku po maturitě. Výsledky shrnul do tabulky: 
#              Po maturitě
# 1. ročník   ano    ne   
#    ano      16     5   
#    ne       16     23

#Změnil se během studia vztah studentů k matematice?

#Testová statistika:
(5-16)^2/(5+16)
pchisq(5.76,1,lower.tail=F)
#v R:
tabulka<-matrix(c(16,5,16,23),2,2,byrow=T)
mcnemar.test(tabulka,correct=F)
mcnemar.test(tabulka,correct=T)

#2.2.Pacienti s cukrovkou typu 2 byli převedeni na nový typ inzulinu.
#1 měsíc před změnou a 1 měsíc po změně si zapisovali, zda měli 
#epizodu hypoglykémie. V tabulce jsou výsledky: 

#                Po změně
# před změnou   ano    ne   
#    ano        38     26   
#    ne         11     34

#Snížil nový typ inzulinu prst hypoglykémie?


#2.Podil sanci
#Podil sancí kojeni ve 24 tydnech podle Plan
table(Koj24,Plan)
35*22/(6*36)
log(35*22/(6*36))-(sqrt(1/35+1/22+1/6+1/36))*qnorm(0.975)
log(35*22/(6*36))+(sqrt(1/35+1/22+1/6+1/36))*qnorm(0.975)
exp(0.2556)
exp(2.2867)

#Podíl šancí kojení ve 24 týdnech podle Dudlik

#Co když použijete chi-kvadrát nebo Fisherův test

#3.Logisticka regrese
#3.1.Model pro Koj24
mod1 <- glm(Koj24 ~ vekM + vekO + Plan + Prs + Dudlik, 
  family=binomial(logit), data=Kojeni)
summary(mod1)
exp(coef(mod1))
#Nebo v Rcmdr: Statistics-Fit models-Generalized linear model

#Test celeho modelu
pchisq(14.83,df=5,lower.tail=F)

mod2 <- glm(Koj24 ~ vekM +  Plan + Prs + Dudlik, 
  family=binomial(logit), data=Kojeni)
summary(mod2)

mod3 <- glm(Koj24 ~ vekM +  Plan + Dudlik, 
  family=binomial(logit), data=Kojeni)
summary(mod3)

mod4 <- glm(Koj24 ~ vekM +  Plan, 
  family=binomial(logit), data=Kojeni)
summary(mod4)
exp(coef(mod4))

mod5 <- glm(Koj24 ~  Plan, 
  family=binomial(logit), data=Kojeni)
summary(mod5)


#interval spolehlivosti pro koeficienty modelu
confint(mod5)
#nebo v Rcmdr: Models-Confidence intervals


#3.2.Pravdepodobnost, ze dite je planovane 
mod6 <- glm(Plan ~ vekM + vekO + pocetDeti, family=binomial(logit), 
  data=Kojeni)
summary(mod6)
#Test celeho modelu
pchisq(11.3,df=2,lower.tail=F)

mod7 <- glm(Plan ~ vekM  + pocetDeti, family=binomial(logit), data=Kojeni)
summary(mod7)
exp(coef(mod7))

