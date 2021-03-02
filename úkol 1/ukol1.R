#Načtení dat
this.dir <- dirname(parent.frame(2)$ofile) #přeřazení workingdir
setwd(this.dir)
load("studenti20.RData") # načtení samotných dat
attach(studenti20)

#1 Uveďte základní popisné statistiky pro velikost boty a šířku levého zápěstí (průměr, medián, kvartily, směrodatná odchylka).
Sum_zapL <- summary(zapesti.leve)
Sum_boty <- summary(bota)