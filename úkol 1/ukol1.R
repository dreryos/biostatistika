## ---- nacteni
load("C:\\Users\\marek\\Desktop\\biostatistika\\úkol 1\\studenti20.RData")
attach(studenti20)

library(Rcmdr)

## ---- pr1
mean(zapesti.leve)
median(zapesti.leve)
quantile(zapesti.leve)
sd(zapesti.leve)
mean(bota)
median(bota)
quantile(bota)
sd(bota)

## ---- pr2
cor(zapesti.leve, bota)

## ---- plot2
scatterplot(zapesti.leve~bota, regLine=TRUE, smooth=FALSE, boxplots=FALSE)

## ---- pr3
tapply(zapesti.leve, pohlavi, summary)

## ---- plot3
plotMeans(zapesti.leve, pohlavi, error.bars="se", main="", connect=FALSE)