## ---- nacteni
options(warn = 0)
options(device = function(file, width = 7, height = 7, ...) {
  cairo_pdf(tempfile(), width = width, height = height, ...)
})
knitr::opts_chunk$set(dev="cairo_pdf")

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
scatterplot(zapesti.leve~bota, regLine=TRUE, smooth=FALSE, boxplots=FALSE,
    xlab="Velikost boty", ylab="Šířka levého zápěstí v mm")

## ---- pr3
numSummary(zapesti.leve, groups=pohlavi, statistics=c("mean", "sd", "quantiles"), 
    quantiles=c(0,.25,.5,.75,1))

## ---- plot3
plotMeans(zapesti.leve, pohlavi, error.bars="se", main="", connect=FALSE, 
    xlab="Pohlaví", ylab="Průměr šířky levého zápěstí v mm")

## ---- pr4
cor(zapesti.leve[pohlavi == "F"], bota[pohlavi == "F"])
cor(zapesti.leve[pohlavi == "M"], bota[pohlavi == "M"])

## ---- plot4
scatterplot(zapesti.leve~bota | pohlavi, regLine=TRUE, smooth=FALSE, 
    boxplots=FALSE, by.groups=TRUE, xlab="Velikost bot",
    ylab="Šířka levého zápěstí v mm")

## ---- pr5
fsport <- Recode(sport, '0="0"; 1:2="1-2"; 2.5:5="2,5-5"; 5.5:15="5,5-15"', 
    as.factor=TRUE)
summary(fsport)

## ---- plot5
Barplot(fsport, xlab="Počet hodin sportovních aktivit týdně", ylab="Počet studentů",
    label.bars=TRUE)

## ---- pr6
numSummary(tep, groups=fsport, statistics=c("mean", "sd", "quantiles"), 
    quantiles=c(0,.25,.5,.75,1))

## ---- plot6
plotMeans(tep, fsport, error.bars="se", main="", connect=FALSE,
    xlab="Počet hodin sportovních aktivit týdně", 
    ylab="Průměr tepové frekvence v klidu po 1 min")

## ---- pr7
table(hudba,fsport)

## ---- plot7
plot(hudba~fsport, xlab="Počet hodin sportovních aktivit týdně",
    ylab="Hraje na hudební nástroj")