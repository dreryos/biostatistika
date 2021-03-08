## ---- test
library(tikzDevice)
knitr::opts_chunk$set(dev="tikz")
load("C:\\Users\\marek\\Desktop\\biostatistika\\ukol1\\studenti20.RData")
attach(studenti20)
plot(hudba~fsport, xlab="Počet hodin sportovních aktivit týdně",
    ylab="Hraje na hudební nástroj")