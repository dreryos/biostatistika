## ---- nacteni
options(warn = 0)
options(device = function(file, width = 7, height = 7, ...) {
  cairo_pdf(tempfile(), width = width, height = height, ...)
})
knitr::opts_chunk$set(dev="cairo_pdf")

load("C:\\Users\\marek\\Desktop\\biostatistika\\úkol 3\\studenti20.RData")
attach(studenti20)

library(Rcmdr)

## ---- pr1
t.test(tep~pohlavi, alternative='two.sided', conf.level=.95, var.equal=FALSE)

## ---- pr2
t.test(biceps.pravy, biceps.levy, alternative='two.sided', conf.level=.95, paired=TRUE)

## ---- pr3
t.test(malicek.levy[pohlavi == "F"], alternative='two.sided', mu=75, conf.level=.95)