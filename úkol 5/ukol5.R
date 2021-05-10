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
mod1 <- lm(bota~vyska)
summary(mod1)

## ---- plot1
oldpar <- par(oma = c(0, 0, 3, 0), mfrow=c(2,2))
plot(mod1)
par(oldpar)

## ---- pr2
mod2 <- lm(bota~vyska + pohlavi)
summary(mod2)
mod3 <- lm(bota~vyska * pohlavi)
summary(mod3)

## ---- pr3
mod4 <- lm(bota~vyska + pohlavi + vaha + zapesti.prave + biceps.pravy + malicek.pravy)
summary(mod4)

mod5 <- lm(bota~vyska + pohlavi + vaha + biceps.pravy)
summary(mod5)