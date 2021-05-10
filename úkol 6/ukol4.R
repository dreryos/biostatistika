## ---- nacteni
options(warn = 0)
options(device = function(file, width = 7, height = 7, ...) {
  cairo_pdf(tempfile(), width = width, height = height, ...)
})
knitr::opts_chunk$set(dev="cairo_pdf")

load("C:\\Users\\marek\\Desktop\\biostatistika\\úkol 4\\lymfo.RData")
attach(lymfo)

library(Rcmdr)
library(mvtnorm, pos=16)
library(survival, pos=16)
library(MASS, pos=16)
library(TH.data, pos=16)
library(multcomp, pos=16)
library(abind, pos=21)

## ---- pr1
mod1 <- aov(loglym ~ infekt, data=lymfo)
summary(mod1)
with(lymfo, numSummary(loglym, groups=infekt, statistics=c("mean", "sd")))
local({
  .Pairs <- glht(mod1, linfct = mcp(infekt = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})

## ---- pr2
plotMeans(loglym, infekt, pohl, error.bars="se", connect=TRUE, legend.pos="farright")
mod2 <- lm(loglym ~ infekt + pohl, data=lymfo, contrasts=list(infekt="contr.Sum", 
  pohl="contr.Sum"))
Anova(mod2)
Tapply(loglym ~ infekt + pohl, mean, na.action=na.omit, data=lymfo) # means
Tapply(loglym ~ infekt + pohl, sd, na.action=na.omit, data=lymfo) 
  # std. deviations
xtabs(~ infekt + pohl, data=lymfo) # counts
