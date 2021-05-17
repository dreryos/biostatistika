## ---- nacteni
options(warn = 0)
options(device = function(file, width = 7, height = 7, ...) {
  cairo_pdf(tempfile(), width = width, height = height, ...)
})
knitr::opts_chunk$set(dev="cairo_pdf")

## ---- pr1
poz <- c(24, 9, 13, 22, 9)
pravd <- c(0.21, 0.15, 0.26, 0.16, 0.22)
chisq.test(poz, p = pravd)
chisq.test(poz, p = pravd)$expected

## ---- pr2
tab2 <- matrix(c(25, 19, 10, 18),2,2, byrow = T)
mcnemar.test(tab2 ,correct = F)

## ---- pr3
tab3 <- matrix(c(31, 23, 38, 11, 17, 8, 8, 10, 4), 3, 3, byrow = T)
dimnames(tab3) <- list(rows = c("1", "2", "3"), columns = c("1", "2", "3"))
tab3
test3 <- chisq.test(tab3, correct = F)
test3
test3$expected
remove(test3)
fisher.test(tab3)
remove(tab3)