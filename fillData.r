#! /usr/bin/env Rscript

# pso alt imc ide pls pas pad nxa b2 spr fc hd1 sex mo1 mo2
dat.classes <- c("numeric", "numeric", "numeric", "factor", "factor", "numeric", "numeric", "factor", "factor", "factor", "numeric", "factor", "factor", "factor", "factor")
dat <- read.csv("transformedDataSet.csv", header = TRUE, na.string = "", stringsAsFactors = TRUE, colClasses = dat.classes)

library(FNN)

knn.info <- get.knn(subset(dat, select = c(pso, alt), !is.na(pso) & !is.na(alt)), k=5)
