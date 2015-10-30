#! /usr/bin/env Rscript

library(ggplot2)

# pso alt imc ide pls pas pad nxa b2 spr fc hd1 sex mo1 mo2
dat.classes <- c("numeric", "integer", "integer", "numeric", "factor", "integer", "integer", "factor", "factor", "factor", "numeric", "factor", "factor", "factor", "factor")
dat <- read.csv("consistentDataSet.csv", header = TRUE, na.string = "", stringsAsFactors = TRUE, colClasses = dat.classes)

# http://emedicine.medscape.com/article/889877-overview
# Bin ide column
dat$ide <- cut(dat$ide, c(0, 4, 10, 16, 20))

# Outliers in imc according to http://www.cdc.gov/growthcharts/clinical_charts.htm
imc.ide.plot.outliers <- ggplot(data=dat, aes(x=ide, y=imc, group=ide)) +
    geom_boxplot() +
    xlab("Age") + ylab("BMI") +
    ggtitle("Age ~ BMI") +
    ylim(0, 40)

compareNA <- function(v1, v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return (same)
}

imc.table <- list(c(13, 20), c(12, 23), c(13, 29), c(15, 34), c(12, 34))
imc.types <- list("(0,4]", "(4,10]", "(10,16]", "(16,20]", NA)

for (i in 1:5) {
    imc.tmp.index <- compareNA(dat$ide, imc.types[[i]])
    imc.tmp.index <- imc.tmp.index & !is.na(dat$imc)
    imc.tmp.values <- dat$imc
    imc.tmp.values[is.na(imc.tmp.values)] <- 0
    dat$imc[imc.tmp.index & (imc.tmp.values < imc.table[[i]][1] | imc.tmp.values > imc.table[[i]][2])] <- NA
}

imc.ide.plot <- ggplot(data=dat, aes(x=ide, y=imc, group=ide)) +
    geom_boxplot() +
    xlab("Age") + ylab("BMI") +
    ggtitle("Age ~ BMI")

# Blood pressure table
#systolic.table <- matrix(c(104, 105, 103, 104, 111, 113, 114, 115, 123, 124, 123, 125, 129, 130, 136, 138), nrow = 4, ncol = 4)
#diastolic.table <- matrix(c(58, 59, 56, 58, 74, 74, 74, 75, 80, 81, 81, 82, 84, 85, 87, 87), nrow = 4, ncol = 4)
