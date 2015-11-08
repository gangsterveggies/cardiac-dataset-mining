#! /usr/bin/env Rscript

library(ggplot2)
library(GGally)

# pso alt imc ide pls pas pad nxa b2 spr fc hd1 sex mo1 mo2
dat.classes <- c("numeric", "integer", "integer", "numeric", "factor", "integer", "integer", "factor", "factor", "factor", "numeric", "factor", "factor", "factor", "factor")
dat <- read.csv("consistentDataSet.csv", header = TRUE, na.string = "", stringsAsFactors = TRUE, colClasses = dat.classes)

# Useful functions

compareNA <- function(v1, v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return (same)
}

outlier.range <- function (v, ref, tbl, tps, fac) {
    for (i in 1:length(tps)) {
        tmp.index <- compareNA(ref, tps[[i]])
        tmp.index <- tmp.index & !is.na(v)
        tmp.values <- v
        tmp.values[is.na(tmp.values)] <- 0
        v[tmp.index & (tmp.values < tbl[[i]][1] / fac | tmp.values > tbl[[i]][2] * fac)] <- NA
    }

    return (v)
}

standardize <- function(v) {
    v.mean <- mean(v, na.rm = TRUE)
    v.sd <- sd(v, na.rm = TRUE)
    v <- (v - v.mean) / v.sd
    
    return (v)
}

# Bin ide column to match pso/alt/imc/pas/pad and fc tables
dat$ide <- cut(dat$ide, c(0, 4, 10, 16, 20))


# Outliers in pso according to http://www.cdc.gov/growthcharts/percentile_data_files.htm
pso.ide.plot.outliers <- ggplot(data=dat, aes(x=ide, y=pso, group=ide)) +
    geom_boxplot() +
    xlab("Age") + ylab("Weight") +
    ggtitle("Age ~ Weight - Outliers")

pso.table <- list(c(2, 20), c(13.6, 45.6), c(24, 83.8), c(43.3, 95.7), c(2, 95.7))
pso.types <- list("(0,4]", "(4,10]", "(10,16]", "(16,20]", NA)

dat$pso <- outlier.range(dat$pso, dat$ide, pso.table, pso.types, 1.4)

pso.ide.plot <- ggplot(data=dat, aes(x=ide, y=pso, group=ide)) +
    geom_boxplot() +
    xlab("Age") + ylab("Weight") +
    ggtitle("Age ~ Weight")


# Outliers in imc according to http://www.cdc.gov/growthcharts/clinical_charts.htm
imc.ide.plot.outliers <- ggplot(data=dat, aes(x=ide, y=imc, group=ide)) +
    geom_boxplot() +
    xlab("Age") + ylab("BMI") +
    ggtitle("Age ~ BMI - Outliers")

imc.table <- list(c(13, 20), c(12, 23), c(13, 29), c(15, 34), c(12, 34))
imc.types <- list("(0,4]", "(4,10]", "(10,16]", "(16,20]", NA)

dat$imc <- outlier.range(dat$imc, dat$ide, imc.table, imc.types, 1.1)

imc.ide.plot <- ggplot(data=dat, aes(x=ide, y=imc, group=ide)) +
    geom_boxplot() +
    xlab("Age") + ylab("BMI") +
    ggtitle("Age ~ BMI")


# Outliers in fc according to http://www.rnceus.com/psvt/psvtvs.html
fc.ide.plot.outliers <- ggplot(data=dat, aes(x=ide, y=fc, group=ide)) +
    geom_boxplot() +
    xlab("Age") + ylab("Cardiac Frequency") +
    ggtitle("Age ~ Cardiac Frequency - Outliers")

fc.table <- list(c(80, 140), c(70, 120), c(55, 105), c(55, 105), c(55, 140))
fc.types <- list("(0,4]", "(4,10]", "(10,16]", "(16,20]", NA)

dat$fc <- outlier.range(dat$fc, dat$ide, fc.table, fc.types, 1.1)

fc.ide.plot <- ggplot(data=dat, aes(x=ide, y=fc, group=ide)) +
    geom_boxplot() +
    xlab("Age") + ylab("Cardiac Frequency") +
    ggtitle("Age ~ Cardiac Frequency")


# Scatter matrix of data without outliers
dat.scatter.matrix <- ggpairs(dat[sample(nrow(dat), 100),], columns = setdiff(1:15, c(8, 15)), diag = list(continuous = "density", discrete = "bar"), axisLabels = "show", color = "nxa")


# Standardize variables, chose sd instead of normalize due to different age classes
dat$pso <- standardize(dat$pso)
dat$alt <- standardize(dat$alt)
dat$imc <- standardize(dat$imc)
dat$pas <- standardize(dat$pas)
dat$pad <- standardize(dat$pad)
dat$fc <- standardize(dat$fc)


write.csv(dat, "transformedDataSet.csv", row.names=FALSE, na="")
