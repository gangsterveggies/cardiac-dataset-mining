#! /usr/bin/env Rscript

# pso alt ide pls pas pad nxa b2 spr fc hd1 sex mo1 mo2
dat.classes <- c("numeric", "numeric", "factor", "factor", "numeric", "numeric", "factor", "factor", "factor", "numeric", "factor", "factor", "factor", "factor")
dat <- read.csv("finalDataSet.csv", header = TRUE, na.string = "", stringsAsFactors = TRUE, colClasses = dat.classes)

library(ggplot2)
library(entropy)

logit <- function(x) {
    log(x / (1 - x))
}

ilogit <- function(x) {
    exp(x) / (exp(x) + 1)
}

cor.matrix <- cor(dat[sapply(dat, is.numeric)])
reg.matrix <- matrix(nrow = ncol(dat), ncol = ncol(dat))
lin.matrix <- list()
inf.matrix <- matrix(nrow = ncol(dat), ncol = ncol(dat))
length(lin.matrix) <- ncol(dat) * ncol(dat)
splot.matrix <- list()
length(splot.matrix) <- ncol(dat) * ncol(dat)

for (i in 1:ncol(dat)) {
    for (j in 1:ncol(dat)) {
        if (class(dat[[i]]) == "numeric" & class(dat[[j]]) == "numeric") {
            mod <- lm(dat[[i]] ~ dat[[j]])
            reg.matrix[i,j] <- round(mean(mod$fitted.values - dat[[j]])^2 / (sd(mod$fitted.values) * sd(dat[[j]])), 2)
            tmp.index <- (i - 1) * ncol(dat) + j
            lin.matrix[[tmp.index]] <- mod
            
            splot.matrix[[tmp.index]] <- ggplot(data = dat, mapping = aes_string(x = names(dat)[i], y = names(dat)[j], color = "nxa")) +
                geom_abline(intercept = coef(lin.matrix[[tmp.index]])[[1]], slope = coef(lin.matrix[[tmp.index]])[[2]], color = "black") +
                geom_point() +
                xlab(paste("[[", as.character(i), "]]", sep="")) + ylab(paste("[[", as.character(j), "]]", sep="")) +
                ggtitle(paste("[[", as.character(i), "]] ~ [[", as.character(j), "]]", sep=""))
        }
    }
}

for (i in 1:ncol(dat)) {
    for (j in 1:ncol(dat)) {
        useBins <- 100
        if (class(dat[[i]]) == "numeric" | class(dat[[j]]) == "numeric") {
            useBins <- 10
        }
        
        if (class(dat[[i]]) == "numeric") {
            l1 <- discretize(dat[[i]], numBins=100)
        }
        else {
            l1 <- dat[[i]]
        }

        if (class(dat[[j]]) == "numeric") {
            l2 <- discretize(dat[[j]], numBins=100)
        }
        else {
            l2 <- dat[[j]]
        }

        inf.matrix[i,j] <- mi.empirical(table(l1, l2))
    }
}
