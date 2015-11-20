#! /usr/bin/env Rscript

# pso alt ide pls pas pad nxa b2 spr fc hd1 sex mo1 mo2
dat.classes <- c("numeric", "numeric", "factor", "factor", "numeric", "numeric", "factor", "factor", "factor", "numeric", "factor", "factor", "factor", "factor")
dat <- read.csv("finalDataSet.csv", header = TRUE, na.string = "", stringsAsFactors = TRUE, colClasses = dat.classes)

cor.matrix <- cor(dat[sapply(dat, is.numeric)])
reg.matrix <- matrix(nrow = ncol(dat), ncol = ncol(dat))

for (i in 1:ncol(dat)) {
    for (j in 1:ncol(dat)) {
        if (class(dat[[i]]) == "numeric") {
            mod <- lm(dat[[i]] ~ dat[[j]])
            reg.matrix[i,j] <- round(mean((as.numeric(mod$fitted.values) - as.numeric(dat[[j]]))^2), 2)
        }
        else {
            glm(dat[[i]] ~ dat[[j]], family = binomial(logit))
            reg.matrix[i,j] <- round(mean((as.numeric(mod$fitted.values) - as.numeric(dat[[j]]))^2), 2)
        }
    }
}
