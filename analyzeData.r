#! /usr/bin/env Rscript

# pso alt ide pls pas pad nxa b2 spr fc hd1 sex mo1 mo2
dat.classes <- c("numeric", "numeric", "factor", "factor", "numeric", "numeric", "factor", "factor", "factor", "numeric", "factor", "factor", "factor", "factor")
dat <- read.csv("finalDataSet.csv", header = TRUE, na.string = "", stringsAsFactors = TRUE, colClasses = dat.classes)

library(ggplot2)
library(energy)
library(entropy)
library(party)
library(e1071)

logit <- function(x) {
    log(as.numeric(x) / (1 - as.numeric(x)))
}

ilogit <- function(x) {
    exp(as.numeric(x)) / (exp(as.numeric(x)) + 1)
}

cor.matrix <- cor(dat[sapply(dat, is.numeric)])
reg.matrix <- matrix(nrow = ncol(dat), ncol = ncol(dat))
dcor.matrix <- matrix(nrow = ncol(dat), ncol = ncol(dat))
lin.matrix <- list()
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
                xlab(names(dat)[i]) + ylab(names(dat)[j]) +
                ggtitle(paste(names(dat)[i], " ~ ", names(dat)[j], sep=""))


            smp <- sample(1:nrow(dat), 1500)
            #dcor.matrix[i,j] <- dcor(dat[[i]][smp], dat[[j]][smp])
        }
    }
}

# Multivariate
dat.pca <- prcomp(dat[sapply(dat, is.numeric)])
dat.pca.info <- data.frame(dat.pca$sd, seq(1, length(dat.pca$sd)))
names(dat.pca.info) <- c("sd", "x")
dat.pca.plot <- ggplot(data=dat.pca.info, aes(x=x, y=sd)) +
    geom_line() +
    geom_point() +
    xlab("Componentes") +
    ylab("Desvio Padrão")
dat.cmp <- data.frame(dat.pca$x)

for (i in 3:5) {
    dat.cmp[[3]] <- NULL
}
dat.cmp$nxa <- dat$nxa

dat.pca.splot <- ggplot(data=dat.cmp, aes(x=PC1, y=PC2, col=nxa)) +
    geom_point() +
    xlab("PC1") +
    ylab("PC2")

#dat.hclust <- hclust(dist(dat.cmp[sample(nrow(dat), 500),]), "med")
#plot(dat.hclust)
#rect.hclust(dat.hclust, 5)

kmax <- 15
dat.numeric <- dat[sapply(dat, is.numeric)]
kmeans.dat <- data.frame(seq(1, kmax), seq(1, kmax))
names(kmeans.dat) <- c("x", "k")
for (i in seq(1, kmax)) {
    fit <- kmeans(dat.numeric, i, nstart=3, iter.max = kmax*kmax, algorithm="Lloyd")
    kmeans.dat$k[i] <- fit$tot.withinss
    #plot(dat.numeric, col=fit$cluster)
}

kmeans.plot <- ggplot(data=kmeans.dat, aes(x=x, y=k)) +
    geom_line() +
    geom_point() +
    xlab("Clusters") +
    ylab("Distância")
#plot(kmeans.dat$x, kmeans.dat$k)
dat.kmeans <- kmeans(dat.numeric, 7, nstart=3, iter.max = 50, algorithm="Lloyd")

dat.numeric$cluster <- as.factor(dat.kmeans$cluster)
dat.kmeans.smat <- ggpairs(dat.numeric, columns = c(1,2,3,4,5), upper = list(continuous = "points", combo = "box"))

# Prediction
sp <- sample(nrow(dat), 0.8*nrow(dat))
nsp <- setdiff(1:nrow(dat), sp)
dat.train <- dat[sp,]
dat.test <- dat[nsp,]
beta <- 1

dat.ctree <- ctree(nxa ~ ide + mo1 + hd1 + pas + spr + sex + pso + b2 + pls, data=dat.train)
ctree.res <- predict(dat.ctree, dat.test)
ctree.matrix <- table(ctree.res, dat.test$nxa)
ctree.recall <- ctree.matrix[1,1] / (ctree.matrix[1,1] + ctree.matrix[1,2])
ctree.precision <- ctree.matrix[1,1] / (ctree.matrix[1,1] + ctree.matrix[2,1])
ctree.fmeasure <- (beta^2 + 1) * ctree.precision * ctree.recall / (beta^2 * ctree.precision + ctree.recall)

dat.svm <- svm(nxa ~ ide + mo1 + hd1 + pas + spr + fc, data=dat.train, class.weights=data.frame(nor=0.9, ano=0.1))
svm.res <- predict(dat.svm, dat.test)
svm.matrix <- table(svm.res, dat.test$nxa)
svm.recall <- svm.matrix[1,1] / (svm.matrix[1,1] + svm.matrix[1,2])
svm.precision <- svm.matrix[1,1] / (svm.matrix[1,1] + svm.matrix[2,1])
svm.fmeasure <- (beta^2 + 1) * svm.precision * svm.recall / (beta^2 * svm.precision + svm.recall)
