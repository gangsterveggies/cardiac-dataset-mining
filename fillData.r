#! /usr/bin/env Rscript

# pso alt imc ide pls pas pad nxa b2 spr fc hd1 sex mo1 mo2
dat.classes <- c("numeric", "numeric", "numeric", "factor", "factor", "numeric", "numeric", "factor", "factor", "factor", "numeric", "factor", "factor", "factor", "factor")
dat <- read.csv("transformedDataSet.csv", header = TRUE, na.string = "", stringsAsFactors = TRUE, colClasses = dat.classes)

library(FNN)

# Again remove rows with ~50% of NAs
sum.nas <- function(row) {
    sum(is.na(row))
}
ind <- apply(dat, 1, sum.nas)
dat <- dat[!(ind >= 7),]

maj.vote <- function(x) {
    which.max(table(x))[[1]]
}

pre.error <- function(real, pred) {
    sqrt(mean((as.numeric(real) - as.numeric(pred))^2, na.rm = TRUE))
}

pre.class.error <- function(real, pred) {
    sum(real != pred, na.rm = TRUE) / sum(!is.na(real))
}

# Input NAs for ide using KNN
ide.knn.index <- !is.na(dat$pso) & !is.na(dat$alt)
ide.knn.info <- get.knn(subset(dat[ide.knn.index,], select = c(pso, alt)), k=5)
ide.knn <- dat$ide[ide.knn.index]

for (i in 1:length(ide.knn)) {
    ide.knn[i] <- levels(ide.knn)[maj.vote(ide.knn[ide.knn.info$nn.index[i,]])]
}

ide.err <- pre.class.error(dat$ide[ide.knn.index], ide.knn)
dat$ide[ide.knn.index] <- levels(ide.knn)[ifelse(is.na(dat$ide[ide.knn.index]), ide.knn, dat$ide[ide.knn.index])]


# Input NAs for alt using regression (chose fc due to: low NAs, high correlation)
alt.mod <- lm(alt ~ ide + fc + sex, dat)
alt.pre <- predict(alt.mod, dat)
alt.err <- pre.error(dat$alt, alt.pre)
dat$alt <- ifelse(is.na(dat$alt), alt.pre, dat$alt)


# Input NAs for pso using regression
pso.mod <- lm(pso ~ ide + alt + sex, dat)
pso.pre <- predict(pso.mod, dat)
pso.err <- pre.error(dat$pso, pso.pre)
dat$pso <- ifelse(is.na(dat$pso), pso.pre, dat$pso)


# Input NAs for pas using KNN
pas.knn.index <- !is.na(dat$pso) & !is.na(dat$alt) & !is.na(dat$ide) & !is.na(dat$sex)
tmp <- subset(dat[pas.knn.index,], select = c(pso, alt))
tmp$ide <- as.numeric(subset(dat[pas.knn.index,], select = c(ide))$ide)
tmp$sex <- as.numeric(subset(dat[pas.knn.index,], select = c(sex))$sex)
pas.knn.info <- get.knn(tmp, k=20)
pas.knn <- dat$pas[pas.knn.index]

for (i in 1:length(pas.knn)) {
    pas.knn[i] <- mean(pas.knn[pas.knn.info$nn.index[i,]], na.rm = TRUE)
}

pas.err <- pre.error(dat$pas[pas.knn.index], pas.knn)
dat$pas[pas.knn.index] <- ifelse(is.na(dat$pas[pas.knn.index]), pas.knn, dat$pas[pas.knn.index])


# Input NAs for pad using regression
pad.mod <- lm(pad ~ ide + sex + pas, dat)
pad.pre <- predict(pad.mod, dat)
pad.err <- pre.error(dat$pad, pad.pre)
dat$pad <- ifelse(is.na(dat$pad), pad.pre, dat$pad)


# Input NAs for hd1 using KNN
hd1.knn.index <- !is.na(dat$mo1) & !is.na(dat$ide) & !is.na(dat$sex) & !is.na(dat$pas) & !is.na(dat$alt)
tmp <- subset(dat[hd1.knn.index,], select = c(pas, alt))
tmp$ide <- as.numeric(subset(dat[hd1.knn.index,], select = c(ide))$ide)
tmp$sex <- as.numeric(subset(dat[hd1.knn.index,], select = c(sex))$sex)
tmp$mo1 <- as.numeric(subset(dat[hd1.knn.index,], select = c(mo1))$mo1)
hd1.knn.info <- get.knn(tmp, k=10)
hd1.knn <- dat$hd1[hd1.knn.index]

for (i in 1:length(hd1.knn)) {
    hd1.knn[i] <- levels(hd1.knn)[maj.vote(hd1.knn[hd1.knn.info$nn.index[i,]])]
}

hd1.err <- pre.error(dat$hd1[hd1.knn.index], hd1.knn)
hd1.err <- pre.class.error(dat$hd1[hd1.knn.index], hd1.knn)
dat$hd1[hd1.knn.index] <- levels(hd1.knn)[ifelse(is.na(dat$hd1[hd1.knn.index]), hd1.knn, dat$hd1[hd1.knn.index])]


# Input NAs for mo2 using KNN
mo2.knn.index <- !is.na(dat$mo1) & !is.na(dat$ide) & !is.na(dat$sex) & !is.na(dat$pas) & !is.na(dat$hd1)
tmp <- subset(dat[mo2.knn.index,], select = c(pas))
tmp$ide <- as.numeric(subset(dat[mo2.knn.index,], select = c(ide))$ide)
tmp$sex <- as.numeric(subset(dat[mo2.knn.index,], select = c(sex))$sex)
tmp$mo1 <- as.numeric(subset(dat[mo2.knn.index,], select = c(mo1))$mo1)
tmp$hd1 <- as.numeric(subset(dat[mo2.knn.index,], select = c(hd1))$hd1)
mo2.knn.info <- get.knn(tmp, k=15)
mo2.knn <- dat$mo2[mo2.knn.index]

for (i in 1:length(mo2.knn)) {
    mo2.knn[i] <- levels(mo2.knn)[maj.vote(mo2.knn[mo2.knn.info$nn.index[i,]])]
}

mo2.err <- pre.error(dat$mo2[mo2.knn.index], mo2.knn)
mo2.err <- pre.class.error(dat$mo2[mo2.knn.index], mo2.knn)
dat$mo2[mo2.knn.index] <- levels(mo2.knn)[ifelse(is.na(dat$mo2[mo2.knn.index]), mo2.knn, dat$mo2[mo2.knn.index])]


# Drop imc column (too many NAs and depends solely on pso and alt)
dat$imc <- NULL

# Drop non complete entries
dat <- dat[complete.cases(dat),]

write.csv(dat, "finalDataSet.csv", row.names=FALSE, na="")
