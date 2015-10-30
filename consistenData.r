#! /usr/bin/env Rscript

# pso alt imc atd dn ide pls pas pad nxa b2 spr fc hd1 sex mo1 mo2
dat.classes <- c("numeric", "integer", "integer", "POSIXct", "POSIXct", "numeric", "factor", "integer", "integer", "factor", "factor", "factor", "numeric", "factor", "factor", "factor", "factor")
dat <- read.csv("cleanDataSet.csv", header = TRUE, na.string = "", stringsAsFactors = TRUE, colClasses = dat.classes)

# Remove duplicate entries
dat <- dat[!duplicated(dat),]

# Remove rows with >50% of NAs
sum.nas <- function(row) {
    sum(is.na(row))
}
ind <- apply(dat, 1, sum.nas)
dat <- dat[!(ind * 2 >= length(dat)),]

# Exchange birth dates with visit dates
tmp.dn <- dat$dn
tmp.atd <- dat$atd
dat$dn <- pmin(tmp.dn, tmp.atd)
dat$atd <- pmax(tmp.dn, tmp.atd)

# Check age with dates
years.const <- 52.25
incons <- abs(dat$ide - round(difftime(dat$atd, dat$dn, unit = "weeks") / years.const, 2)) <= 1
dat$ide[!incons] <- NA

# Outlier check function
outlier.check <- function(column) {
    lowerq <- quantile(column, na.rm = TRUE)[2]
    upperq <- quantile(column, na.rm = TRUE)[4]
    iqr <- IQR(column, na.rm = TRUE)
    mild.threshold.upper <- (iqr * 1.5) + upperq
    mild.threshold.lower <- lowerq - (iqr * 1.5)

    replace(column < mild.threshold.upper & column > mild.threshold.lower, is.na(column), TRUE)
}

# Drop rows withou nxa value
dat <- dat[!is.na(dat$nxa),]

# Drop atd and dn, not needed, consistency checked
dat$atd <- NULL
dat$dn <- NULL

write.csv(dat, "consistentDataSet.csv", row.names=FALSE, na="")
