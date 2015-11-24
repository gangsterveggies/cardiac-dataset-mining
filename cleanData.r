#! /usr/bin/env Rscript

library(stringr) # For special string operations
library(lubridate) # Date parsing and operations

# Read raw date and rename attributes
dat <- read.csv("rawDataSet.csv", header = TRUE, na.string = "", stringsAsFactors = TRUE)
names(dat) <- c("id", "pso", "alt", "imc", "atd", "dn", "ide", "cnv", "pls", "pas", "pad", "ppa", "nxa", "b2", "spr", "fc", "hd1", "hd2", "sex", "mo1", "mo2")

# drop id column
dat$id <- NULL

# Clean pso data
dat$pso[dat$pso < 0.2] <- NA

# Clean alt data
dat$alt[dat$alt < 0.2] <- NA

# Clean imc data
dat$imc[dat$imc < 5] <- NA

# Clean atd data
dat$atd <- dmy(dat$atd)
dat$atd[dat$atd > ymd("2015-10-20")] <- NA

# Clean dn data
dat$dn <- dmy(dat$dn)
dat$dn[dat$dn > ymd("2015-10-20")] <- NA

# Clean ide data
dat$ide <- as.numeric(as.character(dat$ide))
dat$ide[dat$ide < 0 | dat$ide > 20] <- NA

# Drop cnv column (not relevant and weird formating)
dat$cnv <- NULL

# Clean pls data
pls.chr <- as.character(dat$pls)
pls.chr[grep("normais", pls.chr, ignore.case = TRUE)] <- "nor"
pls.chr[grep("amplos", pls.chr, ignore.case = TRUE)] <- "amp"
pls.chr[grep("diminu", pls.chr, ignore.case = TRUE)] <- "dim"
pls.chr[grep("outro", pls.chr, ignore.case = TRUE)] <- "out"
dat$pls <- as.factor(pls.chr)

# pas and pad data is assumed clean (from summary)

# Drop ppa column (dependent on pas and pad and lots of missing values)
dat$ppa <- NULL

# Clean up nxa data
nxa.chr <- as.character(dat$nxa)
nxa.chr[grep("^n", nxa.chr, ignore.case = TRUE)] <- "nor"
nxa.chr[grep("^a", nxa.chr, ignore.case = TRUE)] <- "ano"
dat$nxa <- as.factor(nxa.chr)

# b2 data is assumed clean (from summary)

# Clean up spr data
spr.chr <- as.character(dat$spr)
spr.chr[grep("ausente", spr.chr, fixed = TRUE)] <- "aus"
spr.chr[grep("cont", spr.chr, ignore.case = TRUE)] <- "con"
spr.chr[grep("Sistolico e diastólico", spr.chr, fixed = TRUE)] <- "sed"
spr.chr[grep("sistólico", spr.chr, fixed = TRUE)] <- "sis"
spr.chr[grep("Sistólico", spr.chr, fixed = TRUE)] <- "sis"
spr.chr[grep("diastólico", spr.chr, fixed = TRUE)] <- "dis"
dat$spr <- as.factor(spr.chr)

# Clean up fc data
fc.chr <- as.character(dat$fc)
interval.drop <- function(s) {
    if (grepl("-", s)) {
        a <- as.numeric(strsplit(s, "-")[[1]][1])
        b <- as.numeric(strsplit(s, "-")[[1]][2])
        as.character((a + b) / 2)
    } else if (grepl("a", s)) {
        a <- as.numeric(strsplit(s, "a")[[1]][1])
        b <- as.numeric(strsplit(s, "a")[[1]][2])
        as.character((a + b) / 2)
    } else {
        s
    }
}
fc.chr <- sapply(fc.chr, interval.drop)
dat$fc <- as.numeric(fc.chr)

# hd1 data is assumed clean (from summary)

# Drop hd2 column (lot of missing values and not that relevant)
dat$hd2 <- NULL

# Clean sex data
sex.chr <- as.character(dat$sex)
sex.chr[grep("^m", sex.chr, ignore.case = TRUE)] <- "mas"
sex.chr[grep("^f", sex.chr, ignore.case = TRUE)] <- "fem"
sex.chr[grep("^m|f", sex.chr, invert = TRUE)] <- NA
dat$sex <- as.factor(sex.chr)

# mo1 and mo2 data is assumed clean (from summary)

write.csv(dat, "cleanDataSet.csv", row.names=FALSE, na="")

library(ggplot2)

plot.simple <- function(var, lms) {
    ggplot(dat, aes_string(x=var, color="nxa")) +
        geom_density(alpha=.4) +
        xlab(var) +
        ylab("Frequência") +
        ggtitle(paste("histograma", var, "~ nxa")) +
        xlim(lms)
}

plot.simple.cat <- function(var) {
    ggplot(data=dat, aes_string(x=var, fill="nxa")) +
        geom_bar(stat="bin") +
        xlab(var) +
        ylab("Frequência") +
        ggtitle(paste("bar plot nxa"))
}
