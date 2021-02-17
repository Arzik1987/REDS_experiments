
library(data.table)
library(batchtools)
source("friedman.r")

load(paste0(getwd(),"/results/res.RData"))
load(paste0(getwd(),"/dim.RData"))


#### auxiliary function to compute relative improvement

get.ratios <- function(d, np, bsln = "P"){
  d <- d[npts == np & (is.na(ngen) | ngen == 10000 | ngen == 100000),]
  
  for(i in unique(d$dgp)){
    for(j in unique(d$algorithm)){
      if(j != bsln){
        d[algorithm == j & dgp == i, m.wracc := -1 + d[algorithm == j & dgp == i, m.wracc]/d[algorithm == bsln & dgp == i, m.wracc]]
        d[algorithm == j & dgp == i, m.interp := -1 + d[algorithm == j & dgp == i, m.interp]/d[algorithm == bsln & dgp == i, m.interp]]
        d[algorithm == j & dgp == i, m.consist := -1 + d[algorithm == j & dgp == i, m.consist]/d[algorithm == bsln & dgp == i, m.consist]]
        d[algorithm == j & dgp == i, m.auc := -1 + d[algorithm == j & dgp == i, m.auc]/d[algorithm == bsln & dgp == i, m.auc]]
        d[algorithm == j & dgp == i, m.prec := -1 + d[algorithm == j & dgp == i, m.prec]/d[algorithm == bsln & dgp == i, m.prec]]
      }
    }
  }
  
  d <- d[algorithm != bsln, ]
  d
}


# dependence on M

d <- get.ratios(res[(distr == "laths" | is.na(distr)) & algorithm %in% c("Pc", "RPx"),], np = 400, bsln = "Pc")
d <- d[algorithm == "RPx", .(dgp, m.auc, m.prec, m.consist)]
d <- merge(d, d.dim, by = "dgp")

cor(d[, -"dgp"], use = "pairwise.complete.obs", method = "spearman")[4:5,]

d <- get.ratios(res[(distr == "laths" | is.na(distr)) & algorithm %in% c("BIc", "RBIcxp"),], np = 400, bsln = "BIc")
d <- d[algorithm == "RBIcxp", .(dgp, m.wracc, m.consist)]
d <- merge(d, d.dim, by = "dgp")

cor(d[, -"dgp"], use = "pairwise.complete.obs", method = "spearman")[3:4,]


#### Friedman

## discr

d <- as.data.frame(res[algorithm %in% c("BI", "BIc", "RBIcxp") 
                       & npts == 400 & (is.na(ngen) | ngen == 10000) 
                       & distr == "discr",])
d$dgp <- as.factor(d$dgp)
friedman.test.with.post.hoc(m.wracc ~ algorithm | dgp, data = d)


d <- as.data.frame(res[algorithm %in% c("P", "PB", "PBc", "Pc", "RPcxp", "RPcx") 
                       & npts == 400 & (is.na(ngen) | ngen == 100000) 
                       & distr == "discr",])
d$dgp <- as.factor(d$dgp)
friedman.test.with.post.hoc(m.auc ~ algorithm | dgp, data = d)
friedman.test.with.post.hoc(m.prec ~ algorithm | dgp, data = d)


## ssl

d <- as.data.frame(res[algorithm %in% c("BI", "BIc", "RBIcxp") 
                       & npts == 400 & (is.na(ngen) | ngen == 10000) 
                       & distr == "logitnorm",])
d$dgp <- as.factor(d$dgp)
friedman.test.with.post.hoc(m.wracc ~ algorithm | dgp, data = d)


d <- as.data.frame(res[algorithm %in% c("P", "PB", "PBc", "Pc", "RPxp", "RPx") 
                       & npts == 400 & (is.na(ngen) | ngen == 100000) 
                       & distr == "logitnorm",])
d$dgp <- as.factor(d$dgp)
friedman.test.with.post.hoc(m.auc ~ algorithm | dgp, data = d)
friedman.test.with.post.hoc(m.prec ~ algorithm | dgp, data = d)


## continuous

d <- as.data.frame(res[algorithm %in% c("BI", "BIc", "RBIcxp") 
                       & npts == 400 & (is.na(ngen) | ngen == 10000) 
                       & (distr == "laths" | is.na(distr)),])
d$dgp <- as.factor(d$dgp)
friedman.test.with.post.hoc(m.wracc ~ algorithm | dgp, data = d)


d <- as.data.frame(res[algorithm %in% c("P","PB", "PBc", "Pc", "RPx", "RPf", "RPs") 
                       & npts == 400 & (is.na(ngen) | ngen == 100000) 
                       & (distr == "laths" | is.na(distr)),])
d$dgp <- as.factor(d$dgp)
friedman.test.with.post.hoc(m.auc ~ algorithm | dgp, data = d)
friedman.test.with.post.hoc(m.prec ~ algorithm | dgp, data = d)

