
library("randtoolbox")
library("lhs")

load(paste0(getwd(), "/dsgc_labs.RData"))

#### 1) test set

n = 10000
set.seed(3652)
d <- randomLHS(n, 12)
d[, 1:4] <- d[, 1:4]*4.5 + 0.5
d[, 5:8] <- d[, 5:8]*3 + 1
d[, 9:12] <- d[, 9:12]*0.95 + 0.05

dsgc_labs[[2]] <- as.data.frame(cbind(d, dsgc_labs[[2]]))

#### 2) train set

n = 100000
d <- halton(n, 12)
d[, 1:4] <- d[, 1:4]*4.5 + 0.5
d[, 5:8] <- d[, 5:8]*3 + 1
d[, 9:12] <- d[, 9:12]*0.95 + 0.05

dsgc_labs[[1]] <- as.data.frame(cbind(d, dsgc_labs[[1]]))

d <- dsgc_labs

save(d, file = paste0(getwd(), "/dsgc.RData"))



