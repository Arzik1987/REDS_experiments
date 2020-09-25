library(primre)

#### -1- ####

# compare our plots with those in Dalal, S. et al. 2013. 
# Improving scenario discovery using orthogonal rotations. 
# Environmental Modelling and Software. 48, (2013), 49-64.

par(mfrow=c(3,3))
dgps <- c(paste0(c(1:9)))
for(dgp in dgps){
  d <- get.labs.box(box = NULL, n.points = 1000, dgp = dgp)
  plot(d[[1]][, 1:2], col = d[[2]] + 1, main = dgp, xlab = "x1", ylab = "x2", pch = 16)
}

# dgps 1, 3, 5, 6, 7 are similar to those in the paper
# dgp 9 is useless


#### -2- ####

# calculate shares of stable examples in dgps

dgps <- c(paste0(c(1:8, 10)), "borehole", "ellipse", "hart3", "hart4", "hart6sc",
          "ishigami", "linketal06dec", "linketal06simple", "linketal06sin",
          "loepetal13", "moon10hd", "moon10hdc1", "moon10low", "morretal06",
          "morris", "oakoh04", "otlcircuit", "piston", "soblev99",
          "sobol", "welchetal92", "willetal06", "wingweight")
set.seed(8632)
avgs <- numeric()
for(dgp in dgps){
  d <- get.labs.box(box = NULL, n.points = 10000, dgp = dgp)
  avgs <- c(avgs, mean(d[[2]]))
  cat(paste0(dgp, " "))
  cat(mean(d[[2]]))
  cat("\n")
}
load(paste0(getwd(), "/dsgc.RData"))
avgs <- c(avgs, mean(d[[2]][ ,13]))
d <- cbind(c(dgps, "dsgc"), avgs)
colnames(d) <- c("function", "share")
write.csv(d, paste0(getwd(), "/plots_tables/Tab_8_shares.csv"), row.names = FALSE)
