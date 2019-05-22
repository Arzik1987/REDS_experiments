library(primre)
library(batchtools)
library(ggplot2)
library(data.table)


reg <- loadRegistry(file.dir = paste0(getwd(), "/registry"), work.dir = getwd())


get.v.conv <- function(reg, dg){
  
  res <- unwrap(getJobPars())
  id <- res[algorithm == "gt" & dgp == dg, "job.id"]
  gt <- loadResult(id)[[1]]
  
  ids <- res[algorithm == "bv" & dgp == dg, "job.id"]
  
  reduce <- function(res) res[[1]]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce))
  results <- results[, -1]
  var.c <- apply(results^2, 2, mean) - apply(results, 2, mean)^2 # this version is more fair, IMO
  # var.c <- apply((apply(results, 1, function(x){x - gt}))^2, 1, mean)
  
  #### RF.p
  
  reduce <- function(res) (apply(res[[4]], 2, mean) - gt)^2
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce))
  b.rfp <- apply(results[, -1], 2, mean)

  reduce <- function(res) apply(res[[4]]^2, 2, mean) - apply(res[[4]], 2, mean)^2
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce))
  v.rfp2 <- apply(results[, -1], 2, mean)
  
  reduce <- function(res) apply(res[[2]]^2, 2, mean) - apply(res[[2]], 2, mean)^2
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce))
  v.rfp1 <- apply(results[, -1], 2, mean)
  

  #### RF.l
  
  reduce <- function(res) (apply(res[[5]], 2, mean) - gt)^2
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce))
  b.rfl <- apply(results[, -1], 2, mean)
  
  reduce <- function(res) apply(res[[5]]^2, 2, mean) - apply(res[[5]], 2, mean)^2
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce))
  v.rfl2 <- apply(results[, -1], 2, mean)
  
  reduce <- function(res) apply(res[[3]]^2, 2, mean) - apply(res[[3]], 2, mean)^2
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce))
  v.rfl1 <- apply(results[, -1], 2, mean)

  return(rbind(var.c, e.rfp1 = b.rfp + v.rfp1, e.rfp2 = b.rfp + v.rfp2, 
               e.rfl1 = b.rfl + v.rfl1, e.rfl2 = b.rfl + v.rfl2,
               b.rfp, v.rfp1, v.rfp2, b.rfl, v.rfl1, v.rfl2)*1000)
}

res.3 <- get.v.conv(reg, "3")
res.8 <- get.v.conv(reg, "8")
res.l <- get.v.conv(reg, "linketal06simple")
res.m <- get.v.conv(reg, "morris")

sum(res.3[1,] - res.3[2,] > 0) # 8/10
sum(res.3[1,] - res.3[3,] > 0) # 9/10
sum(res.3[1,] - res.3[7,] - res.3[9,]  > 0) # 10/10

sum(res.8[1,] - res.8[2,] > 0) # 6/10
sum(res.8[1,] - res.8[3,] > 0) # 7/10
sum(res.8[1,] - res.8[5,] > 0) # 7/10

sum(res.l[1,] - res.l[2,] > 0) # 12/10
sum(res.l[1,] - res.l[3,] > 0) # 16/10
sum(res.l[1,] - res.l[5,] > 0) # 16/10

sum(res.m[1,] - res.m[2,] > 0) # 36/40
sum(res.m[1,] - res.m[3,] > 0) # 36/40
sum(res.m[1,] - res.m[5,] > 0) # 0 but still works!

#### check why

get.est.raw <- function(reg, dg){
  
  res <- unwrap(getJobPars())
  id <- res[algorithm == "gt" & dgp == dg, "job.id"]
  gt <- loadResult(id)[[1]]
  
  ids <- res[algorithm == "bv" & dgp == dg, "job.id"]
  
  reduce <- function(res) res[[1]]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce))
  res.o <- results[, -1]

  #### RF.p
  res.p1 <- res.p2 <- res.l1 <- res.l2 <- data.table(matrix(ncol = ncol(res.o), nrow = 0))
  for(i in 1:nrow(ids)){
    res.p1 <- rbindlist(list(res.p1, data.table(loadResult(ids[i])[[2]])))
    res.p2 <- rbindlist(list(res.p2, data.table(loadResult(ids[i])[[4]])))
    res.l1 <- rbindlist(list(res.l1, data.table(loadResult(ids[i])[[3]])))
    res.l2 <- rbindlist(list(res.l2, data.table(loadResult(ids[i])[[5]])))
  }
  
  return(list(res.o, res.p1, res.p2, res.l1, res.l2))
}

res <- get.est.raw(reg, "morris")

rank.o <- apply(res[[1]], 1, function(x)(order(x)[1]))
sum(!(rank.o %in% c(8, 9 , 10)))/200
rank.p1 <- apply(res[[2]], 1, function(x)(order(x)[1]))
sum(!(rank.p1 %in% c(8, 9 , 10)))/20000
rank.p2 <- apply(res[[3]], 1, function(x)(order(x)[1]))
sum(!(rank.p2 %in% c(8, 9 , 10)))/20000
rank.l1 <- apply(res[[4]], 1, function(x)(order(x)[1]))
sum(!(rank.l1 %in% c(8, 9 , 10)))/20000
rank.l2 <- apply(res[[5]], 1, function(x)(order(x)[1]))
sum(!(rank.l2 %in% c(8, 9 , 10)))/20000

#### for plot

get.est <- function(reg, dg, column){
  
  res <- unwrap(getJobPars())
  id <- res[algorithm == "gt" & dgp == dg, "job.id"]
  gt <- loadResult(id)[[1]]
  
  ids <- res[algorithm == "bv" & dgp == dg, "job.id"]
  
  reduce <- function(res) res[[1]][column]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce))
  res.o <- as.numeric(as.matrix(results[, -1]))

  #### RF.p
  
  reduce <- function(res) res[[2]][, column]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce))
  res.p1 <- as.numeric(as.matrix(results[, -1]))
  
  reduce <- function(res) res[[4]][, column]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce))
  res.p2 <- as.numeric(as.matrix(results[, -1]))
  

  return(list(res.o, res.p1, res.p2))
}

res.3.3 <- get.est(reg, "3", 3)

do <- cbind(as.data.frame(res.3.3[[1]]), rep("O", length(res.3.3[[1]])))
colnames(do) <- c("mu", "method")
dp <- cbind(as.data.frame(res.3.3[[3]]), rep("AM", length(res.3.3[[3]])))
colnames(dp) <- c("mu", "method")
d <- data.table(rbind(do, dp))
bs = 9
dm <- d[,.(m.mu = mean(mu)), by = "method"]


p <- ggplot(d, aes(x = mu, fill = method)) + 
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.8) +
  geom_vline(xintercept = 0.08 + 0.002*0.92 - 0.002*0.08, 
             color = "red", linetype = "dotted") +
  scale_color_grey() +
  scale_fill_grey(labels = c(bquote( ~ hat(mu)), bquote( ~ hat(mu)^a))) +
  theme_light(base_size = bs, base_family = "LinLibertine") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"),
        legend.title=element_blank(),
        # legend.position="none",
        axis.title.x = element_blank())

ggsave(paste0(getwd(), "/hist_bv.pdf"), plot = p, device = cairo_pdf, width = 3, height = 0.9)



