
list.of.packages <- c("progress")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(reds)
library(batchtools)
library(data.table)
library(progress)


res.rename <- function(d){
  d[algorithm == "P1", algorithm := "P"]
  d[algorithm == "RP" & meth == "xgbTree", algorithm := "RPxp"]
  d[algorithm == "RP" & meth == "rf", algorithm := "RPfp"]
  d[algorithm == "RPl" & meth == "xgbTree", algorithm := "RPx"]
  d[algorithm == "RPl" & meth == "rf", algorithm := "RPf"]
  d
}


#### metrics except consistency

get.metrics <- function(reg, metric = "auc", pars){
  
  if(metric == "auc"){
    reduce1 <- function(res) qual.auc(res[[1]])
    reduce2 <- function(res) qual.auc(res[[2]])
  }
  if(metric == "alpha"){
    reduce1 <- function(res) res$peel.alpha
    reduce2 <- function(res) res$peel.alpha
  }
  if(metric == "precision"){
    reduce1 <- function(res) res[[1]][nrow(res[[1]]), 2]# - res[[1]][1, 2]
    reduce2 <- function(res) res[[2]][nrow(res[[2]]), 2]# - res[[2]][1, 2]

  }
  if(metric == "interpretability"){
    
    fint <- function(d){
      b <- d[[length(d)]]
      if(ncol(b) == 9){
        box <- matrix(c(67, 450, 0, 80, 0.2, -0.8, 0, -0.1, 90,
                        134, 1000,	1, 100, 0.6, -0.2, 2, 0.1, 200), nrow = 2, byrow = TRUE)
      } else {
        box <- matrix(c(0.1, 0.93, 0.01, 2, 0.001,
                        0.45, 0.99, 0.05, 4.5, 0.005), nrow = 2, byrow = TRUE)
      }
      qual.interpretability(b, box)
    } 
    
    reduce1 <- function(res) fint(res[[3]])
    reduce2 <- function(res) fint(res[[4]])
  }
  if(metric == "boxvolume"){
    
    fbv <- function(d){
      b <- d[[length(d)]]
      if(ncol(b) == 9){
        box <- matrix(c(67, 450, 0, 80, 0.2, -0.8, 0, -0.1, 90,
                        134, 1000,	1, 100, 0.6, -0.2, 2, 0.1, 200), nrow = 2, byrow = TRUE)
      } else {
        box <- matrix(c(0.1, 0.93, 0.01, 2, 0.001,
                        0.45, 0.99, 0.05, 4.5, 0.005), nrow = 2, byrow = TRUE)
      }
      prod(b[2, ] - b[1, ])/prod(box[2, ] - box[1, ])
    } 
    
    reduce1 <- function(res) fbv(res[[3]])
    reduce2 <- function(res) fbv(res[[4]])
  }
  

  cat('\r',"1/2", '\n')
  ids <- pars[, job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce1))
  tmp1 = ijoin(pars, results)
  
  cat('\r',"2/2", '\n')
  ids <- pars[algorithm %in% c("RP"), job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce2))
  tmp2 = ijoin(pars, results)
  tmp2$algorithm <- paste0(tmp2$algorithm, "l")

  tmp <- rbindlist(list(tmp1, tmp2))
  tmp <- res.rename(tmp)
  tmp
}


#### consistency

get.consist <- function(ids, k = 3, pars){ 
  
  if(!(length(ids) %in% c(50))){stop("number of ids != 50")}
  pb$tick()
  alg <- pars[job.id == ids[1], algorithm]
  
  boxes <- list()
  for(i in 1:length(ids)){
    res <- loadResult(ids[i])
    boxes[[i]] <- res[[k]][[length(res[[k]])]]
  }
  
  alg <- pars[job.id == ids[1], problem]
  if(ncol(boxes[[1]]) == 9){
    box <- matrix(c(67, 450, 0, 80, 0.2, -0.8, 0, -0.1, 90,
                    134, 1000,	1, 100, 0.6, -0.2, 2, 0.1, 200), nrow = 2, byrow = TRUE)
  } else {
    box <- matrix(c(0.1, 0.93, 0.01, 2, 0.001,
                    0.45, 0.99, 0.05, 4.5, 0.005), nrow = 2, byrow = TRUE)
  }
  
  n <- length(ids)
  cons <- numeric()
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      cons <- c(cons, consistency.v(boxes[[i]], boxes[[j]], box))
    }
  }
  mean(cons)
}


#### combine peeling trajectories of 50 experiments

get.peeling <- function(reg, dgpname, tab){
  tab <- tab[problem == dgpname,]
  tab <- res.rename(tab)
  
  tmp <- data.table(loadResult(tab[1, job.id])[[1]])
  tmp <- tmp[which(tmp[, 1] > 0)] # since our test data is short...
  peeling <- cbind(tmp, rep(tab[1, algorithm], nrow(tmp)))
  names(peeling) <- c("recall", "precision", "algorithm")
  
  for(i in 2:nrow(tab)){
    tmp <- data.table(loadResult(tab[i, job.id])[[1]])
    tmp <- tmp[which(tmp[, 1] > 0)]
    tmp <- cbind(tmp, rep(tab[i, algorithm], nrow(tmp)))
    names(tmp) <- c("recall", "precision", "algorithm")
    peeling <- rbind(peeling, tmp)
  }
  
  tab <- tab[algorithm %in% c("RPfp", "RPxp"),]
  tab[algorithm == "RPfp", algorithm := "RPf"]
  tab[algorithm == "RPxp", algorithm := "RPx"]
  
  for(i in 1:nrow(tab)){
    tmp <- data.table(loadResult(tab[i, job.id])[[2]])
    tmp <- tmp[which(tmp[, 1] > 0)]
    tmp <- cbind(tmp, rep(tab[i, algorithm], nrow(tmp)))
    names(tmp) <- c("recall", "precision", "algorithm")
    peeling <- rbind(peeling, tmp)
  }

  peeling
}

#### aggregate

reg <- loadRegistry(file.dir = paste0(getwd(), "/registry"), work.dir = getwd())
pars <- unwrap(getJobPars())

print("interpretability")
d <- get.metrics(reg, metric = "interpretability", pars)
names(d)[names(d) == "result.1"] <- "interp"
res.f <- d
nms <- setdiff(names(res.f), c("interp"))

print("auc")
d <- get.metrics(reg, metric = "auc", pars)
names(d)[names(d) == "result.1"] <- "auc"
res.f <- ijoin(d, res.f, by = nms)

print("precision")
d <- get.metrics(reg, metric = "precision", pars)
names(d)[names(d) == "result.1"] <- "precision"
res.f <- ijoin(d, res.f, by = nms)

print("boxvolume")
d <- get.metrics(reg, metric = "boxvolume", pars)
names(d)[names(d) == "result.1"] <- "boxvol"
res.f <- ijoin(d, res.f, by = nms)

print("alpha")
d <- get.metrics(reg, metric = "alpha", pars)
names(d)[names(d) == "result.1"] <- "alpha"
res.f <- ijoin(d, res.f, by = nms)

res <- res.f[, .(m.auc = mean(auc),
             m.prec = mean(precision),
             m.boxvol = mean(boxvol),
             m.interp = mean(interp),
             m.alpha = mean(alpha)),
         by = setdiff(nms, c("job.id", "ind"))]

d <- pars
print("consistency")
print("1/2")
total <- nrow(unique(as.data.frame(d)[, setdiff(names(d), c("job.id", "ind"))]))
pb <- progress_bar$new(total = total)
cons.p <- d[, .(m.consist = get.consist(job.id, k = 3, pars)), by = setdiff(nms, c("job.id", "ind"))]

print("2/2")
total <- nrow(unique(as.data.frame(d[algorithm %in% c("RP"),])[, setdiff(names(d), c("job.id", "ind"))]))
pb <- progress_bar$new(total = total)
cons.l <- d[algorithm %in% c("RP"), .(m.consist = get.consist(job.id, k = 4, pars)), by = setdiff(names(d), c("job.id", "ind"))]
cons.l$algorithm <- paste0(cons.l$algorithm, "l")
cons.l <- res.rename(cons.l)

cons <- rbindlist(list(cons.p, cons.l))
cons <- res.rename(cons)
res <- ijoin(cons, res, by = setdiff(nms, c("job.id", "ind")))

#### for lowess

peeling.TGL <- get.peeling(reg, "TGL", tab = pars)
peeling.lake <- get.peeling(reg, "lake", tab = pars)

dir.create(file.path(getwd(), "results"), showWarnings = FALSE)
save(res.f, file = paste0(getwd(),"/results/res_f.RData"))
save(res, file = paste0(getwd(),"/results/res.RData"))
save(peeling.TGL, file = paste0(getwd(),"/results/peeling_TGL.RData"))
save(peeling.lake, file = paste0(getwd(),"/results/peeling_lake.RData"))
