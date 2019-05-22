library(primre)
library(batchtools)


res.rename <- function(d){
  d[features == "all" & algorithm == "bagging", algorithm := "B.all"]
  d[features == "some" & algorithm == "bagging", algorithm := "B"]
  d[pasting == "TRUE" & algorithm == "normal", algorithm := "O.p"]
  d[pasting == "FALSE" & algorithm == "normal", algorithm := "O"]
  d[algorithm == "rfprim", algorithm := "RF.p"]
  d[dgp == "10", dgp := "102"]
  d
}

#### AUC

get.auc <- function(reg){
  
  reduce.auc1 <- function(res) qual.auc(res[[1]])
  results = unwrap(reduceResultsDataTable(fun = reduce.auc1))
  pars = unwrap(getJobPars())
  auc1 = ijoin(pars, results)

  ids = findExperiments(algo.name = "rfprim")
  reduce.auc2 <- function(res) qual.auc(res[[2]])
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce.auc2))
  pars = unwrap(getJobPars())
  auc2 = ijoin(pars, results)
  auc2$algorithm <- "RF.l"

  auc <- rbindlist(list(auc1, auc2))
  auc <- res.rename(auc)
  auc
}

#### density

get.dens <- function(reg){
  
  reduce.dens1 <- function(res) res[[1]][nrow(res[[1]]), 2]
  results = unwrap(reduceResultsDataTable(fun = reduce.dens1))
  pars = unwrap(getJobPars())
  dens1 = ijoin(pars, results)
  
  ids = findExperiments(algo.name = "rfprim")
  reduce.dens2 <- function(res) res[[2]][nrow(res[[2]]), 2]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce.dens2))
  pars = unwrap(getJobPars())
  dens2 = ijoin(pars, results)
  dens2$algorithm <- "RF.l"
  
  dens <- rbindlist(list(dens1, dens2))
  dens <- res.rename(dens)
  dens
}

#### interpretability

get.interp <- function(reg){
  
  reduce.interp1 <- function(res){
    b <- res[[3]][[length(res[[3]])]]
    dim <- ncol(res[[3]][[1]])
    box <- matrix(c(rep(0, dim), rep(1, dim)), ncol = dim, byrow = TRUE)
    qual.interpretability(b, box)
  }
  
  reduce.interp2 <- function(res){
    b <- res[[4]][[length(res[[4]])]]
    dim <- ncol(res[[4]][[1]])
    box <- matrix(c(rep(0, dim), rep(1, dim)), ncol = dim, byrow = TRUE)
    qual.interpretability(b, box)
  }
  
  reduce.interp3 <- function(res){
    b <- res[[3]][[length(res[[3]])]]
    box <- matrix(c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1, 0.05, 0.05, 0.05, 0.05,
                    5, 5, 5, 5, 4, 4, 4, 4, 1, 1, 1, 1), nrow = 2, byrow = TRUE)
    qual.interpretability(b, box)
  }
  
  reduce.interp4 <- function(res){
    b <- res[[4]][[length(res[[4]])]]
    box <- matrix(c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1, 0.05, 0.05, 0.05, 0.05,
                    5, 5, 5, 5, 4, 4, 4, 4, 1, 1, 1, 1), nrow = 2, byrow = TRUE)
    qual.interpretability(b, box)
  }
  
  pars = unwrap(getJobPars())
  
  ids <- pars[problem == "dgps", job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce.interp1))
  interp1 = ijoin(pars, results)
  
  ids <- pars[problem == "dgps" & algorithm == "rfprim", job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce.interp2))
  interp2 = ijoin(pars, results)
  interp2$algorithm <- "RF.l"
  
  ids <- pars[problem == "dsgc", job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce.interp3))
  interp3 = ijoin(pars, results)
  
  ids <- pars[problem == "dsgc" & algorithm == "rfprim", job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce.interp4))
  interp4 = ijoin(pars, results)
  interp4$algorithm <- "RF.l"
  
  interp <- rbindlist(list(interp1, interp2, interp3, interp4))
  interp <- res.rename(interp)
  interp
}

#### boxvolume

get.boxvol <- function(reg){
  
  reduce.boxvol1 <- function(res){
    b <- res[[3]][[length(res[[3]])]]
    dim <- ncol(res[[3]][[1]])
    box <- matrix(c(rep(0, dim), rep(1, dim)), ncol = dim, byrow = TRUE)
    prod(b[2, ] - b[1, ])/prod(box[2, ] - box[1, ])
  }
  
  reduce.boxvol2 <- function(res){
    b <- res[[4]][[length(res[[4]])]]
    dim <- ncol(res[[4]][[1]])
    box <- matrix(c(rep(0, dim), rep(1, dim)), ncol = dim, byrow = TRUE)
    prod(b[2, ] - b[1, ])/prod(box[2, ] - box[1, ])
  }
  
  reduce.boxvol3 <- function(res){
    b <- res[[3]][[length(res[[3]])]]
    box <- matrix(c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1, 0.05, 0.05, 0.05, 0.05,
                    5, 5, 5, 5, 4, 4, 4, 4, 1, 1, 1, 1), nrow = 2, byrow = TRUE)
    prod(b[2, ] - b[1, ])/prod(box[2, ] - box[1, ])
  }
  
  reduce.boxvol4 <- function(res){
    b <- res[[4]][[length(res[[4]])]]
    box <- matrix(c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1, 0.05, 0.05, 0.05, 0.05,
                    5, 5, 5, 5, 4, 4, 4, 4, 1, 1, 1, 1), nrow = 2, byrow = TRUE)
    prod(b[2, ] - b[1, ])/prod(box[2, ] - box[1, ])
  }
  
  pars = unwrap(getJobPars())
  
  ids <- pars[problem == "dgps", job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce.boxvol1))
  boxvol1 = ijoin(pars, results)
  
  ids <- pars[problem == "dgps" & algorithm == "rfprim", job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce.boxvol2))
  boxvol2 = ijoin(pars, results)
  boxvol2$algorithm <- "RF.l"
  
  ids <- pars[problem == "dsgc", job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce.boxvol3))
  boxvol3 = ijoin(pars, results)
  
  ids <- pars[problem == "dsgc" & algorithm == "rfprim", job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce.boxvol4))
  boxvol4 = ijoin(pars, results)
  boxvol4$algorithm <- "RF.l"
  
  boxvol <- rbindlist(list(boxvol1, boxvol2, boxvol3, boxvol4))
  boxvol <- res.rename(boxvol)
  boxvol
}

#### consistency

get.consist <- function(ids, k = 3){ 
  
  if(length(ids) != 50){stop("number of ids != 50")}
  print(ids[50])
  
  boxes <- list()
  for(i in 1:length(ids)){
    res <- loadResult(ids[i])
    boxes[[i]] <- res[[k]][[length(res[[k]])]]
  }
  
  pars = unwrap(getJobPars())
  alg <- pars[job.id == ids[1], problem]
  if(alg == "dgps"){
    dim <- ncol(boxes[[1]])
    box <- matrix(c(rep(0, dim), rep(1, dim)), ncol = dim, byrow = TRUE)
  } else{
    box <- matrix(c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1, 0.05, 0.05, 0.05, 0.05,
                    5, 5, 5, 5, 4, 4, 4, 4, 1, 1, 1, 1), nrow = 2, byrow = TRUE)
  }

  cons <- numeric()
  for(i in 1:49){
    for(j in (i+1):50){
      cons <- c(cons, consistency.v(boxes[[i]], boxes[[j]], box))
    }
  }
  mean(cons)
}

#### combine peeling trajectories of 50 experiments

get.peeling <- function(reg){
  tab <- unwrap(getJobPars())
  tab <- tab[npts == 400 & (is.na(noise) | noise == 0) & (is.na(ngen) | ngen == 100000) & problem == "dsgc",]
  tab <- res.rename(tab)
  
  tmp <- data.table(loadResult(tab[1, job.id])[[1]])
  peeling <- cbind(tmp, rep(tab[1, algorithm], nrow(tmp)))
  names(peeling) <- c("coverage", "density", "algorithm")
  
  for(i in 2:nrow(tab)){
    tmp <- data.table(loadResult(tab[i, job.id])[[1]])
    tmp <- cbind(tmp, rep(tab[i, algorithm], nrow(tmp)))
    names(tmp) <- c("coverage", "density", "algorithm")
    peeling <- rbind(peeling, tmp)
  }
  
  tab <- tab[algorithm == "RF.p",]
  tab[algorithm == "RF.p", algorithm := "RF.l"]
  
  for(i in 1:nrow(tab)){
    tmp <- data.table(loadResult(tab[i, job.id])[[2]])
    tmp <- cbind(tmp, rep(tab[i, algorithm], nrow(tmp)))
    names(tmp) <- c("coverage", "density", "algorithm")
    peeling <- rbind(peeling, tmp)
  }

  peeling
}

#### aggregate

reg <- loadRegistry(file.dir = paste0(getwd(), "/registry"), work.dir = getwd())

d <- get.auc(reg)
res.f <- d
names(res.f)[names(res.f) == "result.1"] <- "auc"
nms <- setdiff(names(res.f), c("auc"))

d <- get.dens(reg)
names(d)[names(d) == "result.1"] <- "dens"
res.f <- ijoin(d, res.f, by = nms)

d <- get.interp(reg)
names(d)[names(d) == "result.1"] <- "interp"
res.f <- ijoin(d, res.f, by = nms)

d <- get.boxvol(reg)
names(d)[names(d) == "result.1"] <- "boxvol"
res.f <- ijoin(d, res.f, by = nms)

res <- res.f[, .(m.auc = mean(auc),
             m.dens = mean(dens),
             m.boxvol = mean(boxvol),
             m.interp = mean(interp)), 
         by = setdiff(names(d), c("auc", "dens", "interp", "boxvol", "job.id", "ind"))]

d <- unwrap(getJobPars())
cons.p <- d[, .(m.consist = get.consist(job.id, k = 3)), by = setdiff(names(d), c("job.id", "ind"))]
cons.l <- d[algorithm == "rfprim", .(m.consist = get.consist(job.id, k = 4)), by = setdiff(names(d), c("job.id", "ind"))]
cons.l$algorithm <- "RF.l"
cons <- rbindlist(list(cons.p, cons.l))
cons <- res.rename(cons)
res <- ijoin(cons, res, by = nms)

#### for lowess

peeling <- get.peeling(reg)

dir.create(file.path(getwd(), "results"), showWarnings = FALSE)
save(res.f, file = paste0(getwd(),"/results/res_f.RData"))
save(res, file = paste0(getwd(),"/results/res.RData"))
save(peeling, file = paste0(getwd(),"/results/peeling.RData"))
