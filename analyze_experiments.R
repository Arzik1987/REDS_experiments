
list.of.packages <- c("progress")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(reds)
library(batchtools)
library(data.table)
library(progress)


res.rename <- function(d){
  d[algorithm == "RP", algorithm := "REDSp"]
  d[dgp == "10", dgp := "102"]
  d[problem == "dsgc", dgp := "dsgc"]
  d
}


#### metrics except consistency

get.metrics <- function(reg, metric = "auc", pars){
  
  if(metric == "auc"){
    reduce1 <- reduce4 <- function(res) qual.auc(res[[1]])
    reduce2 <- reduce5 <- function(res) qual.auc(res[[2]])
    reduce3 <- reduce6 <- function(res) {return(-1000)}
  }
  if(metric == "alpha"){
    reduce1 <- reduce4 <- function(res) res$peel.alpha
    reduce2 <- reduce5 <- function(res) res$peel.alpha
    reduce3 <- reduce6 <- function(res) {return(-1000)}
  }
  if(metric == "precision"){
    reduce1 <- reduce4 <- function(res) res[[1]][nrow(res[[1]]), 2] - res[[1]][1, 2]
    reduce2 <- reduce5 <- function(res) res[[2]][nrow(res[[2]]), 2] - res[[2]][1, 2]
    reduce3 <- reduce6 <- function(res) {return(-1000)}
  }
  if(metric == "interpretability"){
    
    box1 <- matrix(c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1, 0.05, 0.05, 0.05, 0.05,
                    5, 5, 5, 5, 4, 4, 4, 4, 1, 1, 1, 1), nrow = 2, byrow = TRUE)
    fint <- function(d, box = NULL){
      b <- d[[length(d)]]
      if(is.null(box)){
        dim <- ncol(d[[1]])
        box <- matrix(c(rep(0, dim), rep(1, dim)), ncol = dim, byrow = TRUE)
      }
      qual.interpretability(b, box)
    } 
    
    reduce1 <- function(res) fint(res[[3]], box = box1)
    reduce2 <- function(res) fint(res[[4]], box = box1)
    reduce3 <- function(res) fint(list(res$box), box = box1)
    reduce4 <- function(res) fint(res[[3]])
    reduce5 <- function(res) fint(res[[4]])
    reduce6 <- function(res) fint(list(res$box))
  }
  if(metric == "boxvolume"){
    
    box1 <- matrix(c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1, 0.05, 0.05, 0.05, 0.05,
                     5, 5, 5, 5, 4, 4, 4, 4, 1, 1, 1, 1), nrow = 2, byrow = TRUE)
    fbv <- function(d, box = NULL){
      b <- d[[length(d)]]
      if(is.null(box)){
        dim <- ncol(d[[1]])
        box <- matrix(c(rep(0, dim), rep(1, dim)), ncol = dim, byrow = TRUE)
      }
      prod(b[2, ] - b[1, ])/prod(box[2, ] - box[1, ])
    } 
    
    reduce1 <- function(res) fbv(res[[3]], box = box1)
    reduce2 <- function(res) fbv(res[[4]], box = box1)
    reduce3 <- function(res) fbv(list(res$box), box = box1)
    reduce4 <- function(res) fbv(res[[3]])
    reduce5 <- function(res) fbv(res[[4]])
    reduce6 <- function(res) fbv(list(res$box))
  }
  if(metric == "depth"){
    reduce1 <- reduce4 <- reduce2 <- reduce5 <- function(res) {return(-1000)}
    reduce3 <- reduce6 <- function(res) res$depth
  }
  if(metric == "wracc"){
    reduce1 <- reduce4 <- reduce2 <- reduce5 <- function(res) {return(-1000)}
    reduce3 <- reduce6 <- function(res) res$qtest
  }
  if(metric == "wracct"){
    reduce1 <- reduce4 <- reduce2 <- reduce5 <- function(res) {return(-1000)}
    reduce3 <- reduce6 <- function(res) res$qtrain
  }
  
  # dsgc
  
  cat('\r',"1/6", '\n')
  ids <- pars[problem == "dsgc" & grepl("P", algorithm), job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce1))
  tmp1 = ijoin(pars, results)
  
  cat('\r',"2/6", '\n')
  ids <- pars[problem == "dsgc" & grepl("RP", algorithm), job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce2))
  tmp2 = ijoin(pars, results)
  tmp2$algorithm <- "REDSl"

  cat('\r',"3/6", '\n')
  ids <- pars[problem == "dsgc" & (grepl("BI", algorithm) | algorithm == "REDS"), job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce3))
  tmp3 = ijoin(pars, results)
  
  # other functions 
  
  cat('\r',"4/6", '\n')
  ids <- pars[problem != "dsgc" & grepl("P", algorithm), job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce4))
  tmp4 = ijoin(pars, results)
  
  cat('\r',"5/6", '\n')
  ids <- pars[problem != "dsgc" & grepl("RP", algorithm), job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce5))
  tmp5 = ijoin(pars, results)
  tmp5$algorithm <- "REDSl"
  
  cat('\r',"6/6", '\n')
  ids <- pars[problem != "dsgc" & (grepl("BI", algorithm) | algorithm == "REDS"), job.id]
  results = unwrap(reduceResultsDataTable(ids = ids, fun = reduce6))
  tmp6 = ijoin(pars, results)

  tmp <- rbindlist(list(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6))
  tmp <- res.rename(tmp)
  tmp
}


#### consistency

get.consist <- function(ids, k = 3, pars){ 
  
  if(!(length(ids) %in% c(50, 31))){stop("number of ids != 50 or 31")}
  # print(max(ids))
  pb$tick()
  alg <- pars[job.id == ids[1], algorithm]
  
  boxes <- list()
  for(i in 1:length(ids)){
    res <- loadResult(ids[i])
    if(grepl("BI", alg) | alg == "REDS"){
      boxes[[i]] <- res$box
    } else {
      boxes[[i]] <- res[[k]][[length(res[[k]])]]
    }
  }
  
  alg <- pars[job.id == ids[1], problem]
  if(alg == "dgps"){
    dim <- ncol(boxes[[1]])
    box <- matrix(c(rep(0, dim), rep(1, dim)), ncol = dim, byrow = TRUE)
  } else{
    box <- matrix(c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1, 0.05, 0.05, 0.05, 0.05,
                    5, 5, 5, 5, 4, 4, 4, 4, 1, 1, 1, 1), nrow = 2, byrow = TRUE)
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
  tab[problem == "dsgc", dgp := "dsgc"]
  tab <- tab[npts == 400 & (is.na(ngen) | ngen == 100000) & dgp == dgpname & grepl("P", algorithm),]
  tab <- res.rename(tab)
  
  tmp <- data.table(loadResult(tab[1, job.id])[[1]])
  peeling <- cbind(tmp, rep(tab[1, algorithm], nrow(tmp)))
  names(peeling) <- c("recall", "precision", "algorithm")
  
  for(i in 2:nrow(tab)){
    tmp <- data.table(loadResult(tab[i, job.id])[[1]])
    tmp <- cbind(tmp, rep(tab[i, algorithm], nrow(tmp)))
    names(tmp) <- c("recall", "precision", "algorithm")
    peeling <- rbind(peeling, tmp)
  }
  
  tab <- tab[algorithm == "REDSp",]
  tab[algorithm == "REDSp", algorithm := "REDSl"]
  
  for(i in 1:nrow(tab)){
    tmp <- data.table(loadResult(tab[i, job.id])[[2]])
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

print("depth")
d <- get.metrics(reg, metric = "depth", pars)
names(d)[names(d) == "result.1"] <- "depth"
res.f <- ijoin(d, res.f, by = nms)

print("wracc")
d <- get.metrics(reg, metric = "wracc", pars)
names(d)[names(d) == "result.1"] <- "wracc"
res.f <- ijoin(d, res.f, by = nms)

print("wracct")
d <- get.metrics(reg, metric = "wracct", pars)
names(d)[names(d) == "result.1"] <- "wracct"
res.f <- ijoin(d, res.f, by = nms)


res <- res.f[, .(m.auc = mean(auc),
             m.prec = mean(precision),
             m.boxvol = mean(boxvol),
             m.interp = mean(interp),
             m.depth = mean(depth),
             m.wracc = mean(wracc),
             m.wracct = mean(wracct),
             m.alpha = mean(alpha)),
         by = setdiff(nms, c("job.id", "ind"))]

d <- pars
print("consistency")
print("1/2")
total <- nrow(unique(as.data.frame(d)[, setdiff(names(d), c("job.id", "ind"))]))
pb <- progress_bar$new(total = total)
cons.p <- d[, .(m.consist = get.consist(job.id, k = 3, pars)), by = setdiff(nms, c("job.id", "ind"))]
print("2/2")
total <- nrow(unique(as.data.frame(d[algorithm == "RP",])[, setdiff(names(d), c("job.id", "ind"))]))
pb <- progress_bar$new(total = total)
cons.l <- d[algorithm == "RP", .(m.consist = get.consist(job.id, k = 4, pars)), by = setdiff(names(d), c("job.id", "ind"))]
cons.l$algorithm <- "REDSl"
cons <- rbindlist(list(cons.p, cons.l))
cons <- res.rename(cons)
res <- ijoin(cons, res, by = setdiff(nms, c("job.id", "ind")))

#### for lowess

peeling.dsgc <- get.peeling(reg, "dsgc", tab = pars)
peeling.morris <- get.peeling(reg, "morris", tab = pars)

dir.create(file.path(getwd(), "results"), showWarnings = FALSE)
save(res.f, file = paste0(getwd(),"/results/res_f.RData"))
save(res, file = paste0(getwd(),"/results/res.RData"))
save(peeling.dsgc, file = paste0(getwd(),"/results/peeling_dsgc.RData"))
save(peeling.morris, file = paste0(getwd(),"/results/peeling_morris.RData"))
