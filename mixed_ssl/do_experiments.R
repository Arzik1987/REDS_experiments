
list.of.packages <- c("batchtools", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(reds)
library(batchtools)
library(data.table)

#### if you did something wrong and want to redo everything from scratch

# reg <- loadRegistry(file.dir = paste0(getwd(), "/registry"), work.dir = getwd(), writeable = TRUE)
# removeRegistry(reg = reg)

#### for the first start of this code you need to make a registry, where the results will reside

reg = makeExperimentRegistry(file.dir = paste0(getwd(), "/registry"), packages = "reds", seed = 1)

#### for subsequent uses of the code, when the registry exists

# reg <- loadRegistry(file.dir = paste0(getwd(), "/registry"), work.dir = getwd(), writeable = TRUE)

### if you want to remove experimental results, but keep the registry

# reg <- loadRegistry(file.dir = paste0(getwd(), "/registry"), work.dir = getwd(), writeable = TRUE)
# clearRegistry(reg = getDefaultRegistry())

#### parallelize

reg$cluster.functions = makeClusterFunctionsSocket()
saveRegistry()

#### data

syntdata <- function(data, job, npts, dgp, distr, ...){
  train <- get.labs.box(n.points = npts, dgp = dgp, distr = distr,...)
  test <- get.labs.box(n.points = 20000, dgp = dgp, distr = distr,...)
  dim <- ncol(train[[1]])
  box <- matrix(c(rep(0, dim), rep(1, dim)), nrow = 2, byrow = TRUE)
  list(train = train, test = test, box = box, distr = distr)
}

simdata <- function(data, job, ind, npts, ...){
  temp <- data[[1]][(npts*(ind - 1) + 1):(npts*ind),]
  train <- test <- list()

  train[[1]] <- temp[, 1:12]
  train[[2]] <- temp[, 13]
  test[[1]] <- data[[2]][, 1:12]
  test[[2]] <- data[[2]][, 13]

  box <- matrix(c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1, 0.05, 0.05, 0.05, 0.05,
                  5, 5, 5, 5, 4, 4, 4, 4, 1, 1, 1, 1), nrow = 2, byrow = TRUE)
  list(train = train, test = test, box = box, distr = "laths")
}

load(paste0(getwd(), "/dsgc.RData"))

addProblem(name = "dgps", fun = syntdata, seed = 2020)
addProblem(name = "dsgc", data = d, fun = simdata, seed = 2020)

rm(d)


#### algorithms

P = function(data, job, instance, ...) {
  res <- norm.prim(dtrain = instance$train, dtest = instance$test, 
                   box = instance$box, ...)
  res
}

Pc = function(data, job, instance, ...) {
  res <- norm.prim(dtrain = instance$train, dtest = instance$test, 
                   box = instance$box,
                   peel.alpha = c(0.03, 0.05, 0.07, 0.1, 0.13, 0.16, 0.2), ...)
  res
}

PB = function(data, job, instance, ...) {
  res <- bumping.prim(dtrain = instance$train, dtest = instance$test, 
                      box = instance$box, ...)
  res
}

PBc = function(data, job, instance, ...) {
  res <- bumping.prim(dtrain = instance$train, dtest = instance$test, 
                      box = instance$box, depth = "cv", 
                      peel.alpha = c(0.03, 0.05, 0.07, 0.1, 0.13, 0.16, 0.2), ...)
  res
}

RP = function(data, job, instance, ngen, meth, ...) {
  res <- reds.prim(dtrain = instance$train, dtest = instance$test, 
                 box = instance$box, npts = ngen, distr = instance$distr,
                 meth = meth, ...)
  res
}

RPc = function(data, job, instance, ngen, meth, ...) {
  res <- reds.prim(dtrain = instance$train, dtest = instance$test, 
                 box = instance$box, npts = ngen, distr = instance$distr,
                 meth = meth, peel.alpha = c(0.03, 0.05, 0.07, 0.1, 0.13, 0.16, 0.2), ...)
  res
}



BI = function(data, job, instance, ...) {
  res <- best.interval(dtrain = instance$train, dtest = instance$test, 
                       box = instance$box, depth = "all", ...)
  res
}

BI5 = function(data, job, instance, ...) {
  res <- best.interval(dtrain = instance$train, dtest = instance$test, 
                       box = instance$box, depth = "all", beam.size = 5, ...)
  res
}

BIc = function(data, job, instance, ...) {
  res <- best.interval(dtrain = instance$train, dtest = instance$test, 
                       box = instance$box, depth = "cv", ...)
  res
}


RBIc = function(data, job, instance, ngen, meth, ...) {
  res <- reds.bi(dtrain = instance$train, dtest = instance$test, 
               box = instance$box, npts = ngen, depth = "cv",
               distr = instance$distr, meth = meth, ...)
  res
}


addAlgorithm(name = "P", fun = P)
addAlgorithm(name = "Pc", fun = Pc)
addAlgorithm(name = "PB", fun = PB)
addAlgorithm(name = "PBc", fun = PBc)
addAlgorithm(name = "RP", fun = RP)
addAlgorithm(name = "RPc", fun = RPc)

addAlgorithm(name = "BI", fun = BI)
addAlgorithm(name = "BI5", fun = BI5)
addAlgorithm(name = "BIc", fun = BIc)
addAlgorithm(name = "RBIc", fun = RBIc)


#### check what is there

reg$problems
reg$algorithms


#### add experiments


# functions 1

pdes = list(
  dgps = CJ(npts = c(400),
            distr = c("discr"),
            dgp = c(paste0(c(1:8)), "10", "borehole", "ellipse", "hart3",
                    "hart4", "hart6sc", "ishigami", "linketal06dec",
                    "linketal06simple", "linketal06sin", "loepetal13",
                    "moon10hd", "moon10hdc1", "moon10low", "morretal06",
                    "morris", "oakoh04", "otlcircuit",
                    "piston", "soblev99", "sobol", "welchetal92",
                    "willetal06", "wingweight"))
)


ades = list(
  P = data.table(),
  Pc = data.table(),
  PB = data.table(),
  PBc = data.table(),
  RPc = CJ(ngen = 100000, meth = c("xgbTree")),
  BI = data.table(),
  BI5 = data.table(),
  BIc = data.table(),
  RBIc = CJ(ngen = 10000, meth = c("xgbTree"))
)

addExperiments(pdes, ades, repls = 25)

# functions 2

pdes = list(
  dgps = CJ(npts = c(400),
            distr = c("logitnorm"),
            dgp = c(paste0(c(1,2,4:7)), "10", "borehole", "ellipse", "hart3",
                    "hart4", "hart6sc", "ishigami", "linketal06dec",
                    "linketal06simple", "linketal06sin", "loepetal13",
                    "moon10hd", "moon10hdc1", "moon10low", "morretal06",
                    "morris", "oakoh04", "otlcircuit",
                    "piston", "soblev99", "sobol", "welchetal92",
                    "willetal06", "wingweight"))
)


ades = list(
  P = data.table(),
  Pc = data.table(),
  PB = data.table(),
  PBc = data.table(),
  RP = CJ(ngen = 100000, meth = c("xgbTree")),
  BI = data.table(),
  BI5 = data.table(),
  BIc = data.table(),
  RBIc = CJ(ngen = 10000, meth = c("xgbTree"))
)

addExperiments(pdes, ades, repls = 25)


#### check what is where

summarizeExperiments()
unwrap(getJobPars())


#### run experiments

submitJobs()
waitForJobs()

#### you may interrupt the execution at any point and then do only remain experiments
#### with the following code

# ids <- findNotDone()
# submitJobs(ids)
# waitForJobs()

