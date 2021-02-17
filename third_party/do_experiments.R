
library(reds)
library(batchtools)
library(data.table)

#### if you did something wrong and want to redo everything from scratch

# reg <- loadRegistry(file.dir = paste0(getwd(), "/registry"), work.dir = getwd(), writeable = TRUE)
# removeRegistry(reg = reg)

#### for the first start of this code you need to make a registry, where the results will reside

reg = makeExperimentRegistry(file.dir = paste0(getwd(), "/registry"), packages = "reds", 
                             source = "read_data.R", seed = 1)

#### for subsequent uses of the code, when the registry exists

# reg <- loadRegistry(file.dir = paste0(getwd(), "/registry"), work.dir = getwd(), writeable = TRUE)

### if you want to remove experimental results, but keep the registry

# reg <- loadRegistry(file.dir = paste0(getwd(), "/registry"), work.dir = getwd(), writeable = TRUE)
# clearRegistry(reg = getDefaultRegistry())

#### parallelize

reg$cluster.functions = makeClusterFunctionsSocket()
saveRegistry()


#### problems

simdata <- function(data, job, ind, ...){
  a <- list()
  set.seed(2020)
  for(i in 1:10){
    a <- c(a, caret::createFolds(data[, 10], k = 5, list = TRUE, returnTrain = FALSE))
  }

  train <- test <- list()
  
  test <- list(data[, 1:9][a[[ind]],], data[, 10][a[[ind]]])
  train <- list(data[, 1:9][-a[[ind]],], data[, 10][-a[[ind]]])

  box <- matrix(c(67, 450, 0, 80, 0.2, -0.8, 0, -0.1, 90,
                  134, 1000,	1, 100, 0.6, -0.2, 2, 0.1, 200), nrow = 2, byrow = TRUE)
  
  list(train = train, test = test, box = box)
}

simdata2 <- function(data, job, ind, ...){
  a <- list()
  set.seed(2020)
  for(i in 1:10){
    a <- c(a, caret::createFolds(data[[2]], k = 5, list = TRUE, returnTrain = FALSE))
  }
  
  train <- test <- list()
  
  test <- list(data[[1]][a[[ind]],], data[[2]][a[[ind]]])
  train <- list(data[[1]][-a[[ind]],], data[[2]][-a[[ind]]])
  
  box <- matrix(c(0.1, 0.93, 0.01, 2, 0.001,
                  0.45, 0.99, 0.05, 4.5, 0.005), nrow = 2, byrow = TRUE)
  
  list(train = train, test = test, box = box)
}


addProblem(name = "TGL", data = dTGL, fun = simdata, seed = 2020)
addProblem(name = "lake", data = dl, fun = simdata2, seed = 2020)


#### algorithms

P1 = function(data, job, instance, ...) {
  res <- norm.prim(dtrain = instance$train, dtest = instance$test, 
                   box = instance$box, peel.alpha = 0.1, ...)
  res
}

P = function(data, job, instance, ...) {
  res <- norm.prim(dtrain = instance$train, dtest = instance$test, 
                   box = instance$box, peel.alpha = 0.05, ...)
  res
}


Pc = function(data, job, instance, ...) {
  res <- norm.prim(dtrain = instance$train, dtest = instance$test, 
                   box = instance$box,
                   peel.alpha = c(0.03, 0.05, 0.07, 0.1, 0.13, 0.16, 0.2), ...)
  res
}

RP = function(data, job, instance, ngen, meth, ...) {
  res <- reds.prim(dtrain = instance$train, dtest = instance$test, 
                   box = instance$box, npts = ngen, distr = "laths",
                   meth = meth, ...)
  res
}

addAlgorithm(name = "P1", fun = P1)
addAlgorithm(name = "P", fun = P)
addAlgorithm(name = "Pc", fun = Pc)
addAlgorithm(name = "RP", fun = RP)


#### check what is there

reg$problems
reg$algorithms


#### add experiments


ades = list(
  P1 = data.table(),
  Pc = data.table(),
  RP = data.table(ngen = 100000, meth = c("xgbTree", "rf"))
)

pdes = list(
  TGL = data.table(ind = 1:50)
)

addExperiments(pdes, ades, repls = 1)


ades = list(
  P = data.table(),
  Pc = data.table(),
  RP = data.table(ngen = 100000, meth = c("xgbTree", "rf"))
)

pdes = list(
  lake = data.table(ind = 1:50)
)

addExperiments(pdes, ades, repls = 1)


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
