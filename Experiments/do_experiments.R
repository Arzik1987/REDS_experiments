#### install batchtools if necessary

list.of.packages <- c("batchtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(primre)
library(batchtools)

#### if you did something wrong and want to redo everything from scratch

# reg <- loadRegistry(file.dir = paste0(getwd(), "/registry"), work.dir = getwd(), writeable = TRUE)
# removeRegistry(reg = reg)

#### for the first start of this code you need to make a registry, where the results will reside

reg = makeExperimentRegistry(file.dir = paste0(getwd(), "/registry"), packages = "primre", seed = 1)

#### for subsequent uses of the code, when the registry exists

# reg <- loadRegistry(file.dir = paste0(getwd(), "/registry"), work.dir = getwd(), writeable = TRUE)

### if you want to remove experimental results, but keep the registry

# reg <- loadRegistry(file.dir = paste0(getwd(), "/registry"), work.dir = getwd(), writeable = TRUE)
# clearRegistry(reg = getDefaultRegistry())

#### parallelize

reg$cluster.functions = makeClusterFunctionsSocket()
saveRegistry()

#### data

syntdata <- function(data, job, npts, dgp, ...){
  train <- get.labs.box(n.points = npts, dgp = dgp,...)
  test <- get.labs.box(n.points = 10000, dgp = dgp,...)
  dim <- ncol(train[[1]]) 
  box <- matrix(c(rep(0, dim), rep(1, dim)), nrow = 2, byrow = TRUE)
  list(train = train, test = test, box = box)
}

simdata <- function(data, job, ind, noise, npts, ...){
  temp <- data[[1]][(npts*(ind - 1) + 1):(npts*ind),]
  train <- test <- list()
  
  train[[1]] <- temp[, 1:12]
  train[[2]] <- temp[, 13]
  
  if(noise > 0){
    inds <- sample(1:length(train[[2]]), floor(length(train[[2]])*noise))
    train[[2]][inds] <- 1 - train[[2]][inds]
  }
  
  test[[1]] <- data[[2]][, 1:12]
  test[[2]] <- data[[2]][, 13]
  
  box <- matrix(c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1, 0.05, 0.05, 0.05, 0.05,
                  5, 5, 5, 5, 4, 4, 4, 4, 1, 1, 1, 1), nrow = 2, byrow = TRUE)
  list(train = train, test = test, box = box)
}

load(paste0(getwd(), "/dsgc.RData"))

addProblem(name = "dgps", fun = syntdata, seed = 8632)
addProblem(name = "dsgc", data = d, fun = simdata, seed = 8632)

rm(d)


#### algorithms

conventional = function(data, job, instance, ...) {
  res <- norm.prim(dtrain = instance$train, dtest = instance$test, box = instance$box,...)
  append(res, list(NA, NA))
}

bagging = function(data, job, instance, ...) {
  res <- bagging.prim(dtrain = instance$train, dtest = instance$test, box = instance$box,...)
  append(res, list(NA, NA))
}

primrf = function(data, job, instance, ngen, ...) {
  res <- rf.prim(dtrain = instance$train, dtest = instance$test, box = instance$box, npts = ngen, ...)
  res
}


addAlgorithm(name = "normal", fun = conventional)
addAlgorithm(name = "bagging", fun = bagging)
addAlgorithm(name = "rfprim", fun = primrf)


#### check what is there

reg$problems
reg$algorithms

#### add experiments

# 1) functions

pdes = list(
  dgps = CJ(npts = c(400, 800, 1600), 
            dgp = c(paste0(c(1:8)), "10", "borehole", "ellipse", "hart3", "hart4", "hart6sc",
                    "ishigami", "linketal06dec", "linketal06simple", "linketal06sin",
                    "loepetal13", "moon10hd", "moon10hdc1", "moon10low", "morretal06",
                    "morris", "oakoh04", "otlcircuit", "piston", "soblev99",
                    "sobol", "welchetal92", "willetal06", "wingweight"))

)

ades = list(
  normal = data.table(pasting = c(FALSE, TRUE)),
  bagging = data.table(features = c("some", "all")),
  rfprim = data.table(ngen = 100000)
)

addExperiments(pdes, ades, repls = 50)

# 2) simulations

pdes = list(
  dsgc = CJ(npts = c(200, 400, 800, 1200, 1600, 2000), 
            ind = 1:50,
            noise = 0)
)

ades = list(
  normal = data.table(pasting = c(FALSE, TRUE)),
  bagging = data.table(features = c("some", "all")),
  rfprim = data.table(ngen = 100000)
)

addExperiments(pdes, ades, repls = 1)

# 3) simlations different number of points labelled with rf

pdes = list(
  dsgc = data.table(npts = 400, ind = 1:50, noise = 0)
)

ades = list(
  normal = data.table(pasting = c(FALSE, TRUE)),
  bagging = data.table(features = c("some", "all")),
  rfprim = data.table(ngen = c(200, 400, 800, 1600, 3200, 6400, 25000, 100000))
)

addExperiments(pdes, ades, repls = 1)

# 4) simlations different noise level

pdes = list(
  dsgc = CJ(npts = 400,
            ind = 1:50,
            noise = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5))
)

ades = list(
  normal = data.table(pasting = c(FALSE, TRUE)),
  bagging = data.table(features = c("some", "all")),
  rfprim = data.table(ngen = 100000)
)

addExperiments(pdes, ades, repls = 1)


#### check what is where

summarizeExperiments()
unwrap(getJobPars())

#### run experiments

submitJobs()
waitForJobs()

#### you may interrupt the excecution at any point and then do only remain experiments
#### with the following code

# ids <- findNotDone()
# submitJobs(ids)
# waitForJobs()
