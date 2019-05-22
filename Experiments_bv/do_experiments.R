#### install batchtools if necessary

list.of.packages <- c("batchtools", "lhs", "caret", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(primre)
library(caret)
library(lhs)
library(data.table)
library(batchtools)

#### if you did something wrong and want to redo everything from scratch

# reg <- loadRegistry(file.dir = paste0(getwd(), "/registry"), work.dir = getwd(), writeable = TRUE)
# removeRegistry(reg = reg)

#### for the first start of this code you need to make a registry, where the results will reside

reg = makeExperimentRegistry(file.dir = paste0(getwd(), "/registry"), 
                             packages = c("primre", "lhs", "caret", "data.table") , seed = 1)

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
  d <- get.labs.box(n.points = npts, dgp = dgp,...)
  dim <- ncol(d[[1]]) 
  d[[1]] <- data.table(d[[1]])
  box <- matrix(c(rep(0, dim), rep(1, dim)), nrow = 2, byrow = TRUE)
  list(d = d, box = box)
}

addProblem(name = "dgps", fun = syntdata)


#### algorithms


get.means = function(data, job, instance, ...){
  
  dim <- ncol(instance$box)
  res <- rep(NA, dim*2)
  
  for(i in 1:dim){
    res[i] <- mean(instance$d[[2]][instance$d[[1]][, .SD, .SDcols = i] > 0.95])
    res[i + dim] <- mean(instance$d[[2]][instance$d[[1]][, .SD, .SDcols = i] < 0.05])
  }
  
  list(res)
}

get.bv = function(data, job, instance, ngen, ntr, hyperp, ...){
  
  dim <- ncol(instance$box)
  dp <- rfl <- rfp <- list()
  for(i in 1:2){
    rfp[[i]] <- rfl[[i]] <- matrix(ncol = dim*2, nrow = ntr)
  }

  conv <- rep(NA, dim*2)
  for(i in 1:dim){
    conv[i] <- mean(instance$d[[2]][instance$d[[1]][, .SD, .SDcols = i] > 0.95])
    conv[i + dim] <- mean(instance$d[[2]][instance$d[[1]][, .SD, .SDcols = i] < 0.05])
  }

  res.rf <- train(as.data.frame(instance$d[[1]]), as.factor(instance$d[[2]]), method = "rf",
                    trControl = trainControl(method = "none"),
                    tuneGrid = data.frame(mtry = hyperp))
  
 
  nps <- c(length(instance$d[[2]]), ngen)

  for(j in 1:2){
    for(k in 1:ntr){
      dp[[1]] <- randomLHS(nps[j], dim)
      dp[[1]] <- data.table(dp[[1]])
      
      dp[[2]] <- predict(res.rf, as.data.frame(dp[[1]]), type = "prob")[, 2]
      for(i in 1:dim){
        rfp[[j]][k, i] <- mean(dp[[2]][dp[[1]][, .SD, .SDcols = i] > 0.95])
        rfp[[j]][k, i + dim] <- mean(dp[[2]][dp[[1]][, .SD, .SDcols = i] < 0.05])
      }
      
      # dp[[2]] <- predict(res.rf, as.data.frame(dp[[1]]))
      # dp[[2]] <- as.numeric(as.character(dp[[2]]))
      dp[[2]] <- ifelse(dp[[2]]< 0.5, 0, 1)
      for(i in 1:dim){
        rfl[[j]][k, i] <- mean(dp[[2]][dp[[1]][, .SD, .SDcols = i] > 0.95])
        rfl[[j]][k, i + dim] <- mean(dp[[2]][dp[[1]][, .SD, .SDcols = i] < 0.05])
      }
    }
  }

  return(list(conv, rfp[[1]], rfl[[1]], rfp[[2]], rfl[[2]]))
}

addAlgorithm(name = "gt", fun = get.means)
addAlgorithm(name = "bv", fun = get.bv)


#### check what is there

reg$problems
reg$algorithms

#### add experiments

# 1) ground truth

pdes = list(
  dgps = data.table(
            dgp = c("linketal06simple", "morris", "3", "8"),
            npts = 1000000)
)

ades = list(
  gt = data.table()
)

addExperiments(pdes, ades, repls = 1)


#### 2) actual values

# a) "linketal06simple"

pdes = list(
  dgps = data.table( 
    dgp = "linketal06simple",
    npts = 400)
)

ades = list(
  bv = data.table(ngen = 10000,
                  hyperp = 6, 
                  ntr = 100)
)

addExperiments(pdes, ades, repls = 200)

# b) "morris"

pdes = list(
  dgps = data.table( 
    dgp = "morris",
    npts = 400)
)

ades = list(
  bv = data.table(ngen = 10000,
                  hyperp = 11, 
                  ntr = 100)
)

addExperiments(pdes, ades, repls = 200)

# c) "3"

pdes = list(
  dgps = data.table( 
    dgp = "3",
    npts = 400)
)

ades = list(
  bv = data.table(ngen = 10000,
                  hyperp = 3, 
                  ntr = 100)
)

addExperiments(pdes, ades, repls = 200)

# d) "8"

pdes = list(
  dgps = data.table( 
    dgp = "8",
    npts = 400)
)

ades = list(
  bv = data.table(ngen = 10000,
                  hyperp = 3, 
                  ntr = 100)
)

addExperiments(pdes, ades, repls = 200)


#### check what is where

summarizeExperiments()
tmp <- unwrap(getJobPars())

#### run experiments

# those, used in the body of the paper

ids <- tmp[dgp == "3", "job.id"]
submitJobs(ids)
waitForJobs()

# the rest

ids <- findNotDone()
submitJobs(ids)
waitForJobs()
