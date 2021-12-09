## Selection mechanism: process by which parents are chosen to produce offspring
## Three methods of parent selection:

## Method 1: FitnessRandom
## Select one parent with probability proportional to fitness and select the other
## parent completely at random

## Method 2: FitnessFitness
## Select each parent independent with probability proportional to fitness

## Method 3: Tournament
## Tournament selection: set of chromosomes in generation t is randomly partitioned into 
## k disjoint subsets of equal size; best individual in each group is chosen as a parent;
## additional random partitionings are carried out until sufficient parents have been 
## generated; parents are pairred for breeding.


## package：base R
## generic function: sample
## input: chromosomes, obj(objective values)

library(assertthat)

sel_mech() <- function(chromosomes, obj, fit0, METHOD = NULL) {
  AssertSelecting <-
    function(chromosomes = NULL,
             obj = NULL,
             fit0 = NULL,
             METHOD = NULL) {
      ## Global variables:
      obj <- obj_fun(chromosomes)  # Objective function values
      fit0 <- fit_fun(obj)         # Fitness function values
      P <- length(chromosomes)     # generation population
      
      assert_that(is.list(chromosomes) &&
                    is.vector(obj) && is.vector(fit0) && is.function(METHOD))
      assert_that(is.null(chromosomes), msg = "
                  Please enter a list 'chromosomes'")
      assert_that(is.null(obj),
                  msg = "
                  Please enter the vector 'obj' of objective values")
      assert_that(is.null(fit0),
                  msg = "
                  Please enter the vector 'fit0' of objective values")
      
      
FitnessRandom <- function(chromosomes, fit0){
  prob <- lapply(seq_len(P), function(i) fit0[i]/sum(fit0))
  p1 <- sample(chromosomes, P, prob = prob, replace = T) # parent 1
  P2 <- sample(chromosomes, P, replace = T) # parent 2
  parents <- list(p1, p2)
  return(parents)
}

FitnessFitness <- function(chromosomes, fit0){
  prob <- lapply(seq_len(P), function(i) fit0[i]/sum(fit0))
  p1 <- sample(chromosomes, P, replace = T) # parent 1
  P2 <- sample(chromosomes, P, replace = T) # parent 2
  parent <- list(p1, p2)
  return(parents)
}


## Input: N (number required for parents)
Tournament <- function(chromosomes, fit0){
  
  N <- readline("Number of parents required for breeding: ")
  N <- as.integer(N)
  p.select <- data.frame()  # container for selected parents and their fitness values
  chromosome_fit <- cbind(chromosomes, fit0)
  
 while(N > 0) { 
  k <- runif(1, min = 2, max = N)
  select <- data.frame()
  select <- sapply(1:k, function(i){ 
          group <- list()   # container of chromosome_fit matrix
          j <- seq_len(P)[-index]  # delete parents that has already been selected
          group[[i]] <- chromosome_fit[which(j %% k == i), ]
          group[[i]] <- group[[i]][order(group[[i]][,2]), ] 
          # rearrange "group" according to rank of fitness values
          subselect <- rbind(subselect, data.frame(group[[i]][1, ]))
          # find select parent's index from whole population
          index <- which(chromosome_fit[ ,2] == subselect[i,2], arr.ind = T)
          # filter the index for the ith group
          index <- index[which(1:length(index) %% k == i)]
        return(subselect)
    })
  p.select <- rbind(p.select, select)
  N <- N - k
 }
  
  # random pair for selected parents
  var <- sample(1:P, P)
  parent1 <- p.select[ ,1][var[1:(P/2)]]
  parent2 <- p.select[ ,1][-var[1:(P/2)]]
  parents <- list(parent1, parent2)
  return(parents)
}
  
## Method 4
## Custom function: user defined
Custom_fun <- function(chromosomes, fit0){...}

}
}



