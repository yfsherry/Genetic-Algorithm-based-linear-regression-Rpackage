## Mutation
## Randomly introduce one or more alleles in loci where those alleles are not seen in  
## the corresponding loci of either parent chromosome; usually applied after breeding.
## Input: Mu (mutation probability)
##        offsprings --two crossovered offsprings resverved in pairs in a list with 
##                     two dimensions storing paired parents with row indexes)

library(assertthat)
mut_fun <- function(offsprings){
  AssertMutation <- function(offsprings = NULL){
    assert_that(is.list(offsprings))
    assert_that(length(offsprings) == 2)
    assert_that((!is.list(offsprings)) || is.null(offsprings), msg = "
                Please deliver a list 'offsprings'")
  
  C <- offsprings[[1]][1]
  Mu <- readline("Choose Mu option: 1. neighborhood of 1%\n
               2. 1/C\n
               3. 1/Psqrt(C)")
  if(Mu <- "1" || is.null(Mu)) Mu <- 0.01
  if(Mu <- "2") Mu <- 1/C
  if(Mu <- "3") Mu <- 1/C/sqrt(P)
  
  pair.num <- length(offsprings[[1]])
  for(j in pair.num){
    parent1 <- offsprings[[1]][j]
    parent2 <- offsprings[[2]][j]
    C1 <- length(parent1)
    C2 <- length(parent2)
    C <- min(C1, C2)  # recalculate in case that length of chromosomes vary
    # indicative vector for mutation at each loci, 1 denotes mutation happens, 0 denotes no mutation happens
    mu.indic <- sample(c(0,1), C, replace = T,
                       prob = c(1-Mu, Mu))  
    mu.index <- which(mu.indic == 1, arr.ind = T)
    mu.length <- length(mu.index)

    if(mu.length > 0) {
        mu.loci <- sample(1:C, mu.length, replace = T) 
        # number of loci for a single parent to mutate can be more than one
          if(parent1[mu.loci] == parent2[mu.loci]) {
            
            parent1[mu.loci] <- ifelse(parent1[mu.loci]==0, 1, 0)
            parent2[mu.loci] <- ifelse(parent2[mu.loci]==1, 0, 1)
            offsprings[[1]][j] <- parent1
            offsprings[[2]][j] <- parent2
          }
    }}
  return(offsprings)
}   
}



