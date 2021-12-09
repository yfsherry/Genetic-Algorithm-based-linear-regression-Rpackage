## Fitness function
## Based on P(population) and r (rank of objective functions), we derive each candidates' fitness function.
## input: P, obj
## defined function: callrank
library(assertthat)

fit_fun <- function(chromosomes, obj){
  AssertFitness <- function(chromosomes = NULL, obj = NULL){
    assert_that(is.list(chromosomes) && is.vector(obj))
    assert_that(is.null(chromosomes),msg = "
                  Please enter a list 'chromosomes'")
    assert_that(is.null(obj), msg = "
                  Please enter the vector 'obj' of objective values")
    
    obj <- obj_fun(chromosomes)
    assert_that(!is.function(callrank), msg = "
                something wrong on the ranking function!")
    r <- callrank(obj)
    fit <- 2 * r/(P * (P + 1))
    }
    return(fit)
}

