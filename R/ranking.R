## Ranking 
## Usage: ranking is used for fitness function, which consists of ranks of objective functions and ignoring their values.
## package: base R
## generic function: order
## input: obj (values of objective functions)
##        P (generation population)
library(assertthat)
obj <- obj_fun(chromosomes)

callrank <- function(obj){
  AssertRanking <- function(obj = NULL){
    assert_that(is.vector(obj) && !is.null(obj))
    assert_that(is.null(obj), msg = "
                  Please enter the vector 'obj' of objective values")
  rank <- order(obj)
  return(rank)
 }
}

