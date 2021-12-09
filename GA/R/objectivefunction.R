## Fitness function: 
## input chromosomes and select the best model based on statistical criteria

## package: base R
## generic function: lm, AIC 

## Input: chromosomes (a list or vector, and its initial class of each element 
##                    should be interger, but character is tolerable)
##        EXP (Criteria function)
## Required variables: Y,X{xi:i=1,..,s}
library(assertthat)
obj_fun <- function(chromosomes, EXP, X, Y){
  AssertObjective <- function(chromosomes = NULL, EXP = NULL, X = NULL, Y = NULL,
                            extended = FALSE){
  
  assert_that((is.function(EXP)||!(!is.null(EXP))) && (is.vector(chromosomes) 
                                                       || is.list(chromosomes))
              && is.matrix(X))
  assert_that((is.null(EXP) || !is.function(EXP)),
              msg = "Default criteria: AIC\n
              You may try to enter your own criteria function\n
              please enter a function.")
  assert_that((is.null(chromosomes) && !(is.vector(chromosomes)|| 
                                           is.list(chromosomes))),
              msg = "No entered chromosomes! Or please enter vector
              or a list for 'chromosomes'")
  assert_that((is.null(X) || !is.matrix(X)),
              msg = "Please enter the dataset! Or this 
              dataset is not a matrix.")
  assert_that((is.null(Y) || !is.vector(Y)),
              msg = "No response data 'Y'! Or this 
              dataset is not a vector.")
  
  
  # mutate 'chromosomes' into organized list
  chromosomes <- lapply(chromosomes, function(x) {
    if(is.character(x)){
      x <- sapply(x, function(i){
        as.integer(unlist(strsplit(i,"")))})
      return(x)
    } else {
      return(x)}
    })
  
  P <- length(chromosomes)
  obj <- c()
  for(i in 1:P){
    
    # assign names for needed X columns to let them fit lm/glm
    Xtest <-  X[ ,which(chromosomes[[i]] == 1,arr.ind = T)]
    Xs <- paste0("Xtest",1:ncol(Xtest))
    Xtesti <- data.frame()
    
    for (k in 1:ncol(Xtest)) {
      assign(Xs[k], Xtest[ ,k])
    }
    
    Xtesti <- Xtest1
    for (i in 2:ncol(Xtest)) {
      dataX <- eval(parse(text = paste("Xtest", i, sep = "")))
      Xtesti <- cbind(Xtesti, dataX)
    }
    
    # access AIC
    formula <-  paste0("Y~",paste("Xtest",1:ncol(Xtest),sep = "",collapse = "+"))
    model <- lm(formula = formula)
    
    # allow user's criteria
    if(FUN %is% NULL){
      objadd <- AIC(mod)
    } else {
      objadd <- EXP(mod)
      assert_that(!is.numeric(objadd),
                  msg = "Entered 'EXP' doesn't returns numeric!")
    }
    obj <- c(obj, objadd)
  }
  return(obj)
  }
}
  


