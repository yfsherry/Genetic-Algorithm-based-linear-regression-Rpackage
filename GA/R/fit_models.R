# Fit model for each chromosome in the population
fit_models <- function(pop, X, Y, linear_method, linear_method_args) {
  # Check inputs
  assertthat::assert_that(is.matrix(pop))
  assertthat::assert_that(length(Y) == nrow(X))
  assertthat::assert_that(ncol(pop) == ncol(X))
  assertthat::assert_that(linear_method %in% c("glm", "lm"))
  assertthat::assert_that(is.null(linear_method_args) || is.list(linear_method_args))

  # Apply given linear method to each chromosome in the population
  models <- apply(pop, MARGIN = 1, function(chromosome) {
    # If no variables are selected, use the intercept
    if(length(colnames(X)[chromosome == 1]) == 0) {
      X_variables <- "1"
    } else {
      X_variables <- paste0("X$", colnames(X)[chromosome == 1],
                            collapse = "+")
    }
    formula_str <- as.formula(paste("Y", X_variables, sep = " ~ "))
    return(do.call(linear_method, c(list(formula = formula_str), linear_method_args)))
  })

  return(models)
}

# fit_models(pop, X, Y, linear_method = "lm", linear_method_args = list(qr = TRUE))
# fit_models(pop, X, Y, linear_method = "glm", linear_method_args = list(family = "Gamma"))
# models <- fit_models(pop, X, Y, linear_method = "lm")
