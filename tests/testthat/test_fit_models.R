context("Fit Models")

test_that("fit_models fails on invalid input", {
  pop <- matrix(c(0, 1), nrow = 10, ncol = 5)
  data("mtcars")
  X <- mtcars[,2:6]
  Y <- mtcars$mpg

  # Invalid population
  expect_error(fit_models(5, X, Y, "lm", NULL))
  expect_error(fit_models(c(1, 0, 1, 0, 1, 0), X, Y, "lm", NULL))
  expect_error(fit_models(TRUE, X, Y, "lm", NULL))

  # X and Y should have the same number of rows
  expect_error(fit_models(pop, X[1:3], Y, "lm", NULL))
  expect_error(fit_models(pop, X[1:3], X[1], "lm", NULL))

  # X and population should have same number of columns (covariates)
  expect_error(fit_models(pop, X[1:3], Y, "lm", NULL))
  expect_error(fit_models(pop[,1:4], X, Y, "lm", NULL))

  # linear_method should be one of "lm" or "glm"
  expect_error(fit_models(pop, X, Y, "lmg", NULL))
  expect_error(fit_models(pop, X, Y, "gam", NULL))

  # linear_method_args should be NULL or a list
  expect_error(fit_models(pop, X, Y, "lm", "wow!"))
  expect_error(fit_models(pop, X, Y, "lm", c("family" = "gaussian")))
  expect_error(fit_models(pop, X, Y, "lm", "gaussian"))
})

test_that("fit_models returns the correct sized output", {
  pop <- matrix(c(0, 1), nrow = 10, ncol = 5)
  data("mtcars")
  X <- mtcars[,2:6]
  Y <- mtcars$mpg

  output <- fit_models(pop, X, Y, "lm", NULL)
  expect_type(output, "list")
  expect_length(output, 10)
})
