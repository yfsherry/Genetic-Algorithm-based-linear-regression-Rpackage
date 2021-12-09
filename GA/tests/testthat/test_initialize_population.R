context("Initialize Population")

test_that("initialize_population fails on invalid input", {
  # Invalid alphabet
  expect_error(initialize_population(1, 2, 3))
  expect_error(initialize_population(c(123), 2, 3))

  # Invalid C
  expect_error(initialize_population(c(0, 1), "hi", 3))
  expect_error(initialize_population(c(0, 1), c(0, 1), 3))

  # Invalid P
  expect_error(initialize_population(c(0, 1), 10, "hi"))
  expect_error(initialize_population(c(0, 1), 10, c(0, 1)))
})

test_that("initialize_population returns correct sized output", {
  C1 <- 5
  P1 <- 10
  pop1 <- initialize_population(c(0, 1), C1, P1)

  expect_equal(class(pop1), "matrix")
  expect_equal(nrow(pop1), P1)
  expect_equal(ncol(pop1), C1)

  C2 <- 100
  P2 <- 20
  pop2 <- initialize_population(c(0, 1, 2, 3, 4), C2, P2)

  expect_equal(class(pop2), "matrix")
  expect_equal(nrow(pop2), P2)
  expect_equal(ncol(pop2), C2)
})

test_that("initialize_population returns all items in the alphabet", {
  A1 <- c(0, 1)
  pop1 <- initialize_population(A1, 5, 10)
  expect_equal(names(table(pop1)), as.character(A1))

  A2 <- c("a", "b", "c", "d")
  pop2 <- initialize_population(A2, 100, 20)
  expect_equal(names(table(pop2)), A2)
})
