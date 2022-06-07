# Initialize Population

initialize_population <- function(alphabet, C, P) {
  # Check inputs
  assertthat::assert_that(length(alphabet) >= 2)
  assertthat::assert_that(is.numeric(C) &&
                            length(C) == 1 &&
                            C > 0)
  assertthat::assert_that(is.numeric(P) &&
                            length(P) == 1 &&
                            P > 0)

  pop <- t(replicate(P,
                   sample(alphabet, size = C, replace = TRUE)))

  return(pop)
}
