# Main Select function
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

select <- function(X, Y, alphabet = c(0, 1),
                   C, P, obj_fun, fit_fun, sel_mech,
                   crossover = TRUE, mu = 0.01, max_iterations = 100,
                   linear_method = "lm", linear_method_args = NULL,
                   stop_fun = NULL, G = 1, ...) {

  # Initialize population
  pop_current <- initialize_population(alphabet, C, P)
  t = 0

  # In a loop
    # Fit models
    models <- fit_models(pop_current, X, Y, linear_method,
                         linear_method_args = linear_method_args)

    # Determine parents
    parents <- choose_parents(pop_current, models, obj_fun, fit_fun, sel_mech)

    # Produce offspring

    # Check ending conditions

}

# Scratch code
data("mtcars")
X <- mtcars[,2:5]#11]
Y <- mtcars$mpg
#select(X, Y, C = ncol(X), P = 10)
pop <- initialize_population(c(0,1), C = 4, P = 5)
