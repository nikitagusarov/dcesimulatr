#####################################################################
## This file enlists functions used for data generation procedures ##
#####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

#########################################################
# 1. Coupled creation of Z and X into single data.frame #
#########################################################

#' @title Compose experiment
#' @description This function simulates the data preparation step for an experiment.
#' The function performs coupling between X (population) and Z (experimental_design) datasets.
#'
#' @param population A `population` object.
#' @param experimental_design An `experimental_desing` object.
#' @param seed The desired seed to be set before data generation.
#' No seed is set by default (`seed = NULL`).
#' @return data.frame A complete experimental dataset XZ
#'
#' @examples
#' # Generate individual profile and population
#' ind3 <- individual$new()
#' ind3$add_characteristics(Age = rnorm(mean = 50, sd = 4), Salary = runif(min = 1, max = 5))
#' drule <- decision_rule$new()
#' drule$add_noise(rnorm(), rnorm(sd = 2))
#' drule$add_formulas(Age + 2 * Quality, 1.5 * Age + Quality^2)
#' ind3$add_decision_rule(drule)
#' pop <- population$new(profiles = list(ind3), n = list(5))
#'
#' # Create alternatives and regroup them into design
#' alt1 <- alternative$new()
#' alt1$add_attributes(Quality = runif(min = 0, max = 1), Price = rnorm(mean = 5))
#' alt2 <- alternative$new()
#' alt2$add_attributes(Size = runif(min = 0, max = 1), Price = rnorm(mean = 6))
#' edesign <- experimental_design$new(alternatives = list(alt1, alt2), n = 4)
#'
#' # Compose dataset
#' XZ <- experiment_run(pop, edesign, seed = 10)
#' @export

experiment_compose <- function(population,
                               experimental_design,
                               seed = NULL) {
  # Set configuration variable
  # Number of observation per ind
  if (is.null(experimental_design$n)) {
    n <- 1
  } else {
    n <- experimental_design$n
  }
  # Number of alternatives
  j <- length(
    experimental_design$alternatives
  )
  # Whether we desire identical choice sets across ind
  identical <- experimental_design$identical

  # Generate population
  X <- population_gen(
    population,
    seed = NULL, class = NULL
  )

  # Generate alternatives
  if (identical == TRUE) {
    Z <- compose_identical(
      experimental_design,
      n = n, size = nrow(X)
    )
  } else {
    Z <- compose_distinct(
      experimental_design,
      n = n, size = nrow(X)
    )
  }

  # Bind frames
  XZ <- cbind(
    dplyr::slice(X, rep(1:dplyr::n(), each = n * j)),
    Z
  )

  # Output
  return(XZ)
}
