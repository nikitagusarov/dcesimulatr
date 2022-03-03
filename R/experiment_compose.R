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
#' XZ <- experiment_run(population, experimental_design, seed = 10)
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
    Z <- compose_random(
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
