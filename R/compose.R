#####################################################################
## This file enlists functions used for data generation procedures ##
#####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

########################################
# 1. Definition of coupling strategyes #
########################################

# Compose identical

#' @title Compose identical choice sets
#' @description This is an internal function which creates identical choice sets configurations for give experimental design.
#' The function simplifies the size adjustment of Z depending on the population size of X.
#'
#' @param experimental_design An `experimental_design` object.
#' @param n Number of different choice situations per individual.
#' Is NULL by default, ensuring that the function will retreive the n value from the `experimental_design`.
#' @param size Population size for Z to be adjusted to.
#' @return data.frame Z dataset
#'
#' @examples
#' # Create alternatives
#' alt1 <- alternative$new()
#' alt1$add_attributes(Quality = runif(min = 0, max = 1), Price = rnorm(mean = 5))
#' alt2 <- alternative$new()
#' alt2$add_attributes(Size = runif(min = 0, max = 1), Price = rnorm(mean = 6))
#'
#' # Regroup alternatives into design
#' edesign <- experimental_design$new(alternatives = list(alt1, alt2), n = 4)
#' 
#' # Compose
#' Z <- compose_identical(edesign, size = 2)
#' 
#' @export
#' @import foreach

compose_identical <- function(experimental_design, n = NULL, size) {
  if (is.null(n)) {
    n <- experimental_design$n
    if (is.null(n)) stop("No valid 'n' passed to compose_identical")
  }
  # Generate one Z
  Z <- alternatives_gen(
    experimental_design,
    n = n
  )
  # Repeat it
  Z <- dplyr::slice(
    Z, rep(1:dplyr::n(), size)
  )
  # Output
  return(Z)
}

# Compose random

#' @title Compose random choice sets
#' @description This is an internal function which creates distinct choice sets configurations for give experimental design.
#' The function simplifies the size adjustment of Z depending on the population size of X.
#' Each individuals faces a personalised Z version.
#'
#' @param experimental_design An `experimental_design` object.
#' @param n Number of different choice situations per individual.
#' Is NULL by default, ensuring that the function will retreive the n value from the `experimental_design`.
#' @param size Population size for Z to be adjusted to.
#' @return data.frame Z dataset
#'
#' @examples
#' # Create alternatives
#' alt1 <- alternative$new()
#' alt1$add_attributes(Quality = runif(min = 0, max = 1), Price = rnorm(mean = 5))
#' alt2 <- alternative$new()
#' alt2$add_attributes(Size = runif(min = 0, max = 1), Price = rnorm(mean = 6))
#'
#' # Regroup alternatives into design
#' edesign <- experimental_design$new(alternatives = list(alt1, alt2), n = 4)
#' 
#' # Compose
#' Z <- compose_random(edesign, size = 2)
#' 
#' @export
#' @import foreach

compose_random <- function(experimental_design, n = NULL, size) {
  if (is.null(n)) {
    n <- experimental_design$n
    if (is.null(n)) stop("No valid 'n' passed to compose_random")
  }

  # Regenerate Z for every individual
  Z <- foreach(i = seq(size), .combine = "rbind") %do% {
    # Generate Z for all individuals
    Z <- alternatives_gen(
      experimental_design,
      n = n
    )
  }

  # Output
  return(Z)
}
