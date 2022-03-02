#####################################################################
## This file enlists functions used for data generation procedures ##
#####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

########################################
# 1. Definition of coupling strategyes #
########################################

# Compose identical

#' @title
#' @description
#' @param
#' @param
#' @method
#' @examples
#' @export
#' @importFrom dplyr slice

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

#' @title
#' @description
#' @param
#' @param
#' @method
#' @examples
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
