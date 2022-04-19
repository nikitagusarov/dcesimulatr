#####################################################
## This file declares some supplementary functions ##
#####################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

#########################
# 1. Defining "rsample" #
#########################

#' @title Random sampling
#' @description This function is a wrapper around the `base::sample()`.
#' For more information refer to `?base::sample()`.
#'
#' @param x A vector of one or more elements from which to choose. 
#' @param n A non-negative integer giving the number of items to choose.
#' @param replace Logical, whether the sampling should be with replacement.
#' @param prob A vector of probability weights for obtaining the elements of the vector being sampled.
#' @return vector of length `n` containing elements from `x`.
#'
#' @examples
#' x <- 1:10; n <- 5
#' rsample(x, n, replace = FALSE)
#' @export
#' @import foreach

rsample <- function(
    x, n, 
    replace = FALSE, 
    prob = NULL
) {
    # Wrap sample
    out <- sample(
        x, 
        size = n, 
        replace = replace,
        prob = prob
    )
}