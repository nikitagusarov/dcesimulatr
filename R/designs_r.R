####################################################################
## This file enlists functions which declare experimental designs ##
####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

###############################
# 1. Defining "random" design #
###############################

#' @title Experimental design with random specification
#' @description Create experimental design using random number generator.
#' The design configuration assumes that all alternatives' attributes are defined by random generators:
#' `declared using `r` class simulation functions.
#' Other attribute types will be ignored.
#'
#' @param experimental_design Input experimnetal design object
#' @param n The number of choice sets to be generated.
#' @return data.frame A dataset of choice sets (Z) respecting randomized Design.
#'
#' @examples
#' # Create alternatives
#' alt1 <- alternative$new()
#' alt1$add_attributes(Quality = runif(min = 0, max = 1), Price = rnorm(mean = 5))
#' alt2 <- alternative$new()
#' alt2$add_attributes(Size = runif(min = 0, max = 1), Price = rnorm(mean = 6))
#'
#' # Regroup alternatives into design
#' edesign <- experimental_design$new(alternatives = list(alt1, alt2))
#' Z <- designs_r(edesign, n = 1)
#' @export
#' @import foreach

designs_r <- function(
  experimental_design, 
  n = NULL
) {
  # Avoid warnings on undeclared variales
  CID <- AID <- NULL

  # Avoid check failure
  i <- NULL

  # Verification
  if (is.null(n)) {
    n <- experimental_design$n
    if (is.null(n)) stop("No valid 'n' provided")
  }

  # Idex attributes defined with c
  index <- laws_index(
    experimental_design,
    type = "rand"
  )

  # Verification step
  if (sum(sapply(index, length)) == 0) {
    Z <- NULL
  } else {
    # Get unique characteristics' names
    attr <- experimental_design$get_attributes(index)

    # Generate Z - Run simulation
    Z <- foreach(
      i = seq_along(experimental_design$alternatives),
      .combine = "rbind"
    ) %do% {
      # Get profile chars, laws and obs numbers
      laws <- experimental_design$alternatives[[i]]$get_laws()
      laws <- laws[index[[i]]]

      # Update adjust dimensions for laws
      for (j in seq(laws)) {
        # General case for random laws
        laws[[j]]$n <- n
      }

      # Create DF per alternative profile
      Z <- data.frame(
        lapply(laws, eval)
      )

      # Check and output
      if (all(dim(Z) == c(0, 0))) {
        Z <- data.frame(CID = rep(NA, n))
      }

      # Check
      Z <- check_attributes(Z, attr)

      # Index data
      Z <- index_z(Z, alt_id = i)

      # Exit foreach loop
      return(as.data.frame(Z))
    }

    if (!is.null(Z)) {
      # Rearrange by CID
      Z <- dplyr::arrange(
        Z, CID, AID
      )

      # Put CID and AID first
      col <- ncol(Z)
      Z <- Z[, c(col, col - 1, 1:(col - 2))]

      # Function output
      return(as.data.frame(Z))
    } else {
      return(NULL)
    }
  }
}
