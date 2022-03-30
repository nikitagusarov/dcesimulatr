####################################################################
## This file enlists functions which declare experimental designs ##
####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

#######################################
# 1. Defining "full factorail" design #
#######################################

#' @title Design matrix generation for factors
#' @description Create experimental design based on factors.
#' The design configuration assumes that all alternatives' attributes are "factors":
#' vectors declared using `c()` assignement.
#' Other attribute types will be ignored.
#'
#' @param experimental_design Input experimnetal design object
#' @param n The n is not used in current version
#' @param resample Logical.
#' Declares whether the dataset should be resampled (shuffled) in case of factorial design element presence.
#' @return data.frame A dataset of choice sets (Z) respecting FF Design.
#'
#' @examples
#' # Create alternatives
#' alt1 <- alternative$new()
#' alt1$add_attributes(Quality = c(0:1), Price = c(2, 2.5))
#' alt2 <- alternative$new()
#' alt2$add_attributes(Size = c(1:3), Price = c(1.5, 2))
#'
#' # Regroup alternatives into design
#' edesign <- experimental_design$new(alternatives = list(alt1, alt2), design = "factorial")
#' Z <- designs_f(edesign, n = 2)
#' @export
#' @import foreach
#' @importFrom tidyr expand_grid
#' @importFrom tidyr pivot_longer

designs_f <- function(experimental_design, n = NULL, resample = NULL) {
  # Avoid check failure
  CID <- i <- NULL

  # Verification
  if (is.null(n)) {
    n <- experimental_design$n
    if (is.null(n)) stop("No valid 'n' provided")
  }
  if (is.null(resample)) {
    resample <- experimental_design$resample
    if (is.null(resample)) {
      resample <- FALSE
    }
  }

  # Idex attributes defined with c
  index <- laws_index(
    experimental_design,
    type = "c"
  )

  # Z generation from laws step
  if (sum(sapply(index, length)) == 0) {
    Z <- NULL
  } else {
    # Get unique characteristics' names
    attr <- experimental_design$get_attributes(index)

    # Generate Z - Run simulation
    Z <- foreach(
      i = seq_along(experimental_design$alternatives),
      .combine = "expand_grid"
    ) %do% {
      # Get profile chars, laws and obs numbers
      laws <- experimental_design$alternatives[[i]]$get_laws()
      laws <- laws[index[[i]]]

      # Create DF per alternative profile
      Z <- do.call(
        expand.grid, laws
      )

      # Check and output
      if (all(dim(Z) == c(1, 0)) | all(dim(Z) == c(0, 0))) {
      } else {
        # Check
        Z <- check_attributes(Z, attr)

        # Add index to colnames
        colnames(Z) <- paste0(
          colnames(Z), "_", i
        )

        # Exit foreach loop
        return(Z)
      }
    }
  }

  # Z resampling part
  if (!is.null(Z)) {
    # Add possibility to get random draws from design
    if (nrow(Z) != n | resample == TRUE) {
      # Replace verification
      if (n > nrow(Z)) {
        replace <- TRUE
        message("You requested more observations than in factorial design part, setting the replace param to TRUE !")
      } else {
        replace <- experimental_design$replace
      }
      # Sample CID
      smp <- sample(nrow(Z), n, replace = replace)
      Z <- Z[smp, ]
    }

    # Index data
    Z <- index_z(Z, type = "CID")

    # Expand to long format
    Z <- pivot_longer(
      Z, -CID,
      names_to = c(".value", "AID"),
      names_pattern = "(.*)_(.*)"
    )

    # AID class correction
    Z$AID <- as.integer(Z$AID)

    # Function output
    return(as.data.frame(Z))
  } else {
    return(NULL)
  }
}
