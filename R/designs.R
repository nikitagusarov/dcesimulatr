####################################################################
## This file enlists functions which declare experimental designs ##
####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

###############################
# 1. Defining "random" design #
###############################

#' @title Randomized experimental design
#' @description Create Fully Randomized experimental design.
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
#' Z <- random_design(edesign, n = 1)
#' @export
#' @import foreach

random_design <- function(experimental_design, n) {
  # Avoid check failure
  i <- NULL

  # Idex attributes defined with c
  index <- laws_index(
    experimental_design,
    type = "rand"
  )

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
    } else {
      # Check
      Z <- check_attributes(Z, attr)

      # Index data
      Z <- index_z(Z, alt_id = i)

      # Exit foreach loop
      return(Z)
    }
  }

  # Function output
  return(as.data.frame(Z))
}



##################################
# 2. Defining "factorail" design #
##################################

#' @title Full Factorail experimental design
#' @description Create Full Factorail experimental design.
#' The design configuration assumes that all alternatives' attributes are "factors":
#' vectors declared using `c()` assignement.
#' Other attribute types will be ignored.
#'
#' @param experimental_design Input experimnetal design object
#' @param n The n is not used in current version
#' @param sample Logical
#' Declares whether the created design should be redused and randomly sampled.
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
#' Z <- random_design(edesign)
#' @export
#' @import foreach
#' @importFrom tidyr expand_grid
#' @importFrom tidyr pivot_longer

factorial_design <- function(experimental_design, n, sample = FALSE) {
  # Avoid check failure
  CID <- i <- NULL

  # Idex attributes defined with c
  index <- laws_index(
    experimental_design,
    type = "c"
  )

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
    if (all(dim(Z) == c(0, 0))) {
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
}





##############################
# 3. Defining "mixed" design #
##############################

#' @title Mixed experimental design
#' @description Create mixed experimental design.
#' The design configuration assumes that alternatives' attributes are mixed:
#' vectors declared using `c()` assignement and
#' `declared using `r` class simulation functions.
#'
#' @param experimental_design Input experimnetal design object
#' @param n The n is not used in current version
#' @param sample Logical.
#' Declares whether the created design should be redused and randomly sampled.
#' @return data.frame A dataset of choice sets (Z) respecting FF Design.
#'
#' @examples
#' # Create alternatives
#' alt1 <- alternative$new()
#' alt1$add_attributes(Quality = c(0:1), Price = rnorm(mean = 2, sd = 1))
#' alt2 <- alternative$new()
#' alt2$add_attributes(Size = c(1:3), Price = runif(min = 1, max = 3))
#'
#' # Regroup alternatives into design
#' edesign <- experimental_design$new(alternatives = list(alt1, alt2), design = "mixed")
#' Z <- random_design(edesign, n = 2)
#' @export

mixed_design <- function(experimental_design, n, sample = FALSE) {
  # Part 1: Factorial design step
  Zf <- factorial_design(
    experimental_design,
    n = n
  )

  # Part 2: random design step
  Zr <- random_design(
    experimental_design,
    n = nrow(Zf) / length(experimental_design$alternatives)
  )

  # Merge dataframes
  Z <- dplyr::full_join(
    Zf, Zr,
    by = c("CID", "AID")
  )

  # Function output
  return(as.data.frame(Z))
}
