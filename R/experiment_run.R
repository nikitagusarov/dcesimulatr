#####################################################################
## This file enlists functions used for data generation procedures ##
#####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

######################################
# 1. Application of decision formula #
######################################

#' @title Run experiment
#' @description This function simulates running of an experiment.
#' First, the `experiment_compose` is executed to obtain a coupling between X and Z matrices.
#' Then Deterministic Utilities (DU)  and Total Utilities (TU) are calculated using the specified formulas.
#' Note, that even the TU abbreviation is used this still makes possible to recalibrate the generation functions, tranforms and decision tules to create a Regret Minimisation (RM) framework i.e.
#'
#' @param population A `population` object.
#' @param experimental_design An `experimental_desing` object.
#' @param seed The seed to be preset for data simulation purposes.
#' The default is NULL to use system defined or random seed.
#' @return data.frame A complete experimental dataset with associated utilities.
#'
#' @examples
#' # Generate individual profile and population
#' ind3 <- individual$new()
#' ind3$add_characteristics(Age = rnorm(mean = 50, sd = 4), Salary = runif(min = 1, max = 5))
#' ind3$add_decision_rule(drule <- decision_rule$new())
#' pop <- population$new(profiles = list(ind3), n = list(5))
#'
#' # Create alternatives and regroup them into design
#' alt1 <- alternative$new()
#' alt1$add_attributes(Quality = runif(min = 0, max = 1), Price = rnorm(mean = 5))
#' alt2 <- alternative$new()
#' alt2$add_attributes(Size = runif(min = 0, max = 1), Price = rnorm(mean = 6))
#' edesign <- experimental_design$new(alternatives = list(alt1, alt2), n = 4)
#'
#' # Run experiment
#' res <- experiment_run(pop, edesign)
#' @export

experiment_run <- function(population,
                           experimental_design,
                           seed = NULL) {
  # Avoid check error
  DU <- NULL

  # First we perform coupling
  XZ <- experiment_compose(
    population,
    experimental_design,
    seed
  )

  # Retrieve rules from population
  rules <- population$get_rules()

  # Create deterministic utility dummy
  XZ$DU <- rep(NA, nrow(XZ))
  XZ$TU <- rep(NA, nrow(XZ))

  # Apply rules
  for (i in seq_along(population$profiles)) {
    # Get classes
    cl <- unique(XZ$AID)
    # Update noise params
    noise <- rules[[i]]$noise
    # Update rules
    form <- rules[[i]]$formula

    # Verify length of noise
    if (
      (length(noise) == 1) &
        (length(noise) != length(cl))
    ) {
      noise <- rep(noise, length(cl))
    }

    # Verify length of formula
    if (
      (length(form) == 1) &
        (length(form) != length(cl))
    ) {
      form <- rep(form, length(cl))
    }

    # Apply to XZ
    for (j in seq_along(cl)) {
      # Get noise params for alternative
      noise_j <- noise[[j]]
      noise_j$n <- nrow(XZ[
        (XZ$class == i) & (XZ$AID == j),
      ])

      # Get respecive formula
      formula_j <- form[[j]]

      # Application
      XZ[(XZ$class == i) & (XZ$AID == j), ] <- dplyr::mutate(
        XZ[
          (XZ$class == i) & (XZ$AID == j),
        ],
        DU = !!formula_j,
        TU = DU + !!noise_j
      )
    }
  }

  # Output
  return(XZ)
}
