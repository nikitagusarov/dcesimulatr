#####################################################################
## This file enlists functions used for data generation procedures ##
#####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

###############################################
# 1. Application of decision rule and choices #
###############################################

#' @title Experiment execution
#' @description This function simulates the experimental procedure.
#' The function calls on `experiment_run` procedure at the first step.
#' Then the decision procedure is carried out.
#' The transformation functions are applied to TU (functions output) and then the choice criteria is applied over the results.
#'
#' @param population A `population` object.
#' @param experimental_design An `experimental_desing` object.
#' @param seed The desired seed to be set before data generation.
#' No seed is set by default (`seed = NULL`).
#' @param XZ The experimental data.frame to be used instead of generated one
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
#'
#' pop <- population$new(profiles = list(ind3), n = list(5))
#'
#' # Create alternatives and regroup them into design
#' alt1 <- alternative$new()
#' alt1$add_attributes(Quality = runif(min = 0, max = 1), Price = rnorm(mean = 5))
#' alt2 <- alternative$new()
#' alt2$add_attributes(Size = runif(min = 0, max = 1), Price = rnorm(mean = 6))
#' edesign <- experimental_design$new(alternatives = list(alt1, alt2))
#'
#' # Full experiment
#' XZ <- experiment(pop, edesign, seed = 10)
#' @export

experiment <- function(population,
                       experimental_design,
                       seed = NULL,
                       XZ = NULL) {
  # Avoid check error
  TR <- CID <- IID <- NULL

  # We start with generation of the datastructure
  # and applying utility formulas
  result <- experiment_run(
    population,
    experimental_design,
    seed,
    XZ
  )

  # Retrieve rules from population
  rules <- population$get_rules()

  # Apply computation
  for (i in seq_along(population$profiles)) {
    # Preset choice criteria expression
    choice <- rules[[i]]$choice
    choice[[2]] <- expr(TR)

    # Apply rules and select
    result <- dplyr::mutate(
      dplyr::group_by(
        result,
        IID,
        CID
      ),
      TR = eval(rules[[i]]$transformation),
      CH = TR == eval(choice)
    )
  }

  # Output
  return(as.data.frame(result))
}
