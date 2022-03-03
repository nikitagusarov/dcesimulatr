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
#' @return data.frame A complete experimental dataset XZ
#'
#' @examples
#' XZ <- experiment_run(population, experimental_design, seed = 10)
#' @export

experiment <- function(population,
                       experimental_design,
                       seed = NULL) {
  # We start with generation of the datastructure
  # and applying utility formulas
  result <- experiment_run(
    population,
    experimental_design,
    seed
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
        CID
      ),
      TR = eval(rules[[i]]$transformation),
      CH = TR == eval(choice)
    )
  }

  # Output
  return(as.data.frame(result))
}
