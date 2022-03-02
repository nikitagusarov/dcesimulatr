#####################################################################
## This file enlists functions used for data generation procedures ##
#####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

###############################################
# 1. Application of decision rule and choices #
###############################################

#' @title
#' @description
#' @param
#' @param
#' @method
#' @examples
#' @export

experiment <- function(population,
                       experimental_design) {
  # We start with generation of the datastructure
  # and applying utility formulas
  result <- experiment_run(
    population,
    experimental_design
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
