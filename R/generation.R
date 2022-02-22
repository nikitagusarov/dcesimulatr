#####################################################################
## This file enlists functions used for data generation procedures ##
#####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

#########################################################
# 1. Coupled creation of Z and X into single data.frame #
#########################################################

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export

experiment_run = function(
    population,
    exerimental_design
) {
    # Generate population
    X = population_gen(
        population, seed = NULL, class = NULL
    )

    # Generate alternatives
    Z = alternatives_gen(
        experimental_design, seed = NULL, n = NULL
    )
}