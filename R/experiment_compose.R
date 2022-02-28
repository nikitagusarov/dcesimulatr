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

experiment_compose = function(
    population,
    experimental_design
) {
    # Set configuration variable
    # Number of observation per ind
    if (is.null(experimental_design$n)) {
        n = 1
    } else {
        n = experimental_design$n
    }
    # Number of alternatives
    j = length(
        experimental_design$alternatives
    )
    # Whether we desire identical choice sets across ind
    identical = experimental_design$identical

    # Generate population
    X = population_gen(
        population, seed = NULL, class = NULL
    )

    # Generate alternatives
    if (identical == TRUE) {
        # Generate one Z
        Z = alternatives_gen(
            experimental_design, seed = NULL, n = n
        )
        # Repeat it
        Z = dplyr::slice(
            Z, rep(1:dplyr::n(), nrow(X))
        )
    } else {
        Z = alternatives_gen(
            experimental_design, seed = NULL, n = n*nrow(X)
        )
    }

    # Bind frames
    XZ = cbind(
        dplyr::slice(X, rep(1:dplyr::n(), each = n*j)),
        Z
    )

    # Output
    return(XZ)
}
