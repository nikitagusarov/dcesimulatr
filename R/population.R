###############################################################
## This file enlists functions which define population class ##
###############################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

##################################
# 1. Defining "population" class #
##################################

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export 

population = R6::R6Class(
    # Class name
    "population",
    # Architecture
    list(
        # Values
        profiles = NULL,
        n = NULL,

        # Initialize
        initialize = function(
            profiles = list(NULL), 
            n = list(NULL)
        ) {
            if (length(profiles) != length(n)) {
                stop("Not all of profiles have corresponding n")
            }

            if (class(profiles) != "list") {
                stop("Profiles are not in list")
            }

            # Write values
            self$profiles = profiles
            self$n = n
        },
        
        # Methods to modify object
        add_profile = function(
            individual, n, profile_name = NULL
        ) {
            # Verification
            if (!any(class(individual) == "individual")) {
                stop("No valid individual object provided")
            }

            # Add to our list of profiles
            if (!is.null(profile_name)) {
                self$profiles[[ {{ profile_name }} ]] = individual
            } else {
                self$profiles[[length(self$profiles) + 1]] = individual
            }

            # Add n to list of n
            if (!is.null(profile_name)) {
                self$n[[ {{ profile_name }} ]] = n
            } else {
                self$n[[length(self$n) + 1]] = n
            }
            invisible(self)
        }
    )
)



##########################################################
# 2. Defining functions to operate in "population" class #
##########################################################

population_gen = function(
    population
) {
    # Verification
    if (!any(class(individual) == "individual")) {
        stop("No valid individual object provided")
    }

    # Run simulation

}