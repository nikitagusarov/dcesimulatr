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
        },

        # Methods to querry the object
        get_chars = function() {
            # Get list of chars from all profiles
            chars = lapply(
                self$profiles, 
                function(x) { names(x$characteristics) }
            )
            # Keep unique
            chars = unique(
                unlist(chars)
            )
            return(chars)
        },
        get_n = function() {
            # Get n as vector
            n = unlist(self$n)
            return(n)
        },
        get_rules = function() {
            # Querry individuals for their rules
            rules = lapply(
                self$profiles,
                function(x) { x$get_rule() }
            )
            return(rules)
        }
    )
)



##########################################################
# 2. Defining functions to operate in "population" class #
##########################################################

# Population testing

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export 

is.population = function(population) {
    any(class(population) == "population")
}

# Generation function (indivduals matrix)

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export

population_gen = function(
    population, seed = NULL, class = NULL
) {
    # Verification
    if (!is.population(population)) {
        stop("No valid population object provided")
    }
    if (!all(unlist(
        lapply(population$profiles, is.individual)
    ))) {
        stop("No valid individuals' profiles provided")
    }

    # Reset seed if required
    if (!is.null(seed)) {
        set.seed(seed)
    }

    # Get unique characteristics' names
    chars = population$get_chars()

    # Set info variable for class
    if (length(population$profiles) > 1 & is.null(class)) {
        class = TRUE
    } else { class = FALSE }

    # Run simulation
    foreach (
        i = seq_along(population$profiles),
        .combine = "rbind"
    ) %do% {
        # Get profile chars, laws and obs numbers
        laws = population$profiles[[i]]$get_laws()
        n = population$get_n()[i]
        # Update laws with required n
        for (j in seq_along(laws)) {
            laws[[j]]$n = n
        }

        # Create DF per ind profile
        X = data.frame(
            lapply(laws, eval)
        )

        # Check compeltenes
        if (
            !rlang::is_empty(
                adchars <- setdiff(chars, colnames(X))
            )
        ) {
            X[adchars] = rep(NA, n)
        }

        # Add profile information
        if (class == TRUE) {
            X["class"] = rep(i, n)
        }

        # Exit foreach loop
        return(X)
    }
}
