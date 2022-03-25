###############################################################
## This file enlists functions which define population class ##
###############################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

##################################
# 1. Defining "population" class #
##################################

#' @title Population class
#' @docType class
#'
#' @description The `population` R6 class regroups different individuals profiles.
#' It serves as a wrapper for simultaneous interactions with multiple individual profiiles.
#' The object is used for population description and  generation procedures.
#'
#' @field profiles A list of individual profile.
#' @field n A list of individuals' numbers per profile (repecting the profiles' order).
#'
#' @examples
#' # Create individuals
#' ind1 <- individual$new()
#' ind1$add_characteristics(Age = rnorm(mean = 40, sd = 10))
#' ind1$add_decision_rule(drule <- decision_rule$new())
#' ind2 <- individual$new()
#' ind2$add_characteristics(Age = rnorm(mean = 30, sd = 5))
#' ind2$add_decision_rule(drule <- decision_rule$new())
#'
#' # Regroup individuals into population
#' pop <- population$new(profiles = list(ind1, ind2), n = list(10, 15))
#'
#' # Add new profile
#' ind3 <- individual$new()
#' ind3$add_characteristics(Age = rnorm(mean = 50, sd = 4), Salary = runif(min = 1, max = 5))
#' ind3$add_decision_rule(drule <- decision_rule$new())
#' pop$add_profile(ind3, 5)
#' pop$get_chars()
#' pop$get_n()
#' pop$get_rules()
#' @export
#' @import R6

population <- R6::R6Class(
  # Class name
  "population",
  # Architecture
  list(
    # Values
    profiles = NULL,
    n = NULL,

    # Initialize
    #' @method initialize population
    #' @description Create a new `population` object.
    #' The function allows to create an object populated with individual profiles.
    #' @param profiles A list of individual profiles for population.
    #' @param n The associated numbers for each profile to appear in the dataset.
    initialize = function(profiles = list(NULL),
                          n = list(NULL)) {
      if (length(profiles) != length(n)) {
        stop("Not all of profiles have corresponding n")
      }

      if (class(profiles) != "list") {
        stop("Profiles are not in list")
      }

      # Write values
      self$profiles <- profiles
      self$n <- n
    },

    # Methods to modify object
    #' @method add_profile population
    #' @description Add new individual profile and respective desired number of individuals.
    #' @param individual Individual profile to be added
    #' @param n A number associate to the added profile
    #' @param profile_name An added profile name, not required.
    #' Is NULL by default.
    add_profile = function(individual, n, profile_name = NULL) {
      # Verification
      if (!any(class(individual) == "individual")) {
        stop("No valid individual object provided")
      }

      # Add to our list of profiles
      if (!is.null(profile_name)) {
        self$profiles[[{{ profile_name }}]] <- individual
      } else {
        self$profiles[[length(self$profiles) + 1]] <- individual
      }

      # Add n to list of n
      if (!is.null(profile_name)) {
        self$n[[{{ profile_name }}]] <- n
      } else {
        self$n[[length(self$n) + 1]] <- n
      }
      invisible(self)
    },

    # Methods to querry the object
    #' @method get_chars population
    #' @description Get a vector of available characteristics' names across all individual profiles in population.
    #' @return Character vector with unique characteristics names within populatoin.
    get_chars = function() {
      # Get list of chars from all profiles
      chars <- lapply(
        self$profiles,
        function(x) {
          names(x$characteristics)
        }
      )
      # Keep unique
      chars <- unique(
        unlist(chars)
      )
      return(chars)
    },
    #' @method get_n population
    #' @description Get a vector regroupping individuals' numbers per profile
    #' @return Numeric vector with numbers of n by individual profile.
    get_n = function() {
      # Get n as vector
      n <- unlist(self$n)
      return(n)
    },
    #' @method get_rules population
    #' @description Extract `decision_rule` objects across individual profiles
    #' @return A list of rules present within population.
    get_rules = function() {
      # Querry individuals for their rules
      rules <- lapply(
        self$profiles,
        function(x) {
          x$get_rule()
        }
      )
      return(rules)
    }
  )
)



##########################################################
# 2. Defining functions to operate in "population" class #
##########################################################

# Population testing

#' @title Population class testing
#' @description Test if the given object has an `population` class.
#'
#' @param population Input object to test
#' @return logic
#'
#' @examples
#' pop <- population$new()
#' is.population(pop)
#' @export

is.population <- function(population) {
  any(class(population) == "population")
}

# Generation function (indivduals matrix)

#' @title Generate population
#' @description Generate population data (X in standart notation) from a population object.
#'
#' @param population Input population configuration
#' @param seed The seed to be set before attempting to generate population data.
#' Defaults to NULL.
#' @param class Logical.
#' Indicates whether or not to include individual class information to the resulting data.frame.
#' Defaults to NULL.
#' @return data.frame A data.frame (X) with simulated population (one row per individual).
#'
#' @examples
#' # Create individuals
#' ind1 <- individual$new()
#' ind1$add_characteristics(Age = rnorm(mean = 40, sd = 10))
#' ind1$add_decision_rule(drule <- decision_rule$new())
#' ind2 <- individual$new()
#' ind2$add_characteristics(Age = rnorm(mean = 30, sd = 5))
#' ind2$add_decision_rule(drule <- decision_rule$new())
#'
#' # Regroup individuals into population
#' pop <- population$new(profiles = list(ind1, ind2), n = list(10, 15))
#' X <- population_gen(pop)
#' @export

population_gen <- function(population, seed = NULL, class = TRUE) {
  # Avoid check failure
  i <- NULL

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
  chars <- population$get_chars()

  # Check class
  if (class(class) != "logical") {
    # Set info variable for class
    if (length(population$profiles) >= 1 & is.null(class)) {
      class <- TRUE
    } else {
      class <- FALSE
    }
  }

  # Run simulation
  X <- foreach(
    i = seq_along(population$profiles),
    .combine = "rbind"
  ) %do% {
    # Get profile chars, laws and obs numbers
    laws <- population$profiles[[i]]$get_laws()
    n <- population$get_n()[i]
    # Update laws with required n
    for (j in seq_along(laws)) {
      laws[[j]]$n <- n
    }

    # Create DF per ind profile
    X <- data.frame(
      lapply(laws, eval)
    )

    # Check compeltenes
    if (
      !rlang::is_empty(
        adchars <- setdiff(chars, colnames(X))
      )
    ) {
      X[adchars] <- rep(NA, n)
    }

    # Add profile information
    if (class == TRUE) {
      X["class"] <- rep(i, n)
    }

    # Exit foreach loop
    return(X)
  }

  # Add Individual ID
  X["IID"] <- 1:nrow(X)

  # Return
  return(X)
}
