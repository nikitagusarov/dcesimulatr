################################################################
## This file enlists functions which define individuals class ##
################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

##################################
# 1. Defining "individual" class #
##################################

#' @title Individual class
#' @docType class
#'
#' @description The `individual` R6 class serves to define individuals' profiles to be included into  population.
#' The object stores information about the individuals' characteristics, as well as the decision rules.
#' The values may be specified as random declaration using statistical functions of `r` type.
#' The object is initialized without attributes and should be later populated with them.
#'
#' @field characteristics A list of characteristics' definitions
#' @field decision_rule A decision rule object
#' @filed X A dataset describing individuals
#'
#' @examples
#' ind <- individual$new()
#' ind$add_characteristics(Age = rnorm(mean = 40, sd = 10))
#' ind$add_decision_rule(drule <- decision_rule$new())
#' ind$get_chars()
#' ind$get_laws()
#' ind$get_rule()
#' @export
#' @import R6

individual <- R6::R6Class(
  # Class name
  "individual",
  # Architecture
  list(
    # Values
    characteristics = NULL,
    decision_rule = NULL,
    X = NULL,

    # Modifying methods
    #' @method add_characteristics individual
    #' @description Append a list of `call` attributes definitions to the attributes.
    #' @param ... The attributes should have a name and generation procedure defined (ex: `Age = rnorm(mean = 40, sd = 10)`)
    add_characteristics = function(...) {
      # Convert to quosure to avoid interpretation
      # The accepted calls for now should require n parameter
      # ex: to generate constant value use rep(const)
      self$characteristics <- c(
        self$characteristics,
        as.list(match.call())[-1]
      )
      invisible(self)
    },
    #' @method add_decision_rule individual
    #' @description Create or replace an existing decision rule
    #' @param decision_rule A decision rule to be assigned to individual profile
    add_decision_rule = function(decision_rule) {
      # Verification
      if (!is.decision_rule(decision_rule)) {
        "An error occured, no valid decision rule generated"
      }
      # Assignement
      self$decision_rule <- decision_rule
      invisible(self)
    },
    add_data = function(X) {
      # Verification
      if (!is.data.frame(X)) {
        "An error occured, no valid data provided (supply a data.frame object"
      }
      # Assignement
      self$X <- X
      invisible(self)
    },

    # Querrying methods
    #' @method get_chars individual
    #' @description Get a vector of available characteristics' names
    #' @return Character vector with unique characteristics names.
    get_chars = function() {
      chars <- names(self$characteristics)
      return(chars)
    },
    #' @method get_laws individual
    #' @description Get a list of alternatives' generation rules
    #' @return Get a list of laws associated to individual's characteristics.
    get_laws = function() {
      laws <- self$characteristics
      return(laws)
    },
    #' @method get_rule individual
    #' @description Extract `decision_rule` object
    #' @return A `decisio_rule` object of the given individual profile.
    get_rule = function() {
      return(self$decision_rule)
    },
    #' @method get_data individual
    #' @description Extract `data` object
    #' @return A `data` object of the given individual profile.
    get_data = function() {
      return(self$X)
    }
  )
)



##########################################################
# 2. Defining functions to operate in "individual" class #
##########################################################

# individual class testing

#' @title Individual class testing
#' @description Test if the given object has an `individual` class.
#'
#' @param individual Input object to test
#' @return Logic
#'
#' @examples
#' ind <- individual$new()
#' is.individual(ind)
#' @export

is.individual <- function(individual) {
  any(
    class(individual) == "individual"
  )
}
