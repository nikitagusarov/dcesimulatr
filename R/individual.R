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
#'
#' @section Modifying methods
#' @method add_characteristics
#' @description Append a list of `call` attributes definitions to the attributes.
#' The attributes should have a name and generation procedure defined (ex: `Age = rnorm(mean = 40, sd = 10)`)
#' @method add_decision_rule
#' @description Create or replace an existing decision rule
#'
#' @section Querrying methods
#' @method get_chars
#' @description Get a vector of available characteristics' names
#' @method get_laws
#' @description Get a list of alternatives' generation rules
#' @method get_rule
#' @description Extract `decision_rule` object
#'
#' @examples
#' ind <- individual$new()
#' ind$add_characteristics(Age = rnorm(mean = 40, sd = 10))
#' ind$add_decision_rule(drule <- decision_rule$new())
#' ind$get_char()
#' ind$get_laws()
#' ind$get_rule()
#' @export

individual <- R6::R6Class(
  # Class name
  "individual",
  # Architecture
  list(
    # Values
    characteristics = NULL,
    decision_rule = NULL,

    # Modifying methods
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
    add_decision_rule = function(decision_rule) {
      # Verification
      if (!is.decision_rule(decision_rule)) {
        "An error occured, no valid decision rule generated"
      }
      # Assignement
      self$decision_rule <- decision_rule
      invisible(self)
    },

    # Querrying methods
    get_chars = function() {
      chars <- names(self$characteristics)
      return(chars)
    },
    get_laws = function() {
      laws <- self$characteristics
      return(laws)
    },
    get_rule = function() {
      return(self$decision_rule)
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
