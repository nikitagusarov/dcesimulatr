################################################################
## This file enlists functions which define alternative class ##
################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

###################################
# 1. Defining "alternative" class #
###################################

#' @title Alternative class
#' @docType class
#'
#' @description The `alternative` R6 class serves to define alternatives to be included into choice sets.
#' The object stores information about the available attributes and their possible values.
#' The values may be specified as vectors using `c()` function or as random declaration using statistical functions of `r` type.
#' The object is initialized without attributes and should be later populated with them.
#'
#' @field attributes A list of attributes definitions
#'
#' @section Modifying methods
#' @method add_attributes Append a list of `call` attributes definitions to the attributes.
#' The attributes should have a name and generation procedure defined (ex: `Quality = c(0:1)`)
#'
#' @section Querrying methods
#' @method get_attributes Get a vector of available attributes' names
#' @method get_laws Get a list of alternatives' generation rules
#'
#' @examples
#' alt <- alternative$new()
#' alt$add_attributes(Quality = c(0, 1), Price = rnorm(mean = 5))
#' alt$get_attributes()
#' alt$get_laws()
#' @export

alternative <- R6::R6Class(
  # Class name
  "alternative",
  # Architecture
  list(
    attributes = NULL,

    # Modifying methods
    add_attributes = function(...) {
      # Convert to quosure to avoid interpretation
      # The accepted calls for now should require n parameter
      # ex: to generate constant value use rep(const)
      self$attributes <- c(
        self$attributes,
        as.list(match.call())[-1]
      )
      invisible(self)
    },

    # Querrying methods
    get_attriutes = function() {
      attr <- names(self$attributes)
      return(attr)
    },
    get_laws = function() {
      laws <- self$attributes
      return(laws)
    }
  )
)



###########################################################
# 2. Defining functions to operate in "alternative" class #
###########################################################

# Alternative class testing

#' @title Alternative class testing
#' @description Test if the given object has an `alternative` class.
#'
#' @param alternative Input object to test
#' @return Logic
#'
#' @examples
#' alt <- alternative$new()
#' is.alternative(alt)
#' @export

is.alternative <- function(alternative) {
  any(
    class(alternative) == "alternative"
  )
}
