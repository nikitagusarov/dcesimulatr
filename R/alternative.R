################################################################
## This file enlists functions which define alternative class ##
################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

###################################
# 1. Defining "alternative" class #
###################################

#' @title
#' @description
#' @param
#' @param
#' @method
#' @examples
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

# alternative class testing

#' @title
#' @description
#' @param
#' @param
#' @method
#' @examples
#' @export

is.alternative <- function(alternative) {
  any(
    class(alternative) == "alternative"
  )
}
