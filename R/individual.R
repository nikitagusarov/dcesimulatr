################################################################
## This file enlists functions which define individuals class ##
################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

##################################
# 1. Defining "individual" class #
##################################

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export 

individual = R6::R6Class(
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
            self$characteristics = c(
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
            self$decision_rule = decision_rule
            invisible(self)
        },

        # Querrying methods
        get_chars = function() {
            chars = names(self$characteristics)
            return(chars)
        },
        get_laws = function() {
            laws = self$characteristics
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

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export 

is.individual = function(individual) {
    any(
        class(individual) == "individual"
    )
}
