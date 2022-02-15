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

        # Methods
        add_characteristics = function(...) {
            # Convert to quosure to avoid interpretation
            self$characteristics = c(
                self$characteristics,
                as.list(match.call())[-1]
            )
            invisible(self)
        },
        add_decision_rule = function(
            formula, 
            noise
        ) {
            self$decision_rule = decision_rule$new(
                formula = {{ formula }},
                noise = {{ noise }}
            )
            invisible(self)
        }
    )
)