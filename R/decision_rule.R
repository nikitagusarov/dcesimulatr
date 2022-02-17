#############################################################
## This file enlists functions which define decision rules ##
#############################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

#####################################
# 1. Defining "decision_rule" class #
#####################################

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export 

decision_rule = R6::R6Class(
    # Class name
    "decision_rule",
    # Architecture
    list(
        # Values
        formula = "quosure",
        noise = "quosure",

        # Initialize
        initialize = function(
            noise = evd::rgumbel(loc = 0, scale = 1),
            formula = NULL
        ) {
            self$noise = rlang::enquo(noise)
            self$formula = rlang::enquo(formula)
        },

        # Methods
        modify_noise = function(
            new_noise = rnorm(mean = 1, sd = 10) # evd::rgumbel(loc = 0, scale = 1)
        ) {
            self$noise = rlang::enquo(new_noise)
            invisible(self)
        },
        modify_formula = function(
            new_formula = NULL
        ) {
            # A formula wit random parameters should be provided
            self$formula = rlang::enquo(new_formula)
            invisible(self)
        }
    )
)



#############################################################
# 2. Defining functions to operate in "decision_ruel" class #
#############################################################

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export 

is.decision_rule = function(decision_rule) {
    any(class(decision_rule) == "decision_rule")
}
