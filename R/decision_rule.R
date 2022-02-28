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
        formula = list(),
        noise = list(),
        transformation = expr(exp(TU) / sum(exp(TU))),
        choice = expr(max()),

        # Methods
        add_noise = function(...) {
            self$noise = c(
                self$noise,
                as.list(match.call())[-1]
            )
            invisible(self)
        },
        add_formulas = function(...) {
            # A formula with parameters should be provided
            self$formula = c(
                self$formula,
                as.list(match.call())[-1]
            )
            invisible(self)
        },
        modify_transformation = function(
            transformation
        ) {
            self$transformation = transformation
            invisible(self)
        },
        modify_choice = function(
            choice
        ) {
            self$choice = choice
            invisible(self)
        }
    )
)



#############################################################
# 2. Defining functions to operate in "decision_ruel" class #
#############################################################

# Decision rule class testing

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
