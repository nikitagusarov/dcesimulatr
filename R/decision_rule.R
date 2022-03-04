#############################################################
## This file enlists functions which define decision rules ##
#############################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

#####################################
# 1. Defining "decision_rule" class #
#####################################

#' @title Decision rule class
#' @docType class
#'
#' @description The `decision_rule` R6 class serves to define individuals'decision rules.
#' The object stores information about the decision rule:
#' a list of formulas to be applied on the data;
#' the associated random noise terms,
#' transformation rules and choice selection pattern.
#'
#' @field formula A list of formulas associated with respective alternatives. 
#' @field noise A list of noise specifications on per alternative basis. 
#' @field transformation The desired transformation to be applied. 
#' The transformation should be a function of Total Utility (TU).
#' The default transforamtion is `expr(exp(TU) / sum(exp(TU)))`
#' @field choice The desired criteria should be declared as function.
#'
#' @examples
#' drule <- decision_rule$new()
#' drule$add_noise(rnorm(), rnorm(sd = 2))
#' drule$add_formulas(Age + 2 * Quality, 1.5 * Age + Quality^2)
#' drule$modify_transformation(TU)
#' drule$modify_choice(min())
#' 
#' @export
#' @import rlang R6

decision_rule <- R6::R6Class(
  # Class name
  "decision_rule",
  # Architecture
  list(
    # Values
    formula = list(),
    noise = list(),
    transformation = rlang::expr(exp(TU) / sum(exp(TU))),
    choice = rlang::expr(max()),

    # Methods
    #' @method add_noise decision_rule
    #' @description Append a list of `call` noise definitions to the noise field.
    #' @param ... The noise is declared as randomisation function 
    #' (ex: `evd::rgumbel(loc = 0, scale = 1)`)
    add_noise = function(...) {
      self$noise <- c(
        self$noise,
        as.list(match.call())[-1]
      )
      invisible(self)
    },
    #' @method add_formulas decision_rule
    #' @description Append a list of `call` formula definitions to the formula field.
    #' @param ... The formulas should be defined in the same order as alternatives the individual will face 
    #' (ex: `Age + 2*Quality, 1.5*Age + Quality^2`)
    add_formulas = function(...) {
      # A formula with parameters should be provided
      self$formula <- c(
        self$formula,
        as.list(match.call())[-1]
      )
      invisible(self)
    },
    #' @method modify_transformation decision_rule
    #' @description Specify transformation to applied on Total Utility for individual within each choice set. 
    #' @param transformation The desired transformation to be applied. 
    #' The transformation should be a function of Total Utility (TU).
    #' The default transforamtion is `expr(exp(TU) / sum(exp(TU)))`
    modify_transformation = function(transformation) {
      self$transformation <- enexpr(transformation)
      invisible(self)
    },
    #' @method modify_choice decision_rule
    #' @description The choice rule represents the criteria of individual's final choice.
    #' @param choice The desired criteria should be declared as function.
    #' The default value is `max()` (assuming the individual chooses the alternative with higher associated probability).
    modify_choice = function(choice) {
      self$choice <- enexpr(choice)
      invisible(self)
    }
  )
)



#############################################################
# 2. Defining functions to operate in "decision_ruel" class #
#############################################################

# Decision rule class testing

#' @title Decision rule class testing
#' @description Test if the given object has an `decision_rule` class.
#'
#' @param decision_rule Input object to test
#' @return Logic
#'
#' @examples
#' drule <- decision_rule$new()
#' is.decision_rule(drule)
#' @export

is.decision_rule <- function(decision_rule) {
  any(class(decision_rule) == "decision_rule")
}
