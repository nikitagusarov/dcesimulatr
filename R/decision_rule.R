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
#' @field characteristics A list of characteristics' definitions
#' @field decision_rule A decision rule object
#'
#' @section Modifying methods
#' @method add_noise
#' @description Append a list of `call` noise definitions to the noise field.
#' The noise is declared as randomisation function (ex: `evd::rgumbel(loc = 0, scale = 1)`)
#' @method add_formulas
#' @description Append a list of `call` formula definitions to the formula field.
#' The formulas should be defined in the same order as alternatives the individual will face (ex: `Age + 2*Quality, 1.5*Age + Quality^2`)
#' @method modify_transformation
#' @description The transformation should be a function of Total Utility (TU).
#' The default transforamtion is `expr(exp(TU) / sum(exp(TU)))`
#' @method modify_choice
#' @description The choice rule represents the criteria of individual's final choice.
#' The desired criteria should be declared as function.
#' The default value is `max()` (assuming the individual chooses the alternative with higher associated probability).
#'
#' @examples
#' drule <- decision_rule$new()
#' drule$add_noise(evd::rgumbel(loc = 0, scale = 1), evd::rgumbel(loc = 0, scale = 2))
#' drule$add_formulas(Age + 2 * Quality, 1.5 * Age + Quality^2)
#' drule$modify_transformation(TU)
#' drule$modify_choice(min())
#' @export
#' @import rlang

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
    add_noise = function(...) {
      self$noise <- c(
        self$noise,
        as.list(match.call())[-1]
      )
      invisible(self)
    },
    add_formulas = function(...) {
      # A formula with parameters should be provided
      self$formula <- c(
        self$formula,
        as.list(match.call())[-1]
      )
      invisible(self)
    },
    modify_transformation = function(transformation) {
      self$transformation <- transformation
      invisible(self)
    },
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
