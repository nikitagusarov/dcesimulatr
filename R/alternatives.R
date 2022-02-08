#############################################################
## This file enlists a class alternative's attributes
## The notations are taken from Train 2003.
## antoine.dubois.fr@gmail.com - March 2021
## nikita.gusarov@univ-grenoble-alpes.fr - February 2022
#############################################################

##############################
# 1 - Decision makers' observed characteristics generation
##############################

#' @title ob_alternatives_att
#'
#' @description The class of observed decision makers' attributes
#'
#' @param N The number of decision makers
#'
#' @param p The number of attributes of each decision makers
#'
#' @method gen generates the attributes by the chosen distribution
#'
#' @examples J <- 10
#' q <- 3
#' a <- ob_alternatives_att$new(J = J, q = q)
#' a$gen("student", param = list(location = rep(100, 3), df = 3))
#' a$gen("help")
#' @export factorial design
#'

ob_alternatives_att <- setRefClass(
  "Alternatives' attributes",
  fields = list(
    J = "numeric", 
    q = "numeric"
  ),
  methods = list(
    gen = function(
      law = "normal", 
      n = J, 
      m = q, 
      param = list()
    ) {
      generation(law, n, m, param)
    }
  )
)
