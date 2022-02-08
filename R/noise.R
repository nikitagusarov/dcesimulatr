#############################################################
## This file enlists functions which generate noise
## characteristics.
## The notations are taken from Train 2003.
## antoine.dubois.fr@gmail.com - March 2021
#############################################################

##############################
# 1 - Decision makers' observed characteristics generation
##############################
#' @title ob_decision_makers_att
#'
#' @description The class of observed decision makers' attributes
#'
#' @param N The number of decision makers
#'
#' @param p The number of attributes of each decision makers
#'
#' @method gen generates the attributes by the chosen distribution
#'
#' @examples N <- 10
#' p <- 3
#' a <- ob_decision_makers_att$new(N = N, p = p)
#' a$gen("student", param = list(location = rep(100, 3), df = 3))
#' a$gen()
#' a$gen("help")
#' @export factorial design
#'
#' @title noise
#'
#' @description The class noise
#'
#' @param N The number of decision makers
#'
#' @param J The number of attributes of alternatives
#'
#' @method gen generates the noise
#'
#'
#' @examples N <- 10
#' J <- 5
#' a <- noise$new(N = N, J = J)
#' a$gen()
#' a$gen("normal", param = list(mu = rep(-1000, J)))
#' a$gen("student", param = list(scale = diag(2, J), df = 10))
#' a$gen("discrete_uniform")
#' @export factorial design
#'

noise <- setRefClass("noise",
  fields = list(N = "numeric", J = "numeric"),
  methods = list(gen = function(law = "gumbel", n = N, m = J, param = list()) {
    generation(law, n, m, param)
  })
)
