#############################################################
## This file contains the preference coefficients
## The notations are taken from Train 2003.
## antoine.dubois.fr@gmail.com - March 2021
## nikita.gusarov@univ-grenoble-alpes.fr - February 2022
#############################################################

##############################
# 1 - Decision makers' observed characteristics generation
##############################

#' @title preference_coef
#'
#' @description Class of preference coefficients
#'
#' @param N The number of decision makers
#'
#' @param p The number of attributes of each decision makers
#'
#' @param q The number of attributes of each attributes
#'
#' @method gen generates the attributes by the chosen distribution
#'
#' @method fit computes the preference coefficients
#'
#' @examples S <- matrix(rnorm(100), nrow = 20, byrow = TRUE)
#' X <- matrix(rnorm(20), nrow = 4, byrow = TRUE)
#' choice <- c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5))
#' estimator <- preference_coef(N = 20, p = 5, q = 5)
#'
#' a <- preference_coef(N = 20, p = 5, q = 5)
#' a$gen(law = "student")
#' a$gen(heterogeneity = TRUE)
#' 
preference_coef <- setRefClass(
  "Preference coefficients",
  fields = list(
    N = "numeric", 
    p = "numeric", 
    q = "numeric"
  ),
  methods = list(
    gen = function(
      law = "normal", 
      n = N, m = p, 
      heterogeneity = FALSE, 
      param = list()
    ) {
      if (heterogeneity) {
        generation(law, n, m, param)
      } else {
        matrix(
          generation(law, n = 1, m, param), 
          ncol = m, nrow = n, 
          byrow = TRUE
        )
      }
    }
  )
)
