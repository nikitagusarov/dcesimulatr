#############################################################
## This file enlists functions which generate random matrices.
## The notations are taken from Train 2003.
## antoine.dubois.fr@gmail.com - March 2021
#############################################################

##############################
# 1 - Matrices random generation functions
##############################

#' @title generation
#'
#' @name generation
#'
#' @description 3 distributions are currently available: normal, student,
#' discrete, gumbel. In addition, gen("hepl") provides information about how to call
#' the optional parameters
#'
#' @param n The number rows
#'
#' @param m The number of columns
#'
#' @param law The distribution generating the matrix
#'
#' @examples generation("student", 10, 5, param = list(location = rep(100, 5)))
#' generation("help")
#' @export alternatives data, consumers data
#'

generation <- function(law, n, m, param) {
  if (law == "normal") {
    gen_normal(n, m, param)
  } else if (law == "student") {
    gen_student(n, m, param)
  } else if (law == "discrete_uniform") {
    gen_discrete_uniform(n, m, param)
  } else if (law == "continuous_uniform") {
    gen_continuous_uniform(n, m, param)
  } else if (law == "beta") {
    gen_beta(n, m, param)
  } else if (law == "gumbel") {
    gen_gumbel(n, m, param)
  } else if (law == "chi2") {
    gen_chi2(n, m, param)
  } else if (law == "poisson") {
    gen_poisson(n, m, param)
  } else if (law == "exp") {
    gen_exp(n, m, param)
  } else if (law == "geo") {
    gen_geo(n, m, param)
  } else if (law == "empirical") {
    gen_empirical(n, m, param)
  } # empirical distribution
  else if (law == "help") {
    information()
  } else {
    cat("The available distributions are 'normal', 'student', 'discrete_uniform' and 'gumbel'")
  }
}


#' @title gen_normal
#'
#' @description A function generating a random matrix with multivariate normal distribution
#'
#' @param n The number rows
#'
#' @param m The number of columns
#'
#' @param mu The mean vector
#'
#' @param sd The optional optional covariance matrix
#'
#' @examples gen_normal(10, 2, param = list(mu = rep(-10, 2)))
#' @export alternatives data, consumers data
#'
#' @import matrixcalc
#' @import mvtnorm
#'

gen_normal <- function(n, m, param) {
  if (is.null(param$mu)) {
    param$mu <- rep(0, m)
  } else if (length(param$mu) == 1) {
    param$mu <- rep(param$mu, m)
  } else if (m != length(param$mu)) {
    stop("Unconsistent dimentions of the mean vector, number of attributes ", m, " but declared ", length(param$mu))
  }

  if (is.null(param$sd)) {
    param$sd <- diag(1, m)
  } else if (length(param$sd) == 1) {
    param$sd <- diag(param$sd, m)
  } else if (m != nrow(param$sd)) {
    stop("Unconsistent dimentions of the covariance matrix, number of attributes ", m, " but declared ", nrow(param$sd))
  }

  if (matrixcalc::is.positive.definite(param$sd) & matrixcalc::is.symmetric.matrix(param$sd)) {
    return(mvtnorm::rmvnorm(n = n, as.vector(param$mu), param$sd))
  } else {
    print("The sd matrix is not a covariance matrix")
  }
}


#' @title gen_student
#'
#' @description A function generating a random matrix with multivariate student distribution
#'
#' @param n The number rows
#'
#' @param m The number of columns
#'
#' @param location The optional location parameter
#'
#' @param scale The optional scale matrix
#'
#' @examples gen_student(10, 2, param = list(scale = diag(3, 2), df = 5))
#' @export alternatives data, consumers data
#'
#' @import matrixcalc
#' @import mvtnorm
#'

gen_student <- function(n, m, param) {
  if (is.null(param$location)) {
    param$location <- rep(0, m)
  } else if (length(param$location) == 1) {
    param$location <- rep(param$location, m)
  } else if (m != length(param$location)) {
    stop("Unconsistent dimentions of the mean vector, number of attributes ", m, " but declared ", length(param$mu))
  }

  if (is.null(param$scale)) {
    param$scale <- diag(1, m)
  } else if (length(param$scale) == 1) {
    param$scale <- diag(param$scale, m)
  } else if (m != nrow(param$scale)) {
    stop("Unconsistent dimentions of the covariance matrix, number of attributes ", m, " but declared ", nrow(param$sd))
  }

  if (is.null(param$df)) {
    param$df <- 2
  }

  if (matrixcalc::is.positive.definite(param$scale) & matrixcalc::is.symmetric.matrix(param$scale)) {
    # the matrix whose lines characterizes every decision makers
    return(mvtnorm::rmvt(n = n, delta = as.vector(param$location), sigma = param$scale, df = param$df))
  } else {
    print("The scale matrix is not a covariance matrix")
  }
}


#' @title gen_discrete_uniform
#'
#' @description A function generating a random matrix with discrete uniform distribution
#'
#' @param n The number rows
#'
#' @param m The number of columns
#'
#' @param a Lower bound of the distribution's support
#'
#' @param b Upper bound of the distribution's support
#'
#' @examples gen_discrete_uniform(10, 2, param = list(a = 2, b = 3))
#' @import stats
#'
#' @export alternatives data, consumers data
#'

gen_discrete_uniform <- function(n, m, param) {
  if (is.null(param$a)) {
    param$a <- 0
  }
  if (is.null(param$b)) {
    param$b <- 1
  }

  if (param$a <= param$b) {
    # the matrix whose lines characterizes every decision makers
    return(matrix(round(stats::runif(n * m, min = param$a, max = param$b)), ncol = m, nrow = n))
  } else {
    print("The support's lower bound should be smaller then the support's upper bound")
  }
}


#' @title gen_continuous_uniform
#'
#' @description A function generating a random matrix with continuous uniform distribution
#'
#' @param n The number rows
#'
#' @param m The number of columns
#'
#' @param a Lower bound of the distribution's support
#'
#' @param b Upper bound of the distribution's support
#'
#' @examples gen_continuous_uniform(10, 2, param = list(a = 2, b = 3))
#' @import stats
#'
#' @export alternatives data, consumers data
#'

gen_continuous_uniform <- function(n, m, param) {
  if (is.null(param$a)) {
    param$a <- 0
  }
  if (is.null(param$b)) {
    param$b <- 1
  }

  if (param$a <= param$b) {
    # the matrix whose lines characterizes every decision makers
    return(matrix(stats::runif(n * m, min = param$a, max = param$b), ncol = m, nrow = n))
  } else {
    print("The support's lower bound should be smaller then the support's upper bound")
  }
}


#' @title gen_beta
#'
#' @description A function generating a random matrix with beta distribution
#'
#' @param n The number rows
#'
#' @param m The number of columns
#'
#' @param shape1 The first shape parameter
#'
#' @param shape2 The second shape parameter
#'
#' @param ncp The non-centrality parameter
#'
#' @examples gen_beta(10, 2, param = list(shape1 = 2, shape2 = 3, ncp = 0.5))
#' @import stats
#'
#' @export alternatives data, consumers data
#'

gen_beta <- function(n, m, param) {
  if (is.null(param$shape1)) {
    param$shape1 <- 0.5
  }
  if (is.null(param$shape2)) {
    param$shape2 <- 0.5
  }
  if (is.null(param$ncp)) {
    param$ncp <- 0
  }

  if (param$shape1 < 0 || param$shape2 < 0) stop("The shape parameters should be positive")
  # the matrix whose lines characterizes every decision makers
  return(matrix(stats::rbeta(n * m, shape1 = param$shape1, shape2 = param$shape2, ncp = param$ncp), ncol = m, nrow = n))
}


#' @title gen_gumbel
#'
#' @description A function generating a random matrix with gumbel distribution
#'
#' @param n The number rows
#'
#' @param m The number of columns
#'
#' @param location The optional location parameter
#'
#' @param scale The optional scale, a positive number
#'
#' @examples gen_gumbel(10, 5, param = list(location = 3))
#' gen_gumbel(10, 5, param = list(location = 3, scale = 4))
#' @export alternatives data, consumers data
#'
#' @import evd

gen_gumbel <- function(n, m, param) {
  if (is.null(param$location)) {
    param$location <- 0
  }
  if (is.null(param$scale)) {
    param$scale <- 1
  }

  if (param$scale <= 0) stop("The scale parameter should be positive")
  return(matrix(evd::rgumbel(n * m, loc = param$location, scale = param$scale), ncol = m, nrow = n))
}


#' @title gen_chi2
#'
#' @description A function generating a random matrix with chi-2 distribution
#'
#' @param n The number rows
#'
#' @param m The number of columns
#'
#' @param df Degree of freedom
#'
#' @param ncp The non-centrality parameter
#'
#' @examples gen_chi2(10, 5, param = list(df = 3, ncp = 1))
#' @export alternatives data, consumers data
#'
#' @import stats

gen_chi2 <- function(n, m, param) {
  if (is.null(param$df)) {
    param$df <- 1
  }
  if (is.null(param$ncp)) {
    param$ncp <- 0
  } # non centrality parameter

  if (param$ncp < 0) stop("The non-centrality parameter should be positive")
  return(matrix(stats::rchisq(n * m, df = param$df, ncp = param$ncp), ncol = m, nrow = n))
}


#' @title gen_poisson
#'
#' @description A function generating a random matrix with Poisson distribution
#'
#' @param n The number rows
#'
#' @param m The number of columns
#'
#' @param lambda rate
#'
#' @examples gen_poisson(10, 5, param = list(lambda = 3))
#' @export alternatives data, consumers data
#'
#' @import stats

gen_poisson <- function(n, m, param) {
  if (is.null(param$lambda)) {
    param$lambda <- 1
  }

  if (param$lambda < 0) stop("The rate should be positive")
  return(matrix(stats::rpois(n * m, lambda = param$lambda), ncol = m, nrow = n))
}


#' @title gen_exp
#'
#' @description A function generating a random matrix with exponential distribution
#'
#' @param n The number rows
#'
#' @param m The number of columns
#'
#' @param lambda Rate
#'
#' @examples gen_exp(10, 5, param = list(lambda = 3))
#' @export alternatives data, consumers data
#'
#' @import stats

gen_exp <- function(n, m, param) {
  if (is.null(param$lambda)) {
    param$lambda <- 1
  }

  if (param$lambda < 0) stop("The rate should be positive")
  return(matrix(stats::rexp(n * m, rate = param$lambda), ncol = m, nrow = n))
}


#' @title gen_geo
#'
#' @description A function generating a random matrix with geometric distribution
#'
#' @param n The number rows
#'
#' @param m The number of columns
#'
#' @param prob Probability of success
#'
#' @examples gen_geo(10, 5, param = list(prob = 0.5))
#' @export alternatives data, consumers data
#'
#' @import stats

gen_geo <- function(n, m, param) {
  if (is.null(param$prob)) {
    param$prob <- 0.5
  }

  if (param$prob < 0 || param$prob > 1) stop("The probability of success should belong to [0; 1]")
  return(matrix(stats::rgeom(n * m, prob = param$prob), ncol = m, nrow = n))
}

#' @title gen_empirical
#'
#' @description A function generating a random matrix from an empirical distribution
#'
#' @param n The number rows
#'
#' @param data Data set from which to extract the empirical distribution
#'
#' @examples gen_empirical(10, param = list(data = data.frame(S1 = c(0.5, 0, 12, 6, 7.3))))
#' @export alternatives data, consumers data
#'
#' @import stats

gen_empirical <- function(n, m = NULL, param) {
  those <- round(stats::runif(n = n, min = 1, max = nrow(param$data)), 0)
  return(param$data[those, ])
}


#' @title information
#'
#' @description A function providing information about how to use the functions gen_
#'
#' @examples information()
#' @export alternatives data, consumers data
#'

information <- function() {
  DF <- data.frame(
    Distributions = c("normal", "student", "discrete_uniform", "continuous_uniform", "beta", "gumbel", "chi2", "poisson", "exp", "empirical", "help"),
    Parameters = c("mu, sd", "location, scale, df", "a, b", "a, b", "shape1, shape2, ncp", "location, scale", "df, ncp", "lambda", "lambda", "data", "None")
  )
  cat("The available distributions and the name of their respective parameters\n")
  return(DF)
}
