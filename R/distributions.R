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
#' @description A function looking for the queried distribution
#'
#' @param n The number rows
#'
#' @param m The number of columns
#'
#' @param law The distribution generating the matrix
#'
#' @examples generation("student", 10, 5, param=list(location=rep(100, 5))); generation("help")
#'
#' @export alternatives data, consumers data
#'

generation <- function(law, n, m, param){
  if(law=="normal"){gen_normal(n, m, param)}
  else if(law=="student"){gen_student(n, m, param)}
  else if(law=="discrete_uniform"){gen_discrete_uniform(n, m, param)}
  else if(law=="gumbel"){gen_gumbel(n, m, param)}
  else if(law=="help"){information()}
  else {cat("The available distributions are 'normal', 'student', 'discrete_uniform' and 'gumbel'")}
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
#' @examples gen_normal(10, 2, param=list(mu=rep(-10,2)))
#'
#' @export alternatives data, consumers data
#'
#' @import matrixcalc
#' @import mvtnorm

gen_normal <- function(n, m, param){
  if(is.null(param$mu)){param$mu <- rep(0, m)}
  else if(length(param$mu) == 1){param$mu <- rep(param$mu, m)}
  else if(m != length(param$mu)){stop("Unconsistent dimentions of the mean vector, number of attributes ", m , " but declared ", length(param$mu))}

  if(is.null(param$sd)){param$sd <- diag(1, m)}
  else if(length(param$sd) == 1){param$sd <- diag(param$sd, m)}
  else if(m != nrow(param$sd)){stop("Unconsistent dimentions of the covariance matrix, number of attributes ", m , " but declared ", nrow(param$sd))}

  if(matrixcalc::is.positive.definite(param$sd) & matrixcalc::is.symmetric.matrix(param$sd)){
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
#' @examples gen_student(10, 2, param=list(scale=diag(3,2), df=5))
#'
#' @export alternatives data, consumers data
#'
#' @import matrixcalc
#' @import mvtnorm

gen_student <- function(n, m, param){
  if(is.null(param$location)){param$location <- rep(0, m)}
  else if(length(param$location) == 1){param$location <- rep(param$location, m)}
  else if(m != length(param$location)){stop("Unconsistent dimentions of the mean vector, number of attributes ", m , " but declared ", length(param$mu))}

  if(is.null(param$scale)){param$scale <- diag(1, m)}
  else if(length(param$scale) == 1){param$scale <- diag(param$scale, m)}
  else if(m != nrow(param$scale)){stop("Unconsistent dimentions of the covariance matrix, number of attributes ", m , " but declared ", nrow(param$sd))}

  if(is.null(param$df)){param$df <- 2}

  if(matrixcalc::is.positive.definite(param$scale) & matrixcalc::is.symmetric.matrix(param$scale)){
    #the matrix whose lines characterizes every decision makers
    return(mvtnorm::rmvt(n = n, delta=as.vector(param$location), sigma=param$scale, df=param$df))
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
#' @examples gen_discrete_uniform(10, 2, param=list(a=2, b=3))
#'
#' @export alternatives data, consumers data
#'

gen_discrete_uniform <- function(n, m, param){
  if(is.null(param$a)){param$a <- 0}
  if(is.null(param$b)){param$b <- 1}

  if(param$a <= param$b){
    #the matrix whose lines characterizes every decision makers
    return(matrix(round(runif(n*m, min=param$a, max=param$b)), ncol=m, nrow=n))
  } else {
    print("The support's lower bound should be smaller then the support's upper bound")
  }
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
#' @examples gen_gumbel(10, 5, param=list(location=3)); gen_gumbel(10, 5, param=list(location=3, scale=4))
#'
#' @export alternatives data, consumers data
#'
#' @import evd

gen_gumbel <- function(n, m, param){
  if(is.null(param$location)){param$location <- 0}
  if(is.null(param$scale)){param$scale <- 1}

  if(param$scale <= 0) stop("The scale parameter should be positive")
  return(matrix(evd::rgumbel(n*m, loc=param$location, scale=param$scale), ncol=m, nrow=n))
}

#' @title information
#'
#' @description A function providing information about how to use the functions gen_
#'
#' @examples information()
#'
#' @export alternatives data, consumers data
#'

information <- function(){
  DF <- data.frame(Distributions=c("normal", "student", "discrete_uniform", "gumbel", "help"),
                   Parameters=c("mu, sd", "location, scale, df", "a, b", "location, scale", "None"))
  cat("The available distributions and the name of their respective parameters\n")
  DF
}
