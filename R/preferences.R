#############################################################
## This file contains the preference coefficients
## The notations are taken from Train 2003.
## antoine.dubois.fr@gmail.com - March 2021
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
#'
#' @examples S <- matrix(rnorm(100), nrow = 20, byrow = TRUE)
#' X <- matrix(rnorm(20), nrow = 4, byrow = TRUE)
#' choice <- c(rep(1,5), rep(2,5), rep(3,5), rep(4,5))
#' estimator <- preference_coef(N=20, p=5, q=5)
#'
#' a <- preference_coef(N=20, p=5, q=5); a$gen(law="student"); a$gen(heterogeneity=TRUE)
#'
#' @export factorial design
#'

preference_coef <- setRefClass("Preference coefficients", fields = list(N="numeric", p="numeric", q="numeric"),
                  methods = list(
                    gen=function(law="normal", n=N, m=p, heterogeneity=FALSE, param=list()){
                      if(heterogeneity){generation(law, n, m, param)}
                      else{generation(law, n=1, m, param)}
                      },
                    fit=function(a=data.matrix(S), b=data.matrix(X), c=choice, model="logit"){
                      if(model=="logit"){
                        inv_log_logit_L <- function(beta){return(-log(logit_L(a, b, c, beta)))}
                      }
                      beta_estim <- optim(fn=inv_log_logit_L, par=rep(1, (p+q)), lower=-Inf, upper=Inf)$par
                      return(matrix(beta_estim, nrow=1))
                      }
                ))
