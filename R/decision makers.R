#############################################################
## This file enlists functions which generate decision makers' observed
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
#' @examples N <- 10; p <- 3; a <- ob_decision_makers_att$new(N=N, p=p);
#'           a$gen("student", param=list(location=rep(100, 3), df=3)); a$gen(); a$gen("help")
#'
#' @export factorial design
#'


ob_decision_makers_att <- setRefClass("Decision makers' attributes", fields = list(N="numeric", p="numeric"),
                                      methods = list(gen=function(law="normal", n=.self$N, m=.self$p, param=list()){generation(law, n, m, param)}))
