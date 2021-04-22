#############################################################
## This file characterizes and computes the utility function of a decision maker.
## The notations are taken from Train 2003.
## antoine.dubois.fr@gmail.com - March 2021
#############################################################

##############################
# 0 - Install missing libraries
##############################
# list.of.packages <- c("mvtnorm", "matrixcalc", "evd", "scatterplot3d")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# rm(new.packages, list.of.packages)

##############################
# 1 - Source files and libraries
##############################
library("scatterplot3d")

##############################
# 1 - Utility function
##############################


#' @title FactorialDesign
#'
#' @description The reference class whose methods generate a Factorial Design
#'
#' @param N The number of decision makers
#'
#' @param p The number of attributes of each decision makers
#'
#' @param J The number of alternatives
#'
#' @param q The number of attributes of each alternatives
#'
#' @examples DM_att_names <- list("A", "B", "C"); AT_att_names <- list("D", "E", "F")
#'           AT_names <- list("good1", "good2"); N <- 10
#'           FD <- FactorialDesign(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,N=N)
#'

FactorialDesign <- setRefClass("Factorial Design", fields = list(
  no_choice="logical",
  DM_att_names="list", AT_att_names="list", AT_names="list", N="numeric", p="numeric",
  J="numeric", q="numeric", S="data.frame", X="data.frame", beta="data.frame", beta_hat="data.frame",
  Epsilon="data.frame", V="data.frame", U="data.frame", choice_order="data.frame", choice="vector")
)


FactorialDesign$methods(
  initialize = function(AT_names, DM_att_names, AT_att_names, N, no_choice=FALSE){
    .self$no_choice <<- no_choice
    if(.self$no_choice){.self$AT_names <<- append("no_choice", AT_names)}
    else{.self$AT_names <<- AT_names}
    .self$DM_att_names <<- DM_att_names
    .self$AT_att_names <<- AT_att_names
    .self$N <<- N
    .self$p <<- length(.self$DM_att_names)
    .self$J <<- length(.self$AT_names)
    .self$q <<- length(.self$AT_att_names)

    S <<- data.frame(matrix(NA, nrow=.self$N, ncol=.self$p)); colnames(S) <<- .self$DM_att_names
    X <<- data.frame(matrix(NA, nrow=.self$J, ncol=.self$q)); colnames(X) <<- .self$AT_att_names; row.names(X) <<- .self$AT_names
    beta <<- data.frame(matrix(NA, nrow=N, ncol=p+q)); colnames(beta) <<- c(.self$DM_att_names, .self$AT_att_names)
    beta_hat <<- data.frame(matrix(NA, nrow=N, ncol=p+q)); colnames(beta_hat) <<- c(.self$DM_att_names, .self$AT_att_names)

    Epsilon <<- data.frame(matrix(NA, nrow=.self$N, ncol=.self$J)); colnames(Epsilon) <<- .self$AT_names
    V <<- data.frame(matrix(NA, nrow = .self$N, ncol = .self$J)); colnames(V) <<- .self$AT_names
    U <<- data.frame(matrix(NA, nrow = .self$N, ncol = .self$J)); colnames(U) <<- .self$AT_names
  }
)


#' @title gen_DM_attributes
#'
#' @description The method of the Factorial design class which generates some decision makers' attributes
#'
#' @param law The name of the distribution generating the attributes
#'
#' @param which The number of the columns which should be generated
#'
#' @optional_parameter characteristics of the chosen distribution
#'
#' @examples N <- 10; AT_names <- list("good1", "good2"); DM_att_names <- list("A", "B", "C"); AT_att_names <- list("D", "E", "F")
#'           FD <- FactorialDesign(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,N=N)
#'           FD$gen_DM_attributes("discrete_uniform", b=5, which=c(1,3))
#'           FD$gen_DM_attributes("normal", which=c(2,4,5)); FD$S
#'

FactorialDesign$methods(
  gen_DM_attributes = function(law="normal", which=c(1:.self$p), ...){
    param <- list(...)
    ob_DM_att <- ob_decision_makers_att(N=.self$N, p=.self$p)
    S[, which] <<- ob_DM_att$gen(law=law, m=length(which), param=param)

  if(any(is.na(S))) {warning("There remain some NA values in the decision makers' attributes matrix", call. = FALSE)}}
)


#' @title gen_AT_attributes
#'
#' @description The method of the Factorial design class which generates some alternatives' attributes
#'
#' @param law The name of the distribution generating the attributes
#'
#' @param which The number of the columns which should be generated
#'
#' @optional_parameter characteristics of the chosen distribution
#'
#' @examples N <- 10; AT_names <- list("good1", "good2"); DM_att_names <- list("A", "B", "C"); AT_att_names <- list("D", "E", "F")
#'           FD <- FactorialDesign(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,N=N)
#'           FD$gen_AT_attributes("discrete_uniform", b=2, which=c(1,3))
#'           FD$gen_AT_attributes("normal", which=c(2,4)); FD$X
#'

FactorialDesign$methods(
  gen_AT_attributes = function(law="normal", which=c(1:.self$q), ...){
    param <- list(...)
    ob_AT_att <- ob_alternatives_att(J=.self$J, q=.self$q)
    X[, which] <<- ob_AT_att$gen(law=law, m=length(which), param=param)
    X[1, ] <<- 0

    if(any(is.na(X))) {warning("There remain some NA values in the decision makers' attributes matrix", call. = FALSE)}}
)


#' @title gen_preference_coefficients
#'
#' @description The method of the Factorial design class which generates some preference coefficients' attributes
#'
#' @param which The number of the columns which should be generated
#'
#' @optional_parameter distribution The distribution which generates the chosen columns.
#'
#' @examples N <- 10; AT_names <- list("good1", "good2"); DM_att_names <- list("A", "B", "C"); AT_att_names <- list("D", "E", "F")
#'           FD <- FactorialDesign(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,N=N)
#'           FD$gen_preference_coefficients("student", heterogeneity=TRUE, location=rep(-100,5),  scale=diag(1,5), which=c(1:5))
#'           FD$gen_preference_coefficients("normal", heterogeneity=TRUE, mu=rep(0,3),  sd=diag(1,3), which=c(6:8))
#'           FD$gen_preference_coefficients("discrete_uniform", heterogeneity=TRUE, a=1, b=5, which=c(9))
#'           FD$beta
#'

FactorialDesign$methods(
  gen_preference_coefficients = function(law="normal", heterogeneity=FALSE, which=c(1:(.self$p+.self$q)), ...){
    param <- list(...)
    pref_coef <- preference_coef(N=.self$N, p=.self$p, q=.self$q)
    beta[, which] <<- pref_coef$gen(law=law, m=length(which), heterogeneity=heterogeneity, param=param)

    if(any(is.na(beta))) {warning("There remain some NA values in the decision makers' attributes matrix", call. = FALSE)}}
)


#' @title representative_utility
#'
#' @description The method of the Factorial design class which computes the representative utility
#'
#' @param funct By default, the representative utility function is the sum of the alternatives of
#' a decision maker and the alternatives of an alternative weighted by the preference coefficients.
#' However, the user can choose whatever function with whatever coefficients
#'
#' @examples N <- 10; AT_names <- list("good1", "good2"); DM_att_names <- list("A", "B", "C"); AT_att_names <- list("D", "E", "F")
#'           FD <- FactorialDesign(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,N=N)
#'           FD$representative_utility(); FD$V
#'

FactorialDesign$methods(representative_utility = function(func=NULL){

  beta2 <- data.matrix(beta)
  S2 <- data.matrix(S)
  X2 <- data.matrix(X)

  if(is.null(func)){
      func <- function(s, x, i){
        return(c(s,x)%*% matrix(beta2[i,], ncol = 1))
      }
    if(nrow(beta)==1){
      for(j in 1:.self$J){for( i in 1:.self$N){V[i,j] <<- func(S2[i,], X2[j,], 1)}}
    }else{
      for(j in 1:.self$J){for( i in 1:.self$N){V[i,j] <<- func(S2[i,], X2[j,], i)}}
    }
  }else{
    for(j in 1:.self$J){for( i in 1:.self$N){V[i,j] <<- func(S2[i,], X2[j,])}}
  }
}
)


#' @title total_utility
#'
#' @description The method of the Factorial design class which generates the utility of the agents for each good
#'
#' @optional_parameter distribution The distribution which generates the perturbations
#'
#' @examples N <- 10; AT_names <- list("good1", "good2"); DM_att_names <- list("A", "B", "C"); AT_att_names <- list("D", "E", "F")
#'           FD <- FactorialDesign(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,N=N)
#'           FD$gen_AT_attributes(); FD$gen_DM_attributes(); FD$gen_preference_coefficients();
#'           FD$representative_utility()
#'           FD$total_utility(); FD$Epsilon; FD$U; FD$choice_order
#'           FD$total_utility("student", mu=3, df=4); FD$total_utility("discrete_uniform"); FD$Epsilon; FD$U; FD$choice_order
#'           FD$Epsilon; FD$U; FD$choice_order
#'

FactorialDesign$methods(
  total_utility = function(law="gumbel", ...){
    param <- list(...)
    perturbation <- noise(N=.self$N, J=.self$J)
    Epsilon <<- data.frame(perturbation$gen(law=law, param=param))
    colnames(Epsilon) <<- .self$AT_names

    U <<- V + Epsilon
    sort_index_decr <- function(x){
      return(sort(x, decreasing=TRUE, index.return=TRUE)$ix)}

    choice_order <<- data.frame(t(apply(U, 1, sort_index_decr)))
    colnames(choice_order) <<- .self$AT_names
  }
)


#' @title best_choice
#'
#' @description The method of the Factorial design class which computes the optimal decision makers' choice
#'
#' @examples

FactorialDesign$methods(best_choice = function(){
  choice <<- apply(FD$choice_order, 1, which.min)
}
)


#' @title map
#'
#' @description The method of the Factorial design class which draws a preferenc mapping
#'
#' @examples

FactorialDesign$methods(map = function(dim1, dim2){
  dim1_id <- which(colnames(X)==dim1)
  A <- X
  dim1_in_x <- TRUE
  if(length(dim1_id)==0){dim1_id <- which(colnames(S)==dim1); A <- S; dim1_in_x <- FALSE}
  if(length(dim1_id)==0){stop("dim1 unknown attribute")}

  dim2_id <- which(colnames(X)==dim2)
  B <- X
  dim2_in_x <- TRUE
  if(length(dim2_id)==0){dim2_id <- which(colnames(S)==dim2); B <- S; dim2_in_x <- FALSE}
  if(length(dim2_id)==0){stop("dim2 unknown attribute")}

  colors_list <- brewer.pal(n = .self$J, name = "Set1")
  mat <- c()
  colors <- c()
  for(i in 1:.self$J){
    if(dim1_in_x){x1 <- rep(A[i, dim1_id], .self$N)}
    else{x1 <- A[, dim1_id]}
    if(dim2_in_x){x2 <- rep(B[i, dim2_id], .self$N)}
    else{x2 <- B[, dim2_id]}
    mat <- rbind(mat, cbind(x1, x2, U[, i]))
    colors <- c(colors, rep(colors_list[i], .self$N))
  }
  mat <- data.frame(mat)
  colnames(mat) <- c(dim1, dim2, "Utility")
  s3d <- scatterplot3d(mat, color=colors,  pch = 16)
  legend("bottom", legend = .self$AT_names, inset = -0.25, xpd = TRUE, horiz = TRUE,
         col =  colors_list, pch = 16)
}
)


#' @title factorial_design
#'
#' @description The method of the Factorial design class which generates a factorial design with default distributions
#'
#' @examples N <- 10; AT_names <- list("good1", "good2"); DM_att_names <- list("A", "B", "C"); AT_att_names <- list("D", "E", "F")
#'           FD <- FactorialDesign(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,N=N)
#'           FD$factorial_design()
#'           FD$U; FD$choice_order
#'

FactorialDesign$methods(factorial_design = function(){
  gen_DM_attributes()
  gen_AT_attributes()
  gen_preference_coefficients()
  representative_utility()
  total_utility()
})
