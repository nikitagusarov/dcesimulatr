#############################################################
## This file provides some useful functions
## The notations are taken from Train 2003.
## antoine.dubois.fr@gmail.com - March 2021
#############################################################

##############################
# 1 - Functions
##############################

#' @title normalization
#'
#' @name normalization
#'
#' @description The function which normalizes the representative utility
#'
#' @param V The matrix of representative utilities
#'
#' @param U The matrix of the utilities
#'
#' @examples V <- matrix(rnorm(30), 10, 3)
#' U <- matrix(rnorm(30), 10, 3) + matrix(rnorm(30), 10, 3)
#' normalization(V, U)
#'

normalization <- function(V, U){
  for(i in 1:nrow(U))
    V[i,] <- V[i,]/sum(U[i,])
  return(U)
}


#' @title categorization
#'
#' @name categorization
#'
#' @description The function which classify by k-means the values of each column
#'  of X. Then, the values of each column is set to the center of its cluster.
#'
#' @param X The matrix data
#'
#' @param np_levels The vector of the number of clusters to build in each column.
#'
#' @return $clustered The cluster's average of each attribute of each alternative
#' $category The cluster's number of each attribute of each alternative
#'
#' @export experiment
#'
#' @import stats
#'
#' @examples X <- matrix(rnorm(30), ncol=3)
#' nb_levels <- c(3, 2, 2)
#' X_clustered <- categorization(X, nb_levels)$clustered
#' X_category <- categorization(X, nb_levels)$category
#' X_clustered; X_category

categorization <- function(X, nb_levels){
  if(is.null(ncol(X))){X <- matrix(X, ncol=1, nrow=length(X))}
  if(nrow(X)==2){
    sort_index_inc <- function(x){
      return(sort(x, decreasing=FALSE, index.return=TRUE)$ix)}
    X <- X
    X_cat <- apply(X, 2, sort_index_inc)
  } else{
  X_cat <- matrix(0, nrow(X), ncol(X))
  for(i in 1:ncol(X)){
    if(length(unique(X[,i])) < nb_levels[i]){
      warning("The number of levels of attributes ", i, " is inferior to the number of different attributes")
      nb_levels[i] <- length(unique(X[,i]))
    }
    model <- stats::kmeans( X[,i], nb_levels[i])
    centers <- model$centers
    cluster <- model$cluster
    X_cat[, i] <- cluster
    for(j in 1:nrow(X)){
      X[j, i] <- centers[cluster[j]]
    }
  }
  }
  return(list(clustered=X, category=X_cat))
}


#' @title InfoMatrix
#'
#' @name InfoMatrix
#'
#' @description The function which computes the information matrix.
#'
#' @param experimental_design The table representation of an experimental design
#'

InfoMatrix <- function(experimental_design, DM_att_names, AT_att_names, choice_set_size){
  att_names <- c()
  for(i in 1:choice_set_size){
    att_names <- c(att_names, paste(AT_att_names, i, sep="."))
  }
  B <- experimental_design[c(DM_att_names, att_names)]
  info_matrix <- matrix(0, nrow = ncol(B), ncol = ncol(B))
  for(i in 1:nrow(B)){
    info_matrix <- info_matrix + matrix(as.numeric(B[i,]), ncol=1) %*% matrix(as.numeric(B[i,]), nrow=1)/nrow(experimental_design)
  }
  return(info_matrix)
}


#' @title Dcriterion
#'
#' @name Dcriterion
#'
#' @description The function which computes the determinant of the information matrix.
#'
#' @param experimental_design The design matrix
#'
#' @return The D-score of the experimental design
#'
#' @return Data Frame of a full factorial design
#'
#' @import MASS
#'

Dcriterion <- function(experimental_design, DM_att_names, AT_att_names, choice_set_size){
  M <- InfoMatrix(experimental_design, DM_att_names, AT_att_names, choice_set_size)
  return(round(det(MASS::ginv(M)), digit=5))
}


#' @title summary
#'
#' @name summary
#'
#' @description This function returns a summary of the class Experiment
#'
#' @param FD An instance of the class Experiment
#'
#' @return Some information about an instance of the class Experiment
#'

# summary.Exepriment <- function(FD){
#   cat("Alternatives' names:", unlist(FD$AT_names), "\n")
#   cat("Attributes' alternatives' names:", unlist(FD$info$Alternatives_attributes_names), "\n")
#   if(length(FD$groups)==1){
#     cat("Number of Decision Makers:", FD$groups, "\n")
#   }else{
#     cat("Groups of Decision makers:", FD$groups, "\n")
#   }
#   cat("Decision Makers' characteristics' names:", unlist(FD$info$Decision_Makers_attributes_names))
#   #cat("Choice set size:", FD$info$choice_set_size, "\n")
#   #cat("D-score:", FD$info$D_score, "\n")
#   #cat("real beta: \n")
#   #print(FD$info$mean_real_beta)
#   #cat("estimated beta: \n")
#   #print(FD$info$beta_hat)
# }


#' @title fit
#'
#' @name fit
#'
#' @description This function gives the best estimate of the preference coefficients
#'
#' @param experimental_design The experimental design tableau (should be of format long)
#'
#' @param choice_set_size The size of each choice set
#'
#' @return The value of the loss function at its optimal parameter as well as the value of this optimal parameter
#'
#' @import optimx


loss <- function(experimental_design, choice_set_size){
  conditional_loss <- function(beta){
    X <- experimental_design[setdiff(colnames(experimental_design), c("utility", "DM_id", "choice_set", "alternative", "choice") )]
    X <- data.matrix(X)
    Y <- X %*% beta

    weight <- c()
    nb_questions <- nrow(experimental_design)/(choice_set_size)
    for(i in 1:nb_questions){
      weight <- c(weight, sum(Y[c(((i-1)*choice_set_size +1): (i*choice_set_size))]))
    }
    weight <- rep(weight, rep(choice_set_size, length(weight)))
    proba <- log(Y/weight)
    proba_chosen <- proba * experimental_design$choice
    loss <- sum(proba_chosen)
    return(loss)
  }
  return(conditional_loss)
}


fit <- function(experimental_design, choice_set_size){
  conditionnal_loss <- loss(experimental_design = experimental_design, choice_set_size = choice_set_size)
  nb_param <- length(setdiff(colnames(experimental_design), c("utility", "DM_id", "choice_set", "alternative", "choice")))
  solution <- optimx::optimx(par = rep(1, nb_param), fn=conditionnal_loss, method = "Nelder-Mead")
  value <- solution[(nb_param+1)]
  solution <- solution[c(1:nb_param)]
  colnames(solution) <- setdiff(colnames(experimental_design), c("utility", "DM_id", "choice_set", "alternative", "choice"))
  return(list(solution=solution, value=value))
}
