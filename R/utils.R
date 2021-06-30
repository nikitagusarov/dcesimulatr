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

InfoMatrix <- function(experimental_design){
  B <- experimental_design[,4:(ncol(experimental_design)-2)]
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

Dcriterion <- function(experimental_design){
  M <- InfoMatrix(experimental_design)
  return(det(M))
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

summary.Exepriment <- function(FD){
  cat("Alternatives' names:", unlist(FD$AT_names), "\n")
  cat("Attributes' alternatives' names:", unlist(FD$info$Alternatives_attributes_names), "\n")
  if(length(FD$groups)==1){
    cat("Number of Decision Makers:", FD$groups, "\n")
  }else{
    cat("Groups of Decision makers:", FD$groups, "\n")
  }
  cat("Decision Makers' characteristics' names:", unlist(FD$info$Decision_Makers_attributes_names), "\n")
  cat("Choice set size:", FD$info$choice_set_size, "\n")
  cat("D-score:", FD$info$D_criterion)
}


#' @title long_format
#'
#' @name long_format
#'
#' @description This function transforms a wide format experimental design into a long format design
#'
#' @param design_expe An experimental design of format wide
#'
#' @return The long format of a wide format experimental design
#'

long_format <- function(design_expe, characterisnics_names){ # to work on that
  design_expe[c(1, 2, 4:7, length(design_expe))]
  long <- reshape(design_expe, direction = "wide", idvar = c("DM_id", characterisnics_names),
                  timevar = c("choice_set"), v.names = c("choice"))
  return(long)
}

