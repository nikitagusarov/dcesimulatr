#############################################################
## This file enlists functions which generate experimental designs.
## The notations are taken from Train 2003.
## antoine.dubois.fr@gmail.com - March 2021
#############################################################

##############################
# 1 - Experimental design generation
##############################

#' @title call_design
#'
#' @name call_design
#'
#' @description The function which associates the designs' name to a function which generates it.
#'
#' @param name The name of the chosen experimental design
#'
#' @param A matrix of decision makers data
#'
#' @param B matrix of alternatives data
#'
#' @param U Utility matrix
#'
#' @param choice_set_size the number of alternative per choice set
#'
#' @param J number of alternatives (no_choice included)
#'
#' @param no_choice TRUE FALSE indicator
#'

call_design <- function(name, A, B, U, choice_set_size, J, no_choice, nb_questions){
  if(name=="FuFD"){fullfactorialdesign(A=A, B=B, U=U, choice_set_size=choice_set_size, J=J, no_choice=no_choice)}
  else if(name=="FrFD"){fractionalfactorialdesign(A=A, B=B, U=U, choice_set_size=choice_set_size, J=J, no_choice=no_choice, nb_questions=nb_questions)}
  else{cat("The available experimental designs are FuFD and FrFD")}

}


#' @title fullfactorialdesign
#'
#' @name fullfactorialdesign
#'
#' @description The method of the Experiment class which generate a full factorial design
#'
#' @param A matrix of decision makers data
#'
#' @param B matrix of alternatives data
#'
#' @param U Utility matrix
#'
#' @param choice_set_size the number of alternative per choice set
#'
#' @param J number of alternatives (no_choice included)
#'
#' @param no_choice TRUE FALSE indicator
#'
#' @return Data Frame of a full factorial design
#'

fullfactorialdesign <- function(A, B, U, choice_set_size, J, no_choice){
  p <- ncol(A)
  q <- ncol(B)
  N <- nrow(A)
  DM_att_names <- colnames(A)
  AT_att_names <- colnames(B)
  AT_names <- row.names(B)
  all_combi <- combn(J -no_choice, choice_set_size)
  experimental_design <- data.frame(matrix(ncol = (p + q + 4), nrow = 0))

for(k in 1:N){
  for(i in 1:ncol(all_combi)){
    for(j in 1:nrow(all_combi)){
      conca <- cbind(k, i, AT_names[all_combi[j,i]+ no_choice], A[k,], B[(all_combi[j,i]+no_choice),], U[k,(all_combi[j,i]+no_choice)])
      colnames(conca) <- c()
      conca <- as.matrix(conca, nrow=1)
      experimental_design <- rbind(experimental_design, conca)
    }
  }
}
colnames(experimental_design) <- c("DM_id", "choice_set", "alternative", DM_att_names, AT_att_names, "utility")

chosen <- rep(0, nrow(experimental_design))
for(i in 1:(nrow(experimental_design)/choice_set_size)){
  best_in_choice_set <- which.max(as.numeric(experimental_design$utility[((i-1)*choice_set_size+1):(i*choice_set_size)]))
  if(no_choice){
    id <- experimental_design$DM_id[((i-1)*choice_set_size+1)]
    if(U[id, 1] < max(as.numeric(experimental_design$utility[((i-1)*choice_set_size+1):(i*choice_set_size)]))){
      chosen[(i-1)*choice_set_size + best_in_choice_set] <- 1
    }
  }else{
    chosen[(i-1)*choice_set_size + best_in_choice_set] <- 1
  }
}
experimental_design$choice <- chosen
rownames(experimental_design) <- 1:nrow(experimental_design)
return(experimental_design)
}


#' @title fractionalfactorialdesign
#'
#' @name fractionalfactorialdesign
#'
#' @description The method of the Experiment class which generate a full factorial design
#'
#' @param A matrix of decision makers data
#'
#' @param B matrix of alternatives data
#'
#' @param U Utility matrix
#'
#' @param choice_set_size the number of alternative per choice set
#'
#' @param J number of alternatives (no_choice included)
#'
#' @param no_choice TRUE FALSE indicator
#'
#' @param nb_questions Number of questions to ask to each decision maker
#'
#' @return Data Frame of a random fractional factorial design
#'

fractionalfactorialdesign <- function(A, B, U, choice_set_size, J, no_choice, nb_questions){
  nb_questions <- as.integer(nb_questions)
  experimental_design <- fullfactorialdesign(A=A, B=B, U=U, choice_set_size=choice_set_size, J=J, no_choice=no_choice)
  nb_DM <- max(as.integer(experimental_design$DM_id))
  nb_choice_sets <- max(as.integer(experimental_design$choice_set))
  lines_for_each_id <- choice_set_size*nb_choice_sets
  which_questions <- c()
  for(i in 1:nb_DM){
    questions_for_DM <- sample(1:nb_choice_sets, nb_questions)*choice_set_size
    questions_for_DM <- sort(c(questions_for_DM, questions_for_DM-1))
    which_questions <- c(which_questions, as.integer(questions_for_DM + (i-1)*lines_for_each_id))
  }
  experimental_design <- experimental_design[which_questions,]
  rownames(experimental_design) <- 1:nrow(experimental_design)
  return(experimental_design)
}


#' @title infoDesign
#'
#' @name infoDesign
#'
#' @description A function which provides a summary about an experimental design
#'
#' @param name The name of the chosen design
#'
#' @param experimental_design A generated experimental design
#'
#' @param choice_set_size the number of alternative per choice set
#'
#' @param J number of alternatives (no_choice included)
#'
#' @param no_choice TRUE FALSE indicator
#'
#' @param DM_att_names The names of the decision makers' attributes
#'
#' @param AT_att_names The names of the alternatives' attributes
#'

infoDesign <- function(name, experimental_design, AT_names, choice_set_size, J,
                       no_choice, DM_att_names, AT_att_names){
  infoD <- list(Alternatives_names=unlist(AT_names), choice_set_size=choice_set_size,
                number_of_alternatives=J, no_choice=no_choice,
                Decision_Makers_attributes_names=unlist(DM_att_names),
                Alternatives_attributes_names=unlist(AT_att_names),
                D_criterion=Dcriterion(experimental_design))
  return(infoD)
}

