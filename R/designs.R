#############################################################
## This file enlists functions which generate experimental designs.
## The notations are taken from Train 2003.
## antoine.dubois.fr@gmail.com - March 2021
## nikita.gusarov@univ-grenoble-alpes.fr - February 2022
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
#' @param X matrix of decision makers data
#'
#' @param Z matrix of alternatives data
#'
#' @param U Utility matrix
#'
#' @param choice_set_size the number of alternative per choice set
#'
#' @param J number of alternatives (no_choice included)
#'
#' @param no_choice TRUE FALSE indicator
#'

call_design <- function(
  name, X, Z, U, choice_set_size, 
  J, no_choice, nb_questions, 
  format = "long"
) {
  if (name == "FuFD") {
    fullfactorialdesign(X = X, Z = Z, U = U, choice_set_size = choice_set_size, J = J, no_choice = no_choice, format = format)
  } else if (name == "FrFD") {
    fractionalfactorialdesign(X = X, Z = Z, U = U, choice_set_size = choice_set_size, J = J, no_choice = no_choice, nb_questions = nb_questions, format = format)
  } else {
    cat("The available experimental designs are FuFD and FrFD")
  }
}


#' @title fullfactorialdesign
#'
#' @name fullfactorialdesign
#'
#' @description The method of the Experiment class which generate a full factorial design
#'
#' @param X matrix of decision makers data
#'
#' @param Z matrix of alternatives data
#'
#' @param U Utility matrix
#'
#' @param choice_set_size the number of alternative per choice set
#'
#' @param J number of alternatives (no_choice included)
#'
#' @param no_choice TRUE FALSE indicator
#'

fullfactorialdesign <- function(X, Z, U, choice_set_size, J, no_choice, format = "long") {
  p <- ncol(X)
  q <- ncol(Z)
  N <- nrow(X)
  DM_att_names <- colnames(X)
  AT_att_names <- colnames(Z)
  AT_names <- row.names(Z)
  all_combi <- combn(J - no_choice, choice_set_size)

  if (format == "long") { # build in long format
    experimental_design <- data.frame(matrix(ncol = (4 + p + q), nrow = 0))

    for (k in 1:N) {
      for (i in 1:ncol(all_combi)) {
        for (j in 1:nrow(all_combi)) {
          experimental_design <- rbind(experimental_design, unlist(c(
            k, i, X[k, ],
            AT_names[all_combi[j, i] + no_choice], Z[(all_combi[j, i] + no_choice), ], U[k, (all_combi[j, i] + no_choice)]
          )))
        }
      }
    }
    colnames(experimental_design) <- c("DM_id", "choice_set", DM_att_names, "alternative", AT_att_names, "utility")
    experimental_design$utility <- as.numeric(experimental_design$utility)

    # compute decision makers' choice
    nb_questions <- ncol(all_combi) * N
    experimental_design$choice <- rep(0, nrow(experimental_design))
    for (k in 1:nb_questions) {
      if (no_choice) {
        U <- as.numeric(c(0, experimental_design$utility[c(((k - 1) * (choice_set_size) + 1):(k * choice_set_size))])) # By definition, the level of utility brought by choosing no alternative is equal to 0
        which_max <- which.max(U)
        if (which_max != 1) {
          experimental_design$choice[((k - 1) * (choice_set_size) + which_max - 1)] <- 1
        }
      } else {
        U <- experimental_design$utility[c(((k - 1) * (choice_set_size) + 1):(k * choice_set_size))]
        which_max <- which.max(U)
        experimental_design$choice[((k - 1) * (choice_set_size) + which_max)] <- 1
      }
    }
  } else if (format == "wide") { # build in wide format
    experimental_design <- data.frame(matrix(ncol = (3 + p + (q + 1) * choice_set_size), nrow = 0))
    for (k in 1:N) {
      for (i in 1:ncol(all_combi)) {
        conca <- c(k, i, X[k, ])
        for (j in 1:nrow(all_combi)) {
          conca <- unlist(c(conca, AT_names[(all_combi[j, i] + no_choice)], Z[(all_combi[j, i] + no_choice), ], U[k, (all_combi[j, i] + no_choice)]))
        }
        experimental_design <- rbind(experimental_design, conca)
      }
    }
    colnames(experimental_design)[1:(length(DM_att_names) + 2)] <- c("DM_id", "choice_set", DM_att_names)
    col_names <- c()
    for (i in 1:nrow(all_combi)) {
      col_names <- cbind(col_names, paste(c("alternative", AT_att_names, "utility"), i, sep = "."))
    }
    colnames(experimental_design)[(length(DM_att_names) + 3):ncol(experimental_design)] <- col_names

    for_max_utility <- data.frame(matrix(nrow = nrow(experimental_design), ncol = 0))
    for (i in 1:nrow(all_combi)) {
      for_max_utility <- cbind(for_max_utility, experimental_design[paste("utility", i, sep = ".")])
    }

    # compute decision makers' choice
    if (no_choice) {
      for_max_utility <- cbind(0, for_max_utility)
      experimental_design$choice <- apply(for_max_utility, 1, which.max) - 1
    } else {
      experimental_design$choice <- apply(for_max_utility, 1, which.max)
    }
  }

  return(experimental_design)
}


#' @title fractionalfactorialdesign
#'
#' @name fractionalfactorialdesign
#'
#' @description The method of the Experiment class which generate a full factorial design
#'
#' @param X matrix of decision makers data
#'
#' @param Z matrix of alternatives data
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

fractionalfactorialdesign <- function(X, Z, U, choice_set_size, J, no_choice, nb_questions, format = "long") {
  nb_questions <- as.integer(nb_questions)
  experimental_design <- fullfactorialdesign(X = X, Z = Z, U = U, choice_set_size = choice_set_size, J = J, no_choice = no_choice, format = format)
  nb_DM <- max(as.integer(experimental_design$DM_id))
  nb_choice_sets <- max(as.integer(experimental_design$choice_set))
  if (format == "long") {
    questions_for_DM <- c()
    for (i in 1:nb_DM) {
      questions <- sort(sample(1:nb_choice_sets, nb_questions))
      for (j in 1:nb_questions) {
        question <- c(((questions[j] - 1) * choice_set_size + 1):(questions[j] * choice_set_size))
        questions_for_DM <- c(questions_for_DM, question + (i - 1) * choice_set_size * nb_choice_sets)
      }
    }
  } else if (format == "wide") {
    questions_for_DM <- c()
    for (i in 1:nb_DM) {
      questions_for_DM <- c(questions_for_DM, sort(sample(1:nb_choice_sets, nb_questions)) + (i - 1) * nb_choice_sets)
    }
  }
  experimental_design <- experimental_design[questions_for_DM, ]
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

infoDesign <- function(name, experimental_design_long, experimental_design_wide, AT_names, choice_set_size, J,
                       no_choice, DM_att_names, AT_att_names, beta) {
  optimal_preference_parameter <- fit(experimental_design = experimental_design_long, choice_set_size = choice_set_size)
  infoD <- list(
    Alternatives_names = unlist(AT_names), choice_set_size = choice_set_size,
    number_of_alternatives = J, no_choice = no_choice,
    Decision_Makers_attributes_names = unlist(DM_att_names),
    Alternatives_attributes_names = unlist(AT_att_names),
    D_score = Dcriterion(experimental_design_wide, DM_att_names, AT_att_names, choice_set_size),
    beta_value = optimal_preference_parameter$value,
    beta_hat = optimal_preference_parameter$solution,
    mean_real_beta = apply(data.matrix(beta), 2, mean)
  )
  return(infoD)
}
