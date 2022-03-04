####################################################################
## This file enlists functions which declare experimental designs ##
####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

#############################################
# 1. Helper functions to be used in designs #
#############################################

# Guessing classes of provided attribute definitions

#' @title Classify generating laws
#' @description This is an internal function which classifies the attribute / characteristic generating functions.
#' The function serves to distinguish between the different input formats.
#' This ensures flexibility of the attributes' specifications.
#'
#' @param laws A list of generation rules (rules) as provided by single alternative.
#' @return character A vector of classes in character format.
#'
#' @examples
#' # Create alternative
#' alt1 <- alternative$new()
#' alt1$add_attributes(Quality = runif(min = 0, max = 1), Price = rnorm(mean = 5))
#' 
#' # Class laws
#' claws <- class_laws(alt1$get_laws())
#' 
#' @export

class_laws <- function(laws) {
  # We avoid verification
  # the function is internal to other

  # Classify laws defaulting to "rand"
  laws_j <- rep("rand", length(laws))
  # Check other eventual types
  for (j in seq_along(laws)) {
    # Vectors introduced with "c"
    if (laws[[j]][[1]] == expr(c)) {
      laws_j[j] <- "c"
    }
    # Sample type objects
    if (laws[[j]][[1]] == expr(sample)) {
      # Sampling from given sets
      laws_j[j] <- "sample"
    }
  }

  # Return
  return(laws_j)
}

# Checking attributes of Z aagainst full set

#' @title Attributes check
#' @description Verification that all desired characteristics are present in $Z$.
#' This is an internal function which serves to simplify concatenation of multiple $Z$ when alternatives have different attributes.
#'
#' @param Z Input data.frame Z
#' @param attributes Desired vector of attributes to be present in Z.
#' @return Z With missing attributes filled with NA's.
#'
#' @examples
#' # Create alternatives
#' alt1 <- alternative$new()
#' alt1$add_attributes(Quality = runif(min = 0, max = 1), Price = rnorm(mean = 5))
#' alt2 <- alternative$new()
#' alt2$add_attributes(Size = runif(min = 0, max = 1), Price = rnorm(mean = 6))
#'
#' # Regroup alternatives into design
#' edesign <- experimental_design$new(alternatives = list(alt1, alt2))
#' Z <- random_design(edesign, n = 1)
#' 
#' # Querry object
#' attr <- edesign$get_attributes()
#' Z <- check_attributes(Z, attr)
#' 
#' @export

check_attributes <- function(Z, attributes) {
  # Get dimensions
  m <- nrow(Z)

  # Check completenes
  if (
    !rlang::is_empty(
      adattr <- setdiff(attributes, colnames(Z))
    )
  ) {
    Z[adattr] <- rep(NA, m)
  }

  # Output
  return(Z)
}

# Wrapper for laws

#' @title Index laws
#' @description Index laws list according to desired class of law.
#' This is an internal function which searches the laws of desired class in the lsit of laws.
#' The classes should be obtained separately with a `class_laws` function.
#'
#' @param experimental_design Input object to test
#' @param type Type (class) of the laws to be searched for.
#' Multiple classes may be specified at the same time.
#' This is an internal function which simplifies the treatment of different data generation procedures.
#' Default value is "rand".
#' @return Logic
#'
#' @examples
#' # Create alternatives
#' alt1 <- alternative$new()
#' alt1$add_attributes(Quality = runif(min = 0, max = 1), Price = rnorm(mean = 5))
#' alt2 <- alternative$new()
#' alt2$add_attributes(Size = c(0, 1), Price = rnorm(mean = 6))
#'
#' # Regroup alternatives into design
#' edesign <- experimental_design$new(alternatives = list(alt1, alt2))
#' 
#' # Get index list
#' index <- laws_index(edesign, type = c("rand"))
#' 
#' @export

laws_index <- function(experimental_design, type = "rand") {
  # Index dummy
  index <- list()

  # Assign classes to laws
  for (i in seq_along(experimental_design$alternatives)) {
    # Get laws
    laws <- experimental_design$alternatives[[i]]$get_laws()
    # Write class index
    index[[i]] <- class_laws(laws)
  }

  # Check condition
  index <- lapply(
    index,
    function(x) which(x %in% type)
  )

  # Return index
  return(index)
}

# Index Z data.frame

#' @title Index Z data
#' @description Index Z experimental design data.
#' This function adds alternative id (AID) and / or choice set id (CID).
#' This is an internal function which simplifies indexation of data during generation procedures with complex designs.
#'
#' @param Z Input data.frame object to be indexed
#' @param alt_id Alternative id to be assigned to whole dataset.
#' @param type "AID" or "CID".
#' Specify the desired index to be added to the dataset.
#' By default both "AID" and "CID" are added.
#' This is an internal function which is used in data generation procedures.
#' @return data.frame Indexed dataset Z.
#'
#' @examples
#' # Create alternatives
#' alt1 <- alternative$new()
#' alt1$add_attributes(Quality = runif(min = 0, max = 1), Price = rnorm(mean = 5))
#' alt2 <- alternative$new()
#' alt2$add_attributes(Size = runif(min = 0, max = 1), Price = rnorm(mean = 6))
#'
#' # Regroup alternatives into design
#' edesign <- experimental_design$new(alternatives = list(alt1, alt2))
#' Z <- random_design(edesign, n = 1)
#' 
#' # Idendex data frame
#' Z <- index_z(Z, alt_id = 1)
#' 
#' @export

index_z <- function(Z, alt_id = NULL, type = c("AID", "CID")) {
  # Get data size
  m <- nrow(Z)

  # Alternative  ID
  if ("AID" %in% type) Z["AID"] <- rep(alt_id, m)

  # Choice ID
  if ("CID" %in% type) Z["CID"] <- 1:m

  # Output
  return(Z)
}
