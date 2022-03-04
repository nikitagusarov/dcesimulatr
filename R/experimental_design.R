########################################################################
## This file enlists functions which define experimental design class ##
########################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

###########################################
# 1. Defining "experimental_design" class #
###########################################

#' @title Eperimental design class
#' @docType class
#'
#' @description The `experimental_design` R6 class regroups different alternatives.
#' It contains all the essential information of the desired experimental design.
#' It serves as a wrapper for simultaneous interactions with alternative profiiles.
#' The object is used for choice sets generation procedures.
#'
#' @field alternatives A list of alternatives to use for experimental design construction.
#' @field design An experimental design specification.
#' This field assumes that a savvy user may desire to extend the number of available experimental designs.
#' In the default configuration available designs are: "random", "factorial" and "mixed".
#' @field n A number of choice sets per individual for "random" design configuration.
#' @field identical Logical.
#' Declares whether the choice sets should be identical across individuals or not.
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
#' 
#' @export

experimental_design <- R6::R6Class(
  # Class name
  "experimental_design",
  # Architecture
  list(
    alternatives = NULL,
    design = NULL,
    n = NULL,
    identical = FALSE,

    # Initialize
    #' @method initialize experimental_design
    #' @description Create new experimental design object. 
    #' It's possible to create the whole object with one command.
    #' @param alternatives A list of alternatives to use for experimental design construction.
    #' @param design An experimental design specification.
    #' This field assumes that a savvy user may desire to extend the number of available experimental designs.
    #' In the default configuration available designs are: "random", "factorial" and "mixed". 
    #' The preset value is "random". 
    #' @param n A number of choice sets per individual for "random" design configuration.
    #' NULL by deafult. 
    #' @param identical Logical.
    #' Declares whether the choice sets should be identical across individuals or not.
    #' The default value if FALSE. 
    #' @return An `experimental_design` object. 
    initialize = function(alternatives = list(NULL),
                          design = "random",
                          n = NULL,
                          identical = FALSE) {
      # Write values
      self$alternatives <- alternatives
      self$design <- design
      self$n <- n
      self$identical <- identical
    },

    # Methods to modify object
    #' @method add_alternative experimental_design
    #' @description Add new alternative to the list of available alternatives.
    #' @param alternative An `alternative` object to be included into experimental design. 
    #' @param alternative_name An added alternative name, not required. 
    #' Is NULL by default. 
    add_alternative = function(alternative, alternative_name = NULL) {
      # Verification
      if (!any(class(alternative) == "alternative")) {
        stop("No valid alternative object provided")
      }
      # Add to our list of alternatives
      if (!is.null(alternative_name)) {
        self$alternatives[[{{ alternative_name }}]] <-
          alternative
      } else {
        self$alternatives[[length(self$alternatives) + 1]] <-
          alternative
      }
      invisible(self)
    },
    #' @method set_design experimental_design
    #' @description Set a new `design` configuration.
    #' @param design Specification of the desired design. 
    #' The default designs are "random" (default), "factorial" and "mixed". 
    set_design = function(design = "random") {
      # Reset design
      self$design <- design
      invisible(self)
    },

    # Methods to querry the object
    #' @method get_attributes experimental_design
    #' @description Get a vector of available attributes' names across all alternatives. 
    #' @param index A list of index of attributes to be querried. 
    #' @return Character list of the attributes by alternative. 
    get_attributes = function(index = NULL) {
      # Check index
      if (is.null(index)) {
        index <- lapply(
          self$alternatives,
          seq_along
        )
      }
      # Get list of attr from all alternatives
      attr <- lapply(
        self$alternatives,
        function(x) {
          names(x$attributes)
        }
      )
      # Index elements
      for (i in seq_along(attr)) {
        attr[[i]] <- attr[[i]][index[[i]]]
      }
      # Keep unique
      attr <- unique(
        unlist(attr)
      )
      return(attr[!is.na(attr)])
    },
    #' @method get_design experimental_design
    #' @description Get specified design.
    #' @return Character value of the experimental design. 
    get_design = function() {
      # Get design as vector
      design <- self$design
      return(design)
    }
  )
)



###################################################################
# 2. Defining functions to operate in "experimental_design" class #
###################################################################

# Experimental design testing

#' @title Experimental design class testing
#' @description Test if the given object has an `experimental_design` class.
#'
#' @param experimental_design Input object to test
#' @return Logic
#'
#' @examples
#' edesign <- experimental_design$new()
#' is.population(edesign)
#' 
#' @export

is.experimental_design <- function(experimental_design) {
  any(class(experimental_design) == "experimental_design")
}

# Generation function (alternatives matrix)

#' @title Generate experimental design (choice sets)
#' @description Generate experimental design data (2 in standart notation) from an `experimental_design` object.
#'
#' @param experimental_design Input experimental design configuration
#' @param n It is possible to generate different number of choice sets without directly modifying the `experimental_design` object.
#' User may locally overwrite the `n` value for the particular execution.
#' This parameter is for internal use in conjunction with various wrappers.
#' @param seed The seed to be set before attempting to generate data.
#' Defaults to NULL.
#' @param format The resulting data format specification.
#' At this tage only the "long" format is supported.
#' Meaning each line contains one alternative.
#' @return data.frame A data.frame (Z) with simulated choice sets
#' (one row per alternative if élongé format was declared).
#'
#' @examples
#' # Create alternatives
#' alt1 <- alternative$new()
#' alt1$add_attributes(Quality = runif(min = 0, max = 1), Price = rnorm(mean = 5))
#' alt2 <- alternative$new()
#' alt2$add_attributes(Size = runif(min = 0, max = 1), Price = rnorm(mean = 6))
#'
#' # Regroup alternatives into design
#' edesign <- experimental_design$new(alternatives = list(alt1, alt2), n = 4)
#' Z <- alternatives_gen(edesign)
#' 
#' @export

alternatives_gen <- function(experimental_design,
                             n = NULL,
                             seed = NULL,
                             format = "long") {
  # Avoid check failure
  CID <- NULL 
  
  # Verification
  if (!is.experimental_design(experimental_design)) {
    stop("No valid experimental design object provided")
  }
  if (!all(unlist(
    lapply(experimental_design$alternatives, is.alternative)
  ))) {
    stop("No valid alternatives' provided")
  }
  if (is.null(n)) {
    n <- experimental_design$n
    if (is.null(n)) stop("No valid 'n' provided")
  }

  # Reset seed if required
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Random
  if (experimental_design$design == "random") {
    Z <- random_design(
      experimental_design,
      n = n
    )
  }

  # Factorial
  if (experimental_design$design == "factorial") {
    Z <- factorial_design(
      experimental_design,
      n = n
    )
  }

  # Mixed
  if (experimental_design$design == "mixed") {
    Z <- mixed_design(
      experimental_design,
      n = n
    )
  }

  # Long format dataset
  if (format == "long") {
    Z <- dplyr::arrange(Z, CID)
  }

  # Arrange results
  return(Z)
}
