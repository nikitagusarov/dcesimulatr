########################################################################
## This file enlists functions which define experimental design class ##
########################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

###########################################
# 1. Defining "experimental_design" class #
###########################################

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export

experimental_design = R6::R6Class(
    # Class name
    "experimental_design",
    # Architecture
    list(
        design = NULL,
        alternatives = NULL,
        n = NULL,
        identical = FALSE,

        # Initialize
        initialize = function(
            alternatives = list(NULL), 
            design = "FFD",
            n = NULL,
            identical = FALSE
        ) {
            # Write values
            self$alternatives = alternatives
            self$design = design
            self$n = n
            self$identical = identical
        },
        
        # Methods to modify object
        add_alternative = function(
            alternative, alternative_name = NULL
        ) {
            # Verification
            if (!any(class(alternative) == "alternative")) {
                stop("No valid alternative object provided")
            }

            # Add to our list of alternatives
            if (!is.null(alternative_name)) {
                self$alternatives[[ {{ alternative_name }} ]] = alternative
            } else {
                self$alternatives[[length(self$alternatives) + 1]] = alternative
            }
            invisible(self)
        },

        # Methods to querry the object
        get_attributes = function() {
            # Get list of attr from all alternatives
            attr = lapply(
                self$alternatives, 
                function(x) { names(x$attributes) }
            )
            # Keep unique
            attr = unique(
                unlist(attr)
            )
            return(attr)
        },
        get_design = function() {
            # Get design as vector
            design = self$design
            return(design)
        }

    )
)



###################################################################
# 2. Defining functions to operate in "experimental_design" class #
###################################################################

# Experimental design testing

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export 

is.experimental_design = function(experimental_design) {
    any(class(experimental_design) == "experimental_design")
}

# Generation function (indivduals matrix)

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export

alternatives_gen = function(
    experimental_design, 
    n = NULL,
    seed = NULL,
    format = "long"
) {
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
        stop("No valid 'n' provided")
    }

    # Reset seed if required
    if (!is.null(seed)) {
        set.seed(seed)
    }

    # Get unique characteristics' names
    attr = experimental_design$get_attributes()

    # Generate Z - Run simulation
    Z = foreach (
        i = seq_along(experimental_design$alternatives),
        .combine = "rbind"
    ) %do% {
        # Get profile chars, laws and obs numbers
        laws = experimental_design$alternatives[[i]]$get_laws()
        # Update laws with required n
        for (j in seq_along(laws)) {
            laws[[j]]$n = n
        }

        # Create DF per alternative profile
        Z = data.frame(
            lapply(laws, eval)
        )

        # Check compeltenes
        if (
            !rlang::is_empty(
                adattr <- setdiff(attr, colnames(Z))
            )
        ) {
            Z[adattr] = rep(NA, n)
        }

        # Add profile information
        ## Alternative 
        Z["alternative"] = rep(i, n)
        ## Choice ID
        Z["CID"] = 1:n

        # Exit foreach loop
        return(Z)
    }

    # Arrange results
    return(
        dplyr::arrange(Z, CID)
    )
}

