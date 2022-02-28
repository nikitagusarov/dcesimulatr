####################################################################
## This file enlists functions which declare experimental designs ##
####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

#############################################
# 1. Helper functions to be used in designs #
#############################################

# Guessing classes of provided attribute definitions

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export

class_laws = function(laws) {
    # We avoid verification
    # the function is internal to other

    # Classify laws defaulting to "rand"
    laws_j = rep("rand", length(laws))
    # Check other eventual types
    for (j in seq_along(laws)) {
        # Vectors introduced with "c"
        if (laws[[j]][[1]] == expr(c)) {
            laws_j[j] = "c"
        }
        # Sample type objects
        if (laws[[j]][[1]] == expr(sample)) {
            # Sampling from given sets
            laws_j[j] = "sample"
        }
    }

    # Return
    return(laws_j)
}

# Checking attributes of Z aagainst full set

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export

check_attributes = function(
    Z, attributes
) {
    # Get dimensions
    m = nrow(Z)

    # Check completenes
    if (
        !rlang::is_empty(
            adattr <- setdiff(attributes, colnames(Z))
        )
    ) {
        Z[adattr] = rep(NA, m)
    }

    # Output
    return(Z)
}

# # Wrapper for laws

# #' @title
# #' @description
# #' @param
# #' @param 
# #' @method
# #' @examples
# #' @export

# laws_treatment = function(
#     laws
# ) {
#     # Iterate ower laws list
#     for (j in seq_along(laws)) {
#         if (laws[[j]][[1]] == "c") {
#             # Case for vectors (factors)
#             Z = expand.grid()
#         } else {
#             # General case for random laws
#             # accepting 'n' parameter
#             laws[[j]]$n = n
#         }
#     }

#     # Output
#     return(laws)
# }

# Index Z data.frame

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export

index_z = function(
    Z, type = c("AID", "CID")
) {
    # Get data size
    m = nrow(Z)

    # Alternative  ID
    if ("AID" %in% type) Z["AID"] = rep(i, m)

    # Choice ID
    if ("CID" %in% type) Z["CID"] = 1:m

    # Output
    return(Z)
}