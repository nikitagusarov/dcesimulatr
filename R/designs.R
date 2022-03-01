####################################################################
## This file enlists functions which declare experimental designs ##
####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

###############################
# 1. Defining "random" design #
###############################

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export

random_design = function(
    experimental_design, n
) {
    # Get unique characteristics' names
    attr = experimental_design$get_attributes()

    # Generate Z - Run simulation
    Z = foreach (
        i = seq_along(experimental_design$alternatives),
        .combine = "rbind"
    ) %do% {
        # Get profile chars, laws and obs numbers
        laws = experimental_design$alternatives[[i]]$get_laws()

        # Update adjust dimensions for laws
        for (j in seq(laws)) {
            # General case for random laws
            laws[[j]]$n = n
        }  

        # Create DF per alternative profile
        Z = data.frame(
            lapply(laws, eval)
        )

        # Check
        Z = check_attributes(Z, attr)

        # Index data
        Z = index_z(Z, alt_id = i)

        # Exit foreach loop
        return(Z)
    }

    # Function output
    return(Z)
}



##################################
# 2. Defining "factorail" design #
##################################

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export
#' @importFrom tidyr expand_grid pivot_longer

factorial_design = function(
    experimental_design, n
) {
    # Idex attributes defined with c
    index = laws_index(
        experimental_design, type = "c"
    ) 

    # Get unique characteristics' names
    attr = experimental_design$get_attributes(index)

    # Generate Z - Run simulation
    Z = foreach (
        i = seq_along(experimental_design$alternatives),
        .combine = "expand_grid"
    ) %do% {
        # Get profile chars, laws and obs numbers
        laws = experimental_design$alternatives[[i]]$get_laws()
        laws = laws[index[[i]]]

        # Create DF per alternative profile
        Z = do.call(
            expand.grid, laws
        )

        # Check
        Z = check_attributes(Z, attr)

        # Add index to colnames
        colnames(Z) = paste0(
            colnames(Z), "_", i
        )

        # Exit foreach loop
        return(Z)
    }

    # Index data
    Z = index_z(Z, alt_id = i, type = "CID")

    # Expand to long format
    Z = pivot_longer(
        Z, -CID,
        names_to = c(".value", "AID"),
        names_pattern = "(.*)_(.*)"
    )

    # Function output
    return(as.data.frame(Z))
}
# # For fixed vectors use FFD by default
# # this produces a proto matrix pZ with FFD
# pZ = do.call(
    # expand.grid,
    # laws[[laws_c == "c"]]
# )