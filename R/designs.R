####################################################################
## This file enlists functions which declare experimental designs ##
####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

###############################
# 2. Defining "random" design #
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

        # Class laws
        laws_j = class_laws(laws)

        # Update adjust dimensions for laws
        # for (j in which(laws_j == "rand")) {
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
        Z = index_z(Z)

        # Exit foreach loop
        return(Z)
    }

    # Function output
    return(Z)
}



# # For fixed vectors use FFD by default
# # this produces a proto matrix pZ with FFD
# pZ = do.call(
    # expand.grid,
    # laws[[laws_c == "c"]]
# )