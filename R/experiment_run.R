#####################################################################
## This file enlists functions used for data generation procedures ##
#####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

######################################
# 1. Application of decision formula #
######################################

#' @title
#' @description
#' @param
#' @param 
#' @method
#' @examples
#' @export

experiment_run = function(
    population,
    experimental_design
) {
    # First we perform coupling
    XZ = experiment_compose(
        population,
        experimental_design
    )

    # Retrieve rules from population
    rules = population$get_rules()

    # Create deterministic utility dummy
    XZ$DU = rep(NA, nrow(XZ))
    XZ$TU = rep(NA, nrow(XZ))

    # Apply rules
    for (i in seq_along(population$profiles)) {
        # Get classes
        cl = unique(XZ$class)
        # Update noise params
        noise = rules[[i]]$noise
        # Update rules
        form = rules[[i]]$formula

        # Verify length of noise
        if (
            (length(noise) == 1) & 
            (length(noise) != length(cl))
        ) {
            noise = rep(noise, length(cl))
        }

        # Verify length of formula
        if (
            (length(form) == 1) & 
            (length(form) != length(cl))
        ) {
            form = rep(form, length(cl))
        }
        
        # Apply to XZ
        for (j in seq_along(cl)) {
            # Get noise params for alternative
            noise_j = noise[[j]]
            noise_j$n = nrow(XZ[
                (XZ$class == i) & (XZ$AID == j), 
            ])

            # Get respecive formula
            formula_j = form[[j]]
            
            # Application
            XZ[(XZ$class == i) & (XZ$AID == j), ] = dplyr::mutate(
                XZ[
                    (XZ$class == i) & (XZ$AID == j), 
                ], 
                DU = !!formula_j,
                TU = DU + !!noise_j
            )
        }
    }

    # Output
    return(XZ)
}
