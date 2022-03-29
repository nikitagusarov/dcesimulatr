########################################################################
## This file enlists functions which define experimental design class ##
########################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

############################################
# 3. Defining designs  generation function #
############################################

#' @title GEnerate experimental design
#' @description Create experimental design using both factors and random specifications.
#' The design configuration assumes that alternatives' attributes are mixed:
#' vectors declared using `c()` assignement and
#' `declared using `r` class simulation functions.
#'
#' @param experimental_design Input experimnetal design object
#' @param n The n is not used in current version
#' @param resample Declares whether the dataset should be resampled (shuffled) in case of factorial design element presence.
#' @return data.frame A dataset of choice sets (Z) respecting FF Design.
#'
#' @examples
#' # Create alternatives
#' alt1 <- alternative$new()
#' alt1$add_attributes(Quality = c(0:1), Price = rnorm(mean = 2, sd = 1))
#' alt2 <- alternative$new()
#' alt2$add_attributes(Size = c(1:3), Price = runif(min = 1, max = 3))
#'
#' # Regroup alternatives into design
#' edesign <- experimental_design$new(alternatives = list(alt1, alt2), design = "mixed")
#' Z <- designs_gen(edesign, n = 2)
#' @export

designs_gen <- function(
  experimental_design, 
  n = NULL, 
  resample = NULL
) {
  # Avoid warnings on undeclared variales
  CID <- AID <- NULL

  # Verification
  if (is.null(n)) {
    n <- experimental_design$n
    if (is.null(n)) stop("No valid 'n' provided")
  }
  if (is.null(resample)) {
    resample <- experimental_design$resample
    if (is.null(resample)) {
      resample <- FALSE
    }
  }

  # Part 1: Factorial design step
  Z_f <- designs_f(
    experimental_design,
    n = n
  )

  # Part 2: random design step
  Z_r <- designs_r(
    experimental_design,
    n = n
  )

  # Return parts if only parts are available
  if (is.null(Z_f) & is.null(Z_r)) {
    stop("The resulting Z is empty! Nothing to do.")
  }
  if (is.null(Z_f)) {
    return(Z_r)
  }
  if (is.null(Z_r)) {
    return(Z_f)
  } else {
    # Ensure correct ordering
    Z_f <- dplyr::arrange(Z_f, CID, AID)
    Z_r <- dplyr::arrange(Z_r, CID, AID)

    # Define key to join DF by
    key <- c("CID", "AID")

    # Get common subset of colnames
    cnames <- intersect(
      colnames(Z_f),
      colnames(Z_r)
    )
    # Excluding the key
    cnames <- cnames[!(cnames %in% key)]

    # Merge dataframes
    Z <- merge(
      Z_f, Z_r,
      by = key
    )

    # Patch values in common attributes
    for (i in cnames) {
      # Set the colnames
      x <- paste(i, ".x", sep = "")
      y <- paste(i, ".y", sep = "")
      # Replace NA
      Z[is.na(Z[x]), x] <- Z[is.na(Z[x]), y]
      Z[y] <- NULL
      # Reset names
      colnames(Z)[colnames(Z) == x] <- i
    }

    # Function output
    return(as.data.frame(Z))
  }
}
