####################################################################
## This file enlists functions which declare experimental designs ##
####################################################################

# nikita.gusarov@univ-grenoble-alpes.fr - February 2022

############################################
# 1. Defining "fractioning" transformation #
############################################

#' @title Fractioning transformation to binary format
#' @description Transform the experimental design based on factors.
#' The initial design configuration assumes that all alternatives' attributes are "factors":
#' vectors declared using `c()` assignement.
#' Other attribute types are ignored.
#' However, such definition allows for multilevel and ordered factors specifications.
#' This function transforms multilevel factors to the completely binary version.
#'
#' @param Z Input factorail matrix as returned by the designs_f function.
#' @return data.frame A dataset of choice sets (Z) respecting the transformation rules.
#'
#' @examples
#' # Create alternatives
#' alt1 <- alternative$new()
#' alt1$add_attributes(Quality = c(0:1), Price = c(2, 2.5))
#' alt2 <- alternative$new()
#' alt2$add_attributes(Size = c(1:3), Price = c(1.5, 2))
#'
#' # Regroup alternatives into design
#' edesign <- experimental_design$new(alternatives = list(alt1, alt2), design = "factorial")
#' Z <- designs_f(edesign, n = 3)
#' designs_fractioning(Z)
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate

designs_fractioning <- function(Z) {
  # Avoid warnings in check
  AID <- j  <- i <- NULL

  # Transform dataset to wide format
  Z_w <- tidyr::pivot_wider(
    Z,
    names_from = AID,
    names_sep = "_",
    values_from = setdiff(colnames(Z), c("CID", "AID"))
  )

  # Index not NA columns
  index <- colSums(is.na(Z_w)) < nrow(Z_w)
  # CLean data from Na
  Z_reduced <- as.data.frame(Z_w[, index])

  # Convert to factors
  Z_reduced <- dplyr::mutate_all(
    Z_reduced[, names(Z_reduced) != "CID"],
    as.factor
  )

  # Get remaining colnames
  cnames <- colnames(Z_reduced)
  # Transform to binary factors
  Z_l <- list()
  for (i in seq(ncol(Z_reduced))) {
    main <- Z_reduced[, i]
    lv <- levels(main)
    if (length(lv) > 2) {
      out_l <- list()
      for (j in seq_along(lv)) {
        res <- data.frame(as.numeric(main == lv[j]))
        colnames(res) <- paste0(cnames[i], ".", lv[j])
        out_l[[j]] <- res
      }
      out <- do.call(
        "cbind",
        out_l
      )
      rm(out_l)
      Z_l[[i]] <- out[, -1]; rm(out)
    } else {
      out <- data.frame(main)
      colnames(out) <- cnames[i]
      Z_l[[i]] <- out; rm(out)
    }
  }
  Z_factorial <- do.call(
    "cbind",
    Z_l
  )

  # Clean
  rm(Z_l); gc()

  # Output
  return(Z_factorial)
}
