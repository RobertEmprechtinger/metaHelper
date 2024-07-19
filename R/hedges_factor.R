#' Computational simple approximation of the hedges factor
#' @noRd
#'
#' @param n1 sample size group 1
#' @param n2 sample size group 2
#' @param n_total total sample size
#'
#'
#' @references
#' Borenstein, M., Hedges, L.V., Higgins, J.P.T. and Rothstein, H.R. (2009). Effect Sizes Based on Means. In Introduction to Meta-Analysis (eds M. Borenstein, L.V. Hedges, J.P.T. Higgins and H.R. Rothstein). https://doi.org/10.1002/9780470743386.ch4
#'
hedges_factor_approx <- function(n1 = NA, n2 = NA, n_total = NA){
  ifelse(is.na(n_total),
         df <- n1 + n2 - 2,
         df <- n_total - 2
         )

  1 - 3 / (4 * df - 1)
}


#' Hedges factor exact calculation using the gamma distribution.
#' @noRd
#'
#' @param n1 sample size group 1
#' @param n2 sample size group 2
#' @param n_total total sample size
#'
#'
#' @references
#' Hedges L. V., Olkin I. (1985). Statistical methods for meta-analysis. San Diego, CA: Academic Press
#'
hedges_factor <- function(n1 = NA, n2 = NA, n_total = NA){
  ifelse(is.na(n_total),
         df <- n1 + n2 - 2,
         df <- n_total - 2
  )

  result <- gamma(df / 2) /
    (sqrt(df / 2) * gamma((df - 1) / 2))
  if(is.na(result) & !is.na(df)) result <- 1
  return(result)
}
