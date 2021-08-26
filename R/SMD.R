###### OR & SMD ##########

#' Convert SMD to OR
#'
#' Converts SMD to OR by using the formula provided here:
#' https://stats.stackexchange.com/questions/68290/converting-odds-ratios-to-cohens-d-for-meta-analysis
#'
#' Borenstein, M., Hedges, L.V., Higgins, J.P.T. and Rothstein, H.R. (2009). Converting Among Effect Sizes. In Introduction to Meta-Analysis (eds M. Borenstein, L.V. Hedges, J.P.T. Higgins and H.R. Rothstein). https://doi.org/10.1002/9780470743386.ch7
#'
#' @param OR Odds Ratio
#'
#' @return Returns SMD
#' @export
#'
#' @examples
#'
SMD_from_OR <- function(OR){
  log(OR) * sqrt(3) / pi
}


#' Calculates SMD
#'
#' Uses the differences of two means and divides with the (pooled) standard deviation or the standard deviation of the
#' control group in case Glass's delta should be calculated.
#'
#' @param M1 treatment effect size group 1
#' @param M2 treatment effect size group 2
#' @param SD_pooled the pooled standard deviation or the standard deviation of the control group in case Glass's delta should be calculated
#'
#' @return
#' @export
#'
#' @examples
SMD_calc <- function(M1, M2, SD_pooled){
  (M1 - M2) /
    SD_pooled
}


#' Calculates SMD from arm data
#'
#' @param M1 treatment effect size group 1
#' @param M2 treatment effect size group 2
#' @param SD1 standard deviation group 1
#' @param SD2 standard deviation group 2
#' @param n1 sample size group 1
#' @param n2 sample size group 2
#' @param method
#'
#' @return
#' @export
#'
#' @examples
SMD_from_arm <-
  function(M1,
           M2,
           SD1,
           SD2,
           n1 = NA,
           n2 = NA,
           method = "hedges") {
    SMD_calc((M1 - M2) /
               SD_pool(SD1, SD2, n1, n2, method))
  }


