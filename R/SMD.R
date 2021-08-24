###### OR & SMD ##########

#' Convert SMD to OR
#'
#' Converts SMD to OR by using the formula provided here:
#' https://stats.stackexchange.com/questions/68290/converting-odds-ratios-to-cohens-d-for-meta-analysis
#'
#' @param OR
#'
#' @return
#' @export
#'
#' @examples
#'
SMD_from_OR <- function(OR){
  log(OR) * sqrt(3)/pi
}

SMD.SE_from_OR <- function(CI_low, CI_up, z){
  SE_log_OR <- (log(CI_up) - log(CI_low)) / (2 * z)
  SE_SMD <- SE_log_OR * sqrt(3)/pi
  return(SE_SMD)
}


#' Approximate SMD standard error from sample sizes
#'
#' Approximation provided by Borenstein M: Effect sizes for continuous data.
#' The Handbook of Research Synthesis and Meta-Analysis. Edited by: Cooper H, Hedges LV, Valentine JC. 2009,
#' New York: Russell Sage, 221-235. 2
#'
#' @param n1 sample size group 1
#' @param n2 sample size group 2
#' @param SMD standardized mean differences
#'
#' @return Returns standard error
#' @export
#'
#' @examples
SMD.SE_from_SMD_n <- function(n1, n2, SMD){
  sqrt(
    (n1 + n2) / (n1 * n2) +
      SMD^2 / (2 * (n1 + n2) )
  )
}


