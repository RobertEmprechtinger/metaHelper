#' Calculates the t_value
#' @noRd
#' @keywords internal
#'
#' @param sig_level the significance level
#' @param two_sided whether the two sided t statistics or single sided should be calculated
#' @param df = degrees of freedom
#'
t_calc <- function(sig_level = 0.05,
                   two_sided = TRUE,
                   df) {
  ifelse(two_sided,
         abs(qt(sig_level / 2, df)),
         abs(qt(sig_level, df)))
}
