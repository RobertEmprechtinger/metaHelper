#' Calculates the z_value
#'
#' @param sig_level the significance level
#' @param two_sided whether the two sided z statistics or single sided should be calculated
#'
#' @return
#' @export
#'
#' @examples
z_calc <- function(sig_level = 0.05,
                   two_sided = TRUE) {
  ifelse(two_sided,
         abs(qnorm(sig_level / 2)),
         abs(qnorm(sig_level)))
}
