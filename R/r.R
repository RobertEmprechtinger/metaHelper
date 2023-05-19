#' Approximates correlation coefficient for pre post studies
#'
#' The correlation coefficient can then be used to calculate SD for
#' mean changes.
#'
#' @param SD_diff standard deviation of the difference
#' @param r correlation between pair of observations
#'
#' @return Within standard deviation
#' @export
#'
#' @references
#'
#' @examples
#' # SD_diff is the standard deviation of the group difference
#' SD_diff <- 2
#' # r is the correlation coefficient between the groups
#' r <- 0.5
#' SD_within_from_SD_r(SD_diff, r)
#r.pre_post <- function(SD_diff, r){
#  ifelse(abs(r) > 1,
#         stop("correlation of r greater 1 is not allowed"),
#         SD_diff / sqrt(2 * (1 - r) )
#  )
#}
