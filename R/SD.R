#' Title
#'
#' @param n1
#' @param n2
#' @param SD1
#' @param SD2
#'
#' @return
#' @export
#'
#' @examples
#' @keywords internal
SD_pool_hedge <- function(SD1, SD2, n1, n2) {
  not_possible <- (!is.na(SD1) & !is.na(SD2)) & (is.na(n1) | is.na(n2))
  ifelse(not_possible,
    stop("hedges method needs sample size. You could try method=cohen instead"),
    sqrt(((n1 - 1) * SD1 ^ 2 + (n2 - 1) * SD2 ^ 2) /
           (n1 + n2 - 2))
  )
}


#' Pooled SD from two SDs
#'
#' Calculates pooled standard deviation. The method according to Hedges (1981, p.110)
#' needs the sample sizes (https://www.polyu.edu.hk/mm/effectsizefaqs/effect_size_equations2.html).
#' If exclusively SDs are provided, the simpler equation provided by Cohen 1988 can be used.
#'
#' @param SD1 Standard deviation of group 1
#' @param SD2 Standard deviation of group 2
#' @param n1 Sample size of group 1
#' @param n2 Sample size of group 2
#' @param method the method that should be used to calculate the SD. "hedges" calculates SD* Hedges (1981, p.110) which can further be used to compute hedges g. "cohen" uses the simpliefied method by Cohen 1988,
#'
#' @return
#' @export
#'
#' @examples
SD_pool <- function(SD1,
                    SD2,
                    n1 = NA,
                    n2 = NA,
                    method = "hedges") {
  # SD calculation according to Hedges 1981 or Cohen
  ifelse(method == "hedges",
         #hedge method
         SD_pool_hedge(SD1, SD2, n1, n2),
         #cohen method
         sqrt((SD1 ^ 2 + SD2 ^ 2) /
                2))
}


#' SD from Standard Error
#'
#'This is a simple helper to calculate standard deviation from the standard error
#'
#' @param SE standard error
#' @param n sample size
#'
#' @return
#' @export
#'
#' @examples
SD_from_SE <- function(SE, n){
  SE * sqrt(n)
}


#' Calculates the pooled standard deviation from the pooled standard error
#'
#' It is the reverse method of SEp_from_SDp.N()
#'
#' Literature:
#' https://handbook-5-1.cochrane.org/chapter_7/7_7_3_3_obtaining_standard_deviations_from_standard_errors.htm
#'
#' @param SEp
#' @param n1
#' @param n2
#'
#' @return
#' @export
#'
#' @examples
SDp_from_SEp <- function(SEp, n1, n2){
  SEp / sqrt(1/n1 + 1/n2)
}



