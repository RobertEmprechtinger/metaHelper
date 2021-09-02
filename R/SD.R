#' Pools the SD according to Hedges
#'
#' @keywords internal
#'
#' This is a helper function for SD_pool and not supposed to be used directly.
#'
#' @param n1 sample size group 1
#' @param n2 sample size group 2
#' @param SD1 standard deviation group 1
#' @param SD2 standard deviation group 2
#'
#' @return
#' Pooled standard deviation according to Hedges (1981)
#'
#' @export
#'
#' @examples
#' SD_pool_hedges(2, 3, 50, 50)
SD_pool_hedges <- function(SD1, SD2, n1, n2) {
  result <- c()
  for(i in seq_along(SD1)){

    not_possible <- (!is.na(SD1[i]) & !is.na(SD2[i])) & (is.na(n1[i]) | is.na(n2[i]))

    if(not_possible){
      result[i] <- NA
      warning("hedges method needs sample size. You could try method=cohen instead")
    } else{
      result[i] <- sqrt(((n1[i] - 1) * SD1[i] ^ 2 + (n2[i] - 1) * SD2[i] ^ 2) /
                          (n1[i] + n2[i] - 2))
    }
  }
  return(result)
}


#' Pooled SD from two SDs
#'
#' Calculates pooled standard deviation. The method according to Hedges
#' requires the sample sizes.
#' If exclusively SDs are available, the simpler equation provided by Cohen 1988 can be used.
#'
#'
#' @param SD1 standard deviation of group 1
#' @param SD2 standard deviation of group 2
#' @param n1 sample size of group 1
#' @param n2 sample size of group 2
#' @param method the method that should be used to calculate the SD. Method "hedges" needs sample sizes.
#' Method "cohen" uses the simplified method by Cohen 1988 and does not rely on sample sizes.
#'
#' @return
#' Pooled standard deviation
#'
#' @export
#'
#' @references
#' Borenstein, M., Hedges, L.V., Higgins, J.P.T. and Rothstein, H.R. (2009). Converting Among Effect Sizes. In Introduction to Meta-Analysis (eds M. Borenstein, L.V. Hedges, J.P.T. Higgins and H.R. Rothstein). https://doi.org/10.1002/9780470743386.ch7
#'
#' Ellis, P.D. (2009), "Effect size equations" website: https://www.polyu.edu.hk/mm/effectsizefaqs/effect_size_equations2.html accessed on 2021.08.31.
#'
#' Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect size and related estimators.
#' Journal of Educational Statistics, 6, 107-128.
#'
#' @seealso
#' [metaHelper::SD.within_from_SD.r()] for matched groups
#'
#' @examples
#' # Standard deviation according to Cohen:
#' SD_pool(2, 3, method = "cohen")
#'
#' # Standard deviation according to Hedges needs sample sizes:
#' SD_pool(2, 3, 50, 50)
SD_pool <- function(SD1,
                    SD2,
                    n1 = NA,
                    n2 = NA,
                    method = "hedges") {
  # SD calculation according to Hedges 1981 or Cohen
  if(length(method) == 1){
    method <- rep(method, length(SD1))
  }

  for(i in seq_along(SD1)){
    if(!is.element(method[i], c("hedges", "cohen"))) stop("method needs to be either 'hedges' or 'cohen'")
  }

  ifelse(method == "hedges",
         #hedges method
         SD_pool_hedges(SD1, SD2, n1, n2),
         #cohen method
         sqrt((SD1 ^ 2 + SD2 ^ 2) /
                2))
}


#' SD from Standard Error (single group)
#'
#' Calculates the standard deviation from the standard error for a single group. In case of two arms the method for the pooled standard
#' error has to be used: [metaHelper::SDp_from_SEp()]
#'
#' @param SE standard error
#' @param n sample size
#'
#' @return
#' Single group standard deviation
#'
#' @references
#' https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
#'
#' @export
#'
#' @seealso
#' [metaHelper::SDp_from_SEp()] in case of two arms.
#'
#' @examples
#' # Standard error = 2 and sample size = 100
#' SE <- 2
#' n <- 100
#' SD_from_SE(SE, n)
#'
SD_from_SE <- function(SE, n){
  SE * sqrt(n)
}


#' Standard Deviation from the Pooled Standard Error
#'
#' Calculates the standard deviation from the pooled standard error and sample size fro two groups (e.g. intervention effects).
#' For single groups [SD_from_SE()] has to be used.
#' This method is the reverse method of [SEp_from_SDp()].
#'
#' @references
#' https://handbook-5-1.cochrane.org/chapter_7/7_7_3_3_obtaining_standard_deviations_from_standard_errors.htm
#'
#' @param SEp pooled standard error
#' @param n1 sample size group 1
#' @param n2 sample size group 2
#'
#' @return
#' Pooled standard deviation
#'
#' @seealso
#' [SD_from_SE()] for a single group.
#' [SEp_from_SDp()] if the standard error should be computed instead.
#'
#' @export
#'
#' @examples
#' #pooled standard error, sample size 1 and sample size 2
#' SE <- 0.12
#' n1 <- 140
#' n2 <- 140
#'
#' SDp_from_SEp(SE, n1, n2)
SDp_from_SEp <- function(SEp, n1, n2){
  SEp / sqrt(1/n1 + 1/n2)
}


#' Standard Deviation from Confidence interval
#'
#'
#' Computes the standard deviation from the confidence interval and sample size. This method is only valid for single groups and when the
#' CI is symmetrical around the mean.
#' For two groups (e.g. intervention effects) [SDp_from_CIp()] has to be used.
#'
#' @param CI_low lower limit confidence interval
#' @param CI_up upper limit confidence interval
#' @param N overall sample size
#' @param sig_level significance level
#' @param two_sided whether a two sided test for significance was used
#' @param t_dist whether a t distribution has been used to calculate the CI (usually the case if N < 60)
#'
#' @seealso
#' [SDp_from_CIp()] for two groups (e.g. intervention effects).
#'
#' @return
#' Standard deviation single group
#'
#' @references
#' https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
#'
#' @export
#'
#' @examples
#' # lower CI = -0.5, upper CI = 2, sample size = 100
#' SD_from_CI(-05, 2, 100)
SD_from_CI <- function(CI_low, CI_up, N, sig_level = 0.05, two_sided = TRUE, t_dist = TRUE){
  l <- length(CI_low)
  sig_level <- extend_var(sig_level, l)
  two_sided <- extend_var(two_sided, l)
  t_dist <- extend_var(t_dist, l)

  result <- c()

  for(i in seq_along(CI_low)){
    if(t_dist[i]){
      result[i] <- sqrt(N[i]) * (CI_up[i] - CI_low[i]) / (t_calc(sig_level[i], two_sided[i], N[i] - 1) * 2)
    } else {
      result[i] <- sqrt(N[i]) * (CI_up[i] - CI_low[i]) / (z_calc(sig_level[i], two_sided[i]) * 2)
    }
  }
  return(result)
}


#' 'Within Standard Deviation for Matched Groups
#'
#' Computes the within standard deviation for matched groups. The within standard deviation can further be used to compute the standardized mean
#' differences for matched groups.
#'
#' @param SD_diff standard deviation of the difference (usually reported)
#' @param r correlation between pair of observations
#'
#' @return Within standard deviation
#' @export
#'
#' @examples
#' # SD_diff is the standard deviation of the group difference
#' SD_diff <- 2
#' # r is the correlation coefficient between the groups
#' r <- 0.5
#' SD.within_from_SD.r(SD_diff, r)
SD.within_from_SD.r <- function(SD_diff, r){
  ifelse(abs(r) > 1,
         stop("correlation of r greater 1 is not allowed"),
         SD_diff / sqrt(2 * (1 - r) )
         )
}
