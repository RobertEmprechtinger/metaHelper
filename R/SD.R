#' Pools the SD According to Hedges
#'
#' @keywords internal
#'
#' This is a helper function for SDp_from_SD and not supposed to be used directly.
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
#' poolSD_hedges(2, 3, 50, 50)
poolSD_hedges <- function(SD1, SD2, n1, n2) {
  result <- c()
  for(i in seq_along(SD1)){

    not_possible <- (!is.na(SD1[i]) & !is.na(SD2[i])) & (is.na(n1[i]) | is.na(n2[i]))

    if(not_possible){
      result[i] <- NA
      warning("hedges method needs sample size. You could try method='cohen' instead")
    } else{
      result[i] <- sqrt(((n1[i] - 1) * SD1[i] ^ 2 + (n2[i] - 1) * SD2[i] ^ 2) /
                          (n1[i] + n2[i] - 2))
    }
  }
  return(result)
}


#' Pooled Standard Deviation from two Standard Deviations
#'
#' Calculates pooled standard deviation. The method according to Hedges requires the sample sizes. If only standard deviations are available, the simpler equation provided by Cohen 1988 can be used. If there are more than two groups [SD_M_n_pooled_from_groups()] has to be used.
#'
#'
#' @param SD1 standard deviation of group 1
#' @param SD2 standard deviation of group 2
#' @param n1 sample size of group 1
#' @param n2 sample size of group 2
#' @param method the method ("hedges", "cohen") that should be used to calculate the SD. Method "hedges" requires sample sizes.
#' The "cohen" method uses the simplified method by Cohen 1988 and does not rely on sample sizes.
#'
#' @return
#' Pooled standard deviation
#'
#' @export
#'
#' @references
#' Borenstein, M., Hedges, L.V., Higgins, J.P.T. and Rothstein, H.R. (2009). Converting Among Effect Sizes. In Introduction to Meta-Analysis (eds M. Borenstein, L.V. Hedges, J.P.T. Higgins and H.R. Rothstein). https://doi.org/10.1002/9780470743386.ch7
#'
#' Ellis, P.D. (2009), "Effect size equations". \href{https://www.polyu.edu.hk/mm/effectsizefaqs/effect_size_equations2.html accessed on 2021.08.31.}{Link}
#'
#' Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect size and related estimators.
#' Journal of Educational Statistics, 6, 107-128.
#'
#' @seealso
#' [metaHelper::SD_within_from_SD_r()] for matched groups
#'
#' @examples
#' # Standard deviation according to Cohen:
#' SDp_from_SD(2, 3, method = "cohen")
#'
#' # Standard deviation according to Hedges needs sample sizes:
#' SDp_from_SD(2, 3, 50, 50)
SDp_from_SD <- function(SD1,
                    SD2,
                    n1 = NA,
                    n2 = NA,
                    method = "hedges") {
  # data check
  check_data(SD1=SD1, SD2=SD2)
  if(any(method == "hedges")) check_data(n = n1)
  if(any(method == "hedges")) check_data(n = n2)
  # end data check

  # SD calculation according to Hedges 1981 or Cohen
  if(length(method) == 1){
    method <- rep(method, length(SD1))
  }

  for(i in seq_along(SD1)){
    if(!is.element(method[i], c("hedges", "cohen"))) stop("method needs to be either 'hedges' or 'cohen'")
  }

  ifelse(method == "hedges",
         #hedges method
         poolSD_hedges(SD1, SD2, n1, n2),
         #cohen method
         sqrt((SD1 ^ 2 + SD2 ^ 2) /
                2))
}


#' Standard Deviation from Standard Error (Single Group)
#'
#' Calculates the standard deviation from the standard error for a single group. When there are two arms, the following method for pooled standard error has to be used: [metaHelper::SDp_from_SEp()]
#'
#' @param SE standard error
#' @param n sample size
#'
#' @return
#' Single group standard deviation
#'
#' @references
#' \href{https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm}{Cochrane Handbook}
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
  # data check
  check_data(n=n, SE=SE)
  # end data check

  SE * sqrt(n)
}


#' Standard Deviation from the Pooled Standard Error
#'
#' Calculates the standard deviation from the pooled standard error and sample size from two groups (e.g., intervention effects). For single groups [SD_from_SE()] has to be used. This method is the reverse method of [SEp_from_SDp()].
#'

#' @references
#' \href{https://handbook-5-1.cochrane.org/chapter_7/7_7_3_3_obtaining_standard_deviations_from_standard_errors.htm}{Cochrane Handbook}
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
  # data check
  check_data(SE=SEp, n1=n1, n2=n2)
  # end data check

  SEp / sqrt(1/n1 + 1/n2)
}


#' Standard Deviation from Confidence Interval
#'
#'
#' Computes the standard deviation from the confidence interval and sample size. This method is only valid for single groups and when the CI is symmetrical around the mean. For two groups (e.g., intervention effects), [SDp_from_CIp()] has to be used. For sample sizes smaller than 60, the t_dist is usually used to calculate the confidence interval.
#'
#' @param CI_low lower limit confidence interval
#' @param CI_up upper limit confidence interval
#' @param sig_level significance level
#' @param two_sided whether a two sided test for significance was used
#' @param t_dist whether a t-distribution has been used to calculate the CI. See description.
#' @param n sample size
#'
#' @seealso
#' [SDp_from_CIp()] for two groups (e.g. intervention effects).
#'
#' @return
#' Standard deviation single group
#'
#' @references
#' \href{https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm}{Cochrane Handbook}
#'
#' @export
#'
#' @examples
#' # lower CI = -0.5, upper CI = 2, sample size = 100
#' SD_from_CI(-05, 2, 100)
SD_from_CI <- function(CI_low, CI_up, n, sig_level = 0.05, two_sided = TRUE, t_dist = TRUE){
  # data check
  check_data(CI_low=CI_low, CI_up=CI_up, n=n, sig_level=sig_level)
  # end data check

  l <- length(CI_low)
  sig_level <- extend_var(sig_level, l)
  two_sided <- extend_var(two_sided, l)
  t_dist <- extend_var(t_dist, l)

  result <- c()

  for(i in seq_along(CI_low)){
    if(t_dist[i]){
      result[i] <- sqrt(n[i]) * (CI_up[i] - CI_low[i]) / (t_calc(sig_level[i], two_sided[i], n[i] - 1) * 2)
    } else {
      result[i] <- sqrt(n[i]) * (CI_up[i] - CI_low[i]) / (z_calc(sig_level[i], two_sided[i]) * 2)
    }
  }
  return(result)
}


#' Pooled Standard Deviation from Confidence Interval
#'
#' Computes the pooled standard deviation (e.g., standard deviation of an intervention effect) from confidence intervals and sample sizes. The Cochrane Handbook (see references) calls the resulting standard deviation as "within-group standard deviation". This method is only valid if the confidence interval is symmetrical around the mean and when either the t-distribution or normal-distribution (t_dist = FALSE) has been used to calculate the CI.
#'
#' @param CI_low lower limit confidence interval
#' @param CI_up upper limit confidence interval
#' @param sig_level significance level
#' @param two_sided whether a two sided test for significance was used
#' @param t_dist whether a t distribution has been used to calculate the CI
#' @param n1 sample size group 1
#' @param n2 sample size group 2
#'
#' @return
#' Pooled standard deviation
#'
#' @export
#'
#' @seealso
#' [SD_from_CI()] for single group standard deviation.
#'
#' @references
#' \href{https://handbook-5-1.cochrane.org/chapter_7/7_7_3_3_obtaining_standard_deviations_from_standard_errors.htm}{Cochrane Handbook}
#'
#' @examples
#' #lower CI = 0.5, upper CI = 0.7, N1 = 50, N2 = 70
#' SDp_from_CIp(0.5, 0.7, 50, 70)
SDp_from_CIp <- function(CI_low, CI_up, n1, n2, sig_level = 0.05, two_sided = TRUE, t_dist = TRUE){
  # data check
  check_data(CI_low =CI_low, CI_up=CI_up, n1=n1, n2=n2, sig_level=sig_level)
  # end data check

  result <- c()

  l <- length(CI_low)
  sig_level <- extend_var(sig_level, l)
  two_sided <- extend_var(two_sided, l)
  t_dist <- extend_var(t_dist, l)

  for(i in seq_along(CI_low)){
    # In case sample sizes are NA the results will also be NA
    if(is.na(n1[i]) | is.na(n2[i])){
      result[i] <- NA
    } else{
      # Calculation of SE
      SE <- SEp_from_CIp(CI_low[i], CI_up[i], n1[i], n2[i], sig_level[i], two_sided[i], t_dist[i])

      result[i] <- SE / sqrt(1/n1[i] + 1/n2[i])
    }
  }
  return(result)
}


#' Within Standard Deviation for Matched Groups
#'
#' Computes the within standard deviation for matched groups. The within standard deviation can further be used to compute the standardized mean differences for matched groups.
#'
#' @param SD_diff standard deviation of the difference
#' @param r correlation between pair of observations
#'
#' @return Within standard deviation
#' @export
#'
#' @references
#' Borenstein, M., Hedges, L.V., Higgins, J.P.T. and Rothstein, H.R. (2009). Effect Sizes Based on Means . In Introduction to Meta-Analysis (eds M. Borenstein, L.V. Hedges, J.P.T. Higgins and H.R. Rothstein). https://doi.org/10.1002/9780470743386.ch4
#'
#' @examples
#' # SD_diff is the standard deviation of the group difference
#' SD_diff <- 2
#' # r is the correlation coefficient between the groups
#' r <- 0.5
#' SD_within_from_SD_r(SD_diff, r)
SD_within_from_SD_r <- function(SD_diff, r){
  # data check
  check_data(SD=SD_diff, r=r)
  # end data check

  ifelse(abs(r) > 1,
         stop("correlation of absolute r > 1 is not allowed"),
         SD_diff / sqrt(2 * (1 - r) )
         )
}


#' Combined Group Standard Deviation
#'
#' Computes the pooled standard deviation for multiple groups and also returns the combined mean and the combined n.
#'
#' @param n vector of group sample sizes
#' @param SD vector of group SDs
#' @param M vector of group means
#'
#' @return Within standard deviation
#' @export
#'
#' @references
#' \href{https://handbook-5-1.cochrane.org/chapter_7/table_7_7_a_formulae_for_combining_groups.htm}{Cochrane Handbook}
#'
#' Rücker G, Cates CJ, Schwarzer G. Methods for including information from multi-arm trials in pairwise meta-analysis. Res Synth Methods. 2017 Dec;8(4):392-403. doi: 10.1002/jrsm.1259. Epub 2017 Aug 25. PMID: 28759708.
#'
#' @examples
#' # Compute the Standard deviation for the following grouped data
#' M <- c(1, 1.5, 2) # Means
#' SD <- c(2, 3, 2.5) # SDs
#' n <- c(72, 80, 55) # sample sizes
#' SD_M_n_pooled_from_groups(M, SD, n)
SD_M_n_pooled_from_groups <-function(M, SD, n){
  # data check
  check_data(SD=SD, n=n)
  if(any(c(length(n), length(SD)) != length(M))) stop("All vectors (SD, M, n) need to have the same length")
  if(length(n) == 1) stop("Need multiple groups. Data indicates that you have only provided data of 1 group")
  # end data check

  for(i in 1:(length(n) - 1)){
    comparator <- i + 1
    if(i == 1){
      n_base <- n[1]
      M_base <- M[1]
      SD_base <- SD[1]
    }

    SD_base <- sqrt(((n_base-1) * SD_base^2 + (n[comparator] - 1) * SD[comparator]^2 + ( (n_base * n[comparator]) / (n_base + n[comparator]) ) *
      (M_base^2 + M[comparator]^2 - 2*M_base*M[comparator])) /
      (n_base + n[comparator] - 1))

    M_base <- (n_base * M_base + n[comparator] * M[comparator]) /
      (n_base + n[comparator])

    n_base <- n_base + n[comparator]

  }
  result <- c(SD_base, M_base, n_base)
  names(result) <- c("SD", "mean", "n")
  return(result)
}


