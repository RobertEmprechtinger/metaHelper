#' Standard Error for a Single Group
#'
#' **IMPORTANT**: For cases involving two groups (e.g., intervention effects), use [SEp_from_SDp()] instead.#'
#' Calculates the standard error for a single group. This method is only valid for single groups
#'
#' @seealso
#' [SEp_from_SDp()] for two groups
#'
#' @param SD standard deviation
#' @param n sample size
#'
#' @return
#' Single group standard error
#'
#' @export
#'
#' @references
#' \href{https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm}{Cochrane Handbook}
#'
#' @examples
#' # Standard deviation = 2, group size = 50
#' SE_from_SD(2, 50)
SE_from_SD <- function(SD, n){
  #check data
  check_data(SD=SD, n=n)
  #check data end
  SD / sqrt(n)
}


#' Standard Error (Pooled)
#'
#' **IMPORTANT**: When there is only one group, the following method has to be used: [SE_from_SD()]
#' Calculates the pooled standard error for two groups (e.g., intervention effect).
#'
#' @seealso
#' [SE_from_SD()] for a single group
#'
#' @param SDp pooled standard deviation
#' @param n1 sample size group 1
#' @param n2 sample size group 2
#'
#' @return
#' Pooled standard error for two groups (e.g. standard error of intervention effect)
#'
#' @export
#'
#' @references
#' \href{https://handbook-5-1.cochrane.org/chapter_7/7_7_3_3_obtaining_standard_deviations_from_standard_errors.htm}{Cochrane Handbook}
#'
#' @examples
#' # Pooled standard deviation = 2, sample size group a = 50, sample size group b = 75
#' SEp_from_SDp(2, 50, 75)
SEp_from_SDp <- function(SDp, n1, n2){
  #check data
  check_data(SD=SDp, n=c(n1, n2))
  #check data end
  SDp * sqrt(1/n1 + 1/n2)
}


#' Standard Error of from Confidence Intervals of Odds Ratio
#'
#' Calculates the standard error from an odds ratio confidence interval.
#'
#' This method uses multiple steps in the background:
#' 1 Takes odds ratio (OR) limits and transforms them to log(OR)
#' 2 Calculates the standard error for the log(OR)
#' 3 Transforms the log(OR) standard error to standardized mean differences (SMD) standard error by multiplying it with sqrt(3)/pi
#'
#' @references
#' Chinn S. A simple method for converting an odds ratio to effect size for use in meta-analysis. Stat Med. 2000 Nov 30;19(22):3127-31. doi: 10.1002/1097-0258(20001130)
#'
#' @param CI_low lower odds ratio confidence interval limit
#' @param sig_level the significance level
#' @param two_tailed whether the two-tailed or one-tailed z statistics should be calculated
#' @param CI_up upper odds ratio confidence interval limit
#'
#' @return
#' Standard Error
#'
#' @export
#'
#' @examples
#' # lower CI = 0.6, upper CI = 0.9
#' SE.SMD_from_OR.CI(0.6, 0.9)
SE.SMD_from_OR.CI <- function(CI_low, CI_up, sig_level = 0.05, two_tailed = TRUE){
  #check data
  check_data(CI_low = CI_low, CI_up = CI_up, sig_level=sig_level)
  #check data end

  SE_log_OR <- SEp_from_CIp(log(CI_low), log(CI_up), sig_level = sig_level, two_tailed = two_tailed, t_dist = FALSE)
  SE_SMD <- SE_log_OR * sqrt(3)/pi
  return(SE_SMD)
}


#' Standard Error from Sample Sizes and SMD
#'
#' Approximates SMD standard error from sample sizes and SMD.
#'
#' @references
#' Cooper, H., Hedges, L. V., & Valentine, J. C. (Eds.). (2009). \href{https://psycnet.apa.org/record/2009-05060-000}{Link}
#'
#' @param n1 sample size group 1
#' @param n2 sample size group 2
#' @param SMD standardized mean differences
#' @param method transformation method ("hedges", "cohen")
#'
#' @return Standard error of SMD (e.g. standard error of intervention effect)
#'
#' @export
#'
#' @examples
#' # SMD = 0.6, sample size group_1 = 50, sample size group_2 = 75
#' SE.SMD_from_SMD(0.6, 50, 75)
SE.SMD_from_SMD <- function(SMD, n1, n2, method = "hedges"){
  # check data
  check_data(n = c(n1, n2))
  # check data end

  SE <- c()
  if(length(method) == 1) {
    method <- rep(method, length(SMD))
  }
  for(i in seq_along(SMD)){
    if(!is.element(method[i], c("hedges", "cohen"))){
      stop("method needs to be either 'hedges' or 'cohen'")
    }

    SE[i] <- sqrt(
      (n1[i] + n2[i]) / (n1[i] * n2[i]) +
        SMD[i]^2 / (2 * (n1[i] + n2[i]) )
    )

    SE[i] <- ifelse(method[i] == "hedges",
                    SE[i] * hedges_factor(n1[i], n2[i]),
                    SE[i])
  }
  return(SE)
}


#' Standard Error from Confidence Interval for Differences of Means
#'
#' Calculates the standard error from the confidence interval limits for differences of means (and can also be used for the confidence intervals of standardized mean differences, SMD).
#' This method is valid only when the confidence interval is symmetrical around the mean and is applicable for t-distributions or normal distributions (as specified by the `t_dist` argument).
#' For sample sizes less than 60, it is generally recommended to use the t-distribution.
#'
#' @param CI_low lover OR confidence interval limit
#' @param sig_level the significance level
#' @param two_tailed whether the two-tailed or one-tailed statistics should be calculated
#' @param CI_up upper OR confidence interval limit
#' @param n1 sample size group 1 (not required if t_dist = FALSE)
#' @param n2 sample size group 2 (not required if t_dist = FALSE)
#' @param t_dist whether the t-distribution should be calculated - requires samples sizes
#'
#' @return
#' Pooled standard error (e.g. intervention effect)
#'
#' @export
#'
#' @references
#' \href{https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm}{Cochrane Handbook}
#'
#' @examples
#' # lower CI = -1.5, upper CI = 0.5
#' SEp_from_CIp(-1.5, 0.5)
SEp_from_CIp <- function(CI_low, CI_up, n1 = NA, n2 = NA, sig_level = 0.05, two_tailed = TRUE, t_dist = TRUE){
  # check data
  check_data(CI_low = CI_low, CI_up = CI_up, n = c(n1, n2), sig_level = sig_level)
  # check data end

  l <- length(CI_low)
  sig_level <- extend_var(sig_level, l)
  two_tailed <- extend_var(two_tailed, l)
  t_dist <- extend_var(t_dist, l)

  result <- c()
  for(i in seq_along(CI_low)){
    if(t_dist[i]){
      if(is.na(n1[i]) | is.na(n2[i])){
        result[i] <- NA
        warning("t_dist needs sample size")
      } else{
        result[i] <- abs((CI_up[i] - CI_low[i]) / (2 * t_calc(sig_level[i], two_tailed[i], df = n1[i] + n2[i] - 2)))
      }
    } else{
      result[i] <- abs((CI_up[i] - CI_low[i]) / (2 * z_calc(sig_level[i], two_tailed[i])))
    }
  }
  return(result)
}


#' Standard Error from Treatment Effect and p-Value
#'
#' Calculates the pooled standard error using the treatment effect and p-value.
#' To avoid an infinitive return when p-value = 1, the p-value is automatically adjusted to 0.99999
#'
#' @param TE reported treatment effect
#' @param p reported p-value
#' @param two_tailed whether one-tailed or two-tailed statistics should be calculated
#'
#' @return
#' Pooled standard error (e.g. standard error of intervention effect)
#'
#' @export
#'
#' @references
#' Altman D G, Bland J M. How to obtain the confidence interval from a P value BMJ 2011; 343 :d2090 doi:10.1136/bmj.d2090
#' \href{https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm}{Cochrane Handbook}
#'
#' @examples
#' # TE = 1.5, p = 0.8
#' SEp_from_TE.p(1.5, 0.8)
SEp_from_TE.p <- function(TE, p, two_tailed = TRUE){
  # check data
  check_data(p=p)
  # check data end

  SE <- c()
  l <- length(TE)
  two_tailed <- extend_var(two_tailed, length(TE))

  for(i in seq_along(TE)){
    ifelse(p[i] == 1,
           p[i] <- 0.99999,
           p[i])
    SE[i] <- TE[i] / z_calc(p[i], two_tailed[i])
  }
  return(SE)
}


