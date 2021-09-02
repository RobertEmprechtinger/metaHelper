#' Standard Error for a Single Group
#'
#' Calculates the standard error for a single group. In case of two groups (e.g. intervention effect), [SEp_from_SDp()] has to be used.
#'
#' @seealso
#' [SEp_from_SDp()] for two groups
#'
#' @param SD standard deviation
#' @param n sample size
#'
#' Calculates the standard error with the following formula:
#' SD / sqrt(n)
#'
#' @return
#' Single group standard error
#'
#' @references
#' https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm#'
#'
#' @export
#'
#' @examples
#' # Standard deviation = 2, group size = 50
#' SE_from_SD(2, 50)
SE_from_SD <- function(SD, n){
  SD / sqrt(n)
}


#' Pooled Standard Error
#'
#' Caclulates the pooled standard error for two groups (e.g. intervention effect). In case of a single group, [SE_from_SD()] has to be used.
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
#' @export
#'
#' @references
#' https://handbook-5-1.cochrane.org/chapter_7/7_7_3_3_obtaining_standard_deviations_from_standard_errors.htm
#'
#' @examples
#' # Pooled standard deviation = 2, sample size group a = 50, sample size group b = 75,
#' SEp_from_SDp(2, 50, 75)
SEp_from_SDp <- function(SDp, n1, n2){
  SDp * sqrt(1/n1 + 1/n2)
}


#' SE of SMD from Confidence Intervals of Odds Ratio
#'
#' https://stats.stackexchange.com/questions/68290/converting-odds-ratios-to-cohens-d-for-meta-analysis
#'
#' The method uses multiple steps in background:
#' 1. Takes OR limits and transforms them to log(OR)
#' 2. Calculates the standard error for the log(OR)
#' 3. Transforms the log(OR) standard error to SMD standard error by multiplying it with sqrt(3)/pi
#'
#' @references
#' Chinn S. A simple method for converting an odds ratio to effect size for use in meta-analysis. Stat Med. 2000 Nov 30;19(22):3127-31. doi: 10.1002/1097-0258(20001130)19:22<3127::aid-sim784>3.0.co;2-m. PMID: 11113947.
#'
#'
#' @param CI_low lower OR confidence interval limit
#' @param sig_level the significance level
#' @param two_sided whether the two sided z statistics or single sided should be calculated
#' @param CI_up upper OR confidence interval limit
#'
#' @return
#' @export
#'
#' @examples
#' # lower CI = 0.6, upper CI = 0.9
#' SE.SMD_from_OR.CI(0.6, 0.9)
SE.SMD_from_OR.CI <- function(CI_low, CI_up, sig_level = 0.05, two_sided = TRUE){
  SE_log_OR <- SEp_from_CIp(log(CI_low), log(CI_up), sig_level = sig_level, two_sided = two_sided, t_dist = FALSE)
  SE_SMD <- SE_log_OR * sqrt(3)/pi
  return(SE_SMD)
}


#' Standard Error from Sample Sizes and SMD
#'
#' Approximates SMD standard error from sample sizes and SMD. Approximation provided by Borenstein (2009, p.226).
#'
#' @references
#' Borenstein M: Effect sizes for continuous data.
#' The Handbook of Research Synthesis and Meta-Analysis. Edited by: Cooper H, Hedges LV, Valentine JC. 2009,
#' New York: Russell Sage, 221-235. 2
#'
#' @param n1 sample size group 1
#' @param n2 sample size group 2
#' @param SMD standardized mean differences
#'
#' @return Standard error of SMD (e.g. standard error of intervention effect)
#' @export
#'
#' @examples
#' # SMD = 0.6, sample size group_1 = 50, sample size group_2 = 75
#' SE.SMD_from_SMD(0.6, 50, 75)
SE.SMD_from_SMD <- function(SMD, n1, n2, method = "hedges"){
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


#' Standard Error from Confidence Interval for differences of means
#'
#' Calculates the standard error from confidence interval limits for differences of means.
#' This method is only valid in case the confidence interval is symmetrical
#' around the mean and works only for the t or normal distribution (argument t_dist).
#'
#' @references
#' https://handbook-5-1.cochrane.org/chapter_7/7_7_7_2_obtaining_standard_errors_from_confidence_intervals_and.htm
#'
#' @param CI_low lover OR confidence interval limit
#' @param sig_level the significance level
#' @param two_sided whether the two sided statistics or single sided should be calculated
#' @param CI_up upper OR confidence interval limit
#'
#' @return
#' Pooled standard error (e.g. intervention effect)
#' @export
#'
#' @examples
#' # lower CI = -1.5, upper CI = 0.5
#' SEp_from_CIp(-1.5, 0.5)
SEp_from_CIp <- function(CI_low, CI_up, N1 = NA, N2 = NA, sig_level = 0.05, two_sided = TRUE, t_dist = TRUE){
  l <- length(CI_low)
  sig_level <- extend_var(sig_level, l)
  two_sided <- extend_var(two_sided, l)
  t_dist <- extend_var(t_dist, l)

  result <- c()
  for(i in seq_along(CI_low)){
    if(t_dist[i]){
      if(is.na(N1[i]) | is.na(N2[i])){
        result[i] <- NA
        warning("t_dist needs sample size")
      } else{
        result[i] <- abs((CI_up[i] - CI_low[i]) / (2 * t_calc(sig_level[i], two_sided[i], df = N1[i] + N2[i] - 2)))
        }
    } else{
      result[i] <- abs((CI_up[i] - CI_low[i]) / (2 * z_calc(sig_level[i], two_sided[i])))
    }
  }
  return(result)
}


#' Standard Error from Treatment Effect and p-value
#'
#' Calculates the pooled standard error by the use of the treatment effect and p value.
#' To avoid an infinitive return in case of p = 1, p is automatically adjusted to 0.99999
#'
#' @param TE reported treatment effect
#' @param p reported p-value
#' @param two_sided whether the two sided statistics or single sided should be calculated
#'
#' @return
#' Pooled standard error (e.g. standard error of intervention effect)
#'
#' @export
#'
#' @references
#' Altman D G, Bland J M. How to obtain the confidence interval from a P value BMJ 2011; 343 :d2090 doi:10.1136/bmj.d2090
#'
#' @examples
#' # TE = 1.5, p = 0.8
#' SEp_from_TE.p(1.5, 0.8)
SEp_from_TE.p <- function(TE, p, two_sided = TRUE){
  SE <- c()
  l <- length(TE)
  two_sided <- extend_var(two_sided, length(TE))

  for(i in seq_along(TE)){
    ifelse(p[i] == 1,
           p[i] <- 0.99999,
           p[i])
    SE[i] <- TE[i] / z_calc(p[i], two_sided[i])
  }
  return(SE)
}

