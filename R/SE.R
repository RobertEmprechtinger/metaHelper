#' Calculates the Standard Error for a Single Group
#'
#' @param SD standard deviation
#' @param n sample size
#'
#' Calculates the standard error with the following formulae:
#' SD / sqrt(n)
#'
#' @return
#' @export
#'
#' @examples
SE_from_SD.N <- function(SD, n){
  SD / sqrt(n)
}


#' Calculates the pooled Standard Error
#'
#' @param SDp pooled standard deviation
#' @param n1 sample size group 1
#' @param n2 sample size group 2
#'
#' @return
#' @export
#'
#' @examples
SEp_from_SDp.N <- function(SDp, n1, n2){
  SDp * sqrt(1/n1 + 1/n2)
}


#' SE of SMD from Confidence Intervals of
#'
#' https://stats.stackexchange.com/questions/68290/converting-odds-ratios-to-cohens-d-for-meta-analysis
#'
#' The method uses multiple steps in background:
#' 1. It takes to OR limits and transforms them to log(OR)
#' 2. It calculates the standard error for the log(OR)
#' 3. It transforms the log(OR) standard error to SMD standard error by multiplying it with sqrt(3)/pi
#'
#' The results are tested on the examples provided by Chin (2000). Test tolerance 0.01.
#'
#' Literature:
#' Chinn S. A simple method for converting an odds ratio to effect size for use in meta-analysis. Stat Med. 2000 Nov 30;19(22):3127-31. doi: 10.1002/1097-0258(20001130)19:22<3127::aid-sim784>3.0.co;2-m. PMID: 11113947.
#'
#'
#' @param CI_low lover OR confidence interval limit
#' @param sig_level the significance level
#' @param two_sided whether the two sided z statistics or single sided should be calculated
#' @param CI_up upper OR confidence interval limit
#'
#' @return
#' @export
#'
#' @examples
SMD.SE_from_OR.CI <- function(CI_low, CI_up, sig_level = 0.05, two_sided = TRUE){
  #SE_log_OR <- (log(CI_up) - log(CI_low)) / (2 * z_calc(sig_level, two_sided))
  SE_log_OR <- SE_from_CI(log(CI_low), log(CI_up), sig_level = sig_level, two_sided = two_sided)
  SE_SMD <- SE_log_OR * sqrt(3)/pi
  return(SE_SMD)
}


#' Approximate SMD standard error from sample sizes
#'
#' Estimate SMD standard error from sample sizes. Approximation provided by Borenstein (2009, p.226)
#'
#' Literature:
#' Borenstein M: Effect sizes for continuous data.
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


#' Obtains standard error from confidence intervals
#'
#' Method is according to the archived version of the Cochrane handbook:
#' https://handbook-5-1.cochrane.org/chapter_7/7_7_7_2_obtaining_standard_errors_from_confidence_intervals_and.htm
#'
#' @param CI_low lover OR confidence interval limit
#' @param sig_level the significance level
#' @param two_sided whether the two sided z statistics or single sided should be calculated
#' @param CI_up upper OR confidence interval limit
#'
#' @return
#' @export
#'
#' @examples
SE_from_CI <- function(CI_low, CI_up, sig_level = 0.05, two_sided = TRUE){
  (CI_up - CI_low) / (2 * z_calc(sig_level, two_sided))
}



#' Obtains standard error from treatment effect and p-value
#'
#' Method is according to Altman D G, Bland J M. How to obtain the confidence interval from a P value BMJ 2011; 343 :d2090 doi:10.1136/bmj.d2090
#'
#' To avoid an infinitive return, p = 1 is automatically adjusted to 0.99999
#'
#' @param TE reported treatment effect
#' @param p reported p-value
#' @param two_sided whether the two sided z statistics or single sided should be calculated
#'
#' @return
#' @export
#'
#' @examples
SE_from_TE.p <- function(TE, p, two_sided = TRUE){
  ifelse(p > 1,
         stop("p needs to be 1 or smaller"),
         function(){})
  ifelse(p == 1,
         p <- 0.99999,
         p)
  TE / z_calc(p, two_sided)
}

