#' Standard Error for a Single Group
#'
#' Calculates the standard error for a single group. In case of two groups (e.g. intervention effect), [SEp_from_SDp.N()] has to be used.
#'
#' @seealso
#' [SEp_from_SDp.N()] for two groups
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
#' @export
#'
#' @examples
#' # Standard deviation = 2, group size = 50
#' SE_from_SD.n(2, 50)
SE_from_SD.n <- function(SD, n){
  SD / sqrt(n)
}


#' Pooled Standard Error
#'
#' Caclulates the pooled standard error for two groups (e.g. intervention effect). In case of a single group, [SE_from_SD.N()] has to be used.
#'
#' @seealso
#' [SEp_from_SDp.N()] for a single group
#'
#' @param SDp pooled standard deviation
#' @param n1 sample size group 1
#' @param n2 sample size group 2
#'
#' @return
#' Pooled standard error for for two groups
#' @export
#'
#' @references
#' https://handbook-5-1.cochrane.org/chapter_7/7_7_3_3_obtaining_standard_deviations_from_standard_errors.htm
#'
#' @examples
#' # Pooled standard deviation = 2, sample size group a = 50, sample size group b = 75,
#' SEp_from_SDp.n(2, 50, 75)
SEp_from_SDp.n <- function(SDp, n1, n2){
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
  SE_log_OR <- SE_from_CI(log(CI_low), log(CI_up), sig_level = sig_level, two_sided = two_sided)
  SE_SMD <- SE_log_OR * sqrt(3)/pi
  return(SE_SMD)
}


#' SMD standard error from sample sizes
#'
#' Approximates SMD standard error from sample sizes. Approximation provided by Borenstein (2009, p.226).
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
#' @return Standard error of SMD
#' @export
#'
#' @examples
#' # SMD = 0.6, sample size group_1 = 50, sample size group_2 = 75
#' SE.SMD_from_SMD.n(0.6, 50, 75)
SE.SMD_from_SMD.n <- function(SMD, n1, n2, method = "hedges"){
  SE <- c()
  for(i in seq_along(SMD)){
    if(length(method) == 1) {
      method <- rep(method, length(SMD))
      }

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
SEp_from_TE.p <- function(TE, p, two_sided = TRUE){
  ifelse(p > 1,
         stop("p needs to be 1 or smaller"),
         function(){})
  ifelse(p == 1,
         p <- 0.99999,
         p)
  TE / z_calc(p, two_sided)
}

