#' Standardized Mean Difference from Odds Ratio
#'
#' Approximates SMD from OR.
#'
#' @param OR odds ratio
#'
#' @return
#' @export
#'
#' @examples
#' # Transform an OR of 0.3 to SMD
#' SMD_from_OR(0.3)
#'
#' @references
#' Borenstein, M., Hedges, L.V., Higgins, J.P.T. and Rothstein, H.R. (2009). Converting Among Effect Sizes. In Introduction to Meta-Analysis (eds M. Borenstein, L.V. Hedges, J.P.T. Higgins and H.R. Rothstein). https://doi.org/10.1002/9780470743386.ch7
#'
SMD_from_OR <- function(OR){
  if(OR <= 0){
    stop("OR needs to be greater than 0")
  }
  log(OR) * sqrt(3) / pi
}


#' Standardized Mean Differences from Means and Standard Deviations
#'
#' Uses the differences of two means and divides with the (pooled) standard deviation or the standard deviation of the
#' control group in case Glass's delta should be calculated.
#'
#' @param M1 treatment effect size group 1
#' @param M2 treatment effect size group 2
#' @param SD_pooled the pooled standard deviation or the standard deviation of the control group in case Glass's delta should be calculated
#'
#' @return
#' @export
#'
#' @examples
#' # Mean control = 153, Mean intervention = 136, pooled SD = 25
#' SMD_calc(153, 136, 25)
SMD_from_mean <- function(M1, M2, SD_pooled){
  if(SD_pooled <= 0) stop("SD needs to be greater than 0")
  (M1 - M2) /
    SD_pooled
}


#' Standardized Mean Differences from Arm Data
#'
#' Calculates SMD directly from study data. Method "hedges" needs sample size data.
#'
#' @param M1 treatment effect size group 1
#' @param M2 treatment effect size group 2
#' @param SD1 standard deviation group 1
#' @param SD2 standard deviation group 2
#' @param n1 sample size group 1
#' @param n2 sample size group 2
#' @param method transformation method ("hedges", "cohen")
#'
#' @return
#' @export
#'
#' @references
#' Borenstein, M., Hedges, L.V., Higgins, J.P.T. and Rothstein, H.R. (2009). Converting Among Effect Sizes. In Introduction to Meta-Analysis (eds M. Borenstein, L.V. Hedges, J.P.T. Higgins and H.R. Rothstein). https://doi.org/10.1002/9780470743386.ch7
#'
#' Hedges L. V., Olkin I. (1985). Statistical methods for meta-analysis. San Diego, CA: Academic Press
#'
#' Goulet-Pelletier, J.-C., & Cousineau, D. (2018). A review of effect sizes and their confidence intervals, Part 1: The Cohen’s d family. The Quantitative Methods for Psychology, 14(4), 242–265. https://doi.org/10.20982/tqmp.14.4.p242
#'
#' @examples
#' # Mean control = 23, Mean intervention = 56, SD control = 30,
#' #    SD intervention = 35, sample size control = 45, sample size intervention = 60
#' SMD_from_arm(23, 56, 30, 35, 45, 60)
SMD_from_arm <-
  function(M1,
           M2,
           SD1,
           SD2,
           n1 = NA,
           n2 = NA,
           method = "hedges") {
    if(length(method) == 1) method <- rep(method, length(M1))
    SMD <- c()

    for(i in seq_along(M1)){
      if(!(method[i] %in% c("hedges", "cohen"))) stop("method needs to be either 'hedges' or 'cohen'")

      SMD[i] <- SMD_from_mean(M1[i], M2[i],
                         SDp_from_SD(SD1[i], SD2[i], n1[i], n2[i], method[i]))

      if(method[i] == "hedges"){
        SMD[i] <- SMD[i] * hedges_factor(n1[i], n2[i])
      }
    }
    return(SMD)
  }


#' Calculates SMD from matched groups
#'
#' Calculates the standardized mean differences for matched groups. Needs either the mean of the groups or
#' the difference between groups.
#' SD_within is usually not reported but can be calculated by the use of [SD_within_from_SD_r()].
#'
#' @param M_diff mean difference between groups
#' @param M1 mean group 1 (in case M_diff not provided)
#' @param M2 mean group 2 (in case M_diff not provided)
#' @param SD_within within standard deviation. CAVE this is usually not reported but needs to be computed from the difference standard deviation.
#' This can be done with [SD_within_from_SD_r()].
#'
#' @return
#' @export
#'
#' @references
#' M., Hedges, L.V., Higgins, J.P.T. and Rothstein, H.R. (2009). Converting Among Effect Sizes. In Introduction to Meta-Analysis (eds M. Borenstein, L.V. Hedges, J.P.T. Higgins and H.R. Rothstein). https://doi.org/10.1002/9780470743386.ch7
#'
#' @examples
#' # Calcuation with group means
#' SMD.matched_calc(M1 = 103, M2 = 100, SD_within = 7.1005)
#'
#' # Calculation with group difference
#' SMD.matched_calc(M_diff = 3, SD_within = 7.1005)
#'
#' # Calculation with standard deviation between
#' # Correlation Coefficient between groups
#' r <- 0.7
#'
#' # SD between groups
#' SD_between <- 5.5
#'
#' SMD.matched_calc(M_diff = 3,
#'     SD_within = SD.within_from_SD.r(SD_between, r))
SMD_from_mean_matched <- function(M_diff = NA, M1 = NA, M2 = NA, SD_within) {
  l <- length(SD_within)
  M_diff <- extend_var(M_diff, l)
  M1 <- extend_var(M1, l)
  M2 <- extend_var(M2, l)

  SMD <- c()

  for(i in seq_along(SD_within)){
    # check whether differences or group values were reported
    ifelse(is.na(M_diff[i]),
           method <- "ind",
           method <- "diff")

    # calculate d on the basis of group values
    ifelse(method == "ind" & (is.na(M1[i]) | is.na(M2[i])),
           SMD[i] <- NA,
           SMD[i] <- SMD_from_mean(M1[i], M2[i], SD_within[i])
    )

    # on the basis of diff values
    ifelse(method == "diff",
           SMD[i] <- M_diff[i] / SD_within[i],
           SMD[i])
  }
  return(SMD)
}


SMD_from_arm.pre_post <- function(M1_pre,
                                  M2_pre,
                                  M1_post,
                                  M2_post,
                                  SD1_pre,
                                  SD2_pre,
                                  SD1_post,
                                  SD2_post,
                                  n1 = NA,
                                  n2 = NA,
                                  method = "hedges"){
  # 1. get pooled SDs
  if(method == "hedges"){
    if(is.na(n1) | is.na(n2)){
      warning("hedges method needs sample size. You could try method='cohen' instead")
      return(NA)
    }
  }
  SD1 <- SDp_from_SD(SD1_pre, SD1_post, n1, n2, method)
  SD2 <- SDp_from_SD(SD2_pre, SD2_post, n1, n2, method)

  # 2. get pre post differences
  MDiff1 <- M1_post - M1_pre
  MDiff2 <- M2_post - M2_pre

  # 3. Calculate SMD
  SMD_from_arm(MDiff1, MDiff2, SD1, SD2, n1, n2, method)
}




