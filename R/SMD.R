#' Standardized Mean Difference from Odds Ratio
#'
#' Approximates SMD from OR.
#'
#' @param OR odds ratio
#'
#' @return
#' Standardized Mean Difference
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
  # check data
  check_data(OR = OR)
  # check data end
  log(OR) * sqrt(3) / pi
}


#' Standardized Mean Difference (SMD) from Means and Pooled Standard Deviation
#'
#' Calculates the SMD. It needs to be provided with the pooled standard deviation. If the pooled standard deviation is not available [SMD_from_group()] provides a direct method to calculate the SMD and also offers different forms like Hedges' g or Cohen's d.
#'
#' CAVE: If you want to get Hedges' g it is insufficient to simply pool the standard deviation with [SDp_from_SD()]. The resulting SMD needs to be further multiplied with the hedges factor. This is done automatically when you use [SMD_from_group()].
#'
#' @param M1 treatment effect size group 1
#' @param M2 treatment effect size group 2
#' @param SD_pooled the pooled standard deviation or the standard deviation of the control group in case Glass's delta should be calculated
#'
#' @return
#' Standardized Mean Differences
#' @export
#'
#' @references
#' https://handbook-5-1.cochrane.org/chapter_9/9_2_3_2_the_standardized_mean_difference.htm
#'
#' @examples
#' # Mean control = 153, Mean intervention = 136, pooled SD = 25
#' SMD_from_mean(153, 136, 25)
SMD_from_mean <- function(M1, M2, SD_pooled){
  # check data
  check_data(SD = SD_pooled)
  # check data end
  (M1 - M2) /
    SD_pooled
}


#' Standardized Mean Differences from Group Data
#'
#' Calculates SMD directly from group data. Method "hedges" needs sample size data and returns Hedges' g. Method "cohen" returns Cohen's d.
#'
#' @param M1 treatment effect size group 1
#' @param M2 treatment effect size group 2
#' @param SD1 standard deviation group 1
#' @param SD2 standard deviation group 2
#' @param n1 sample size group 1
#' @param n2 sample size group 2
#' @param method calculation method ("hedges", "cohen")
#'
#' @return
#' Standardized Mean Differences
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
#' SMD_from_group(23, 56, 30, 35, 45, 60)
SMD_from_group <-
  function(M1,
           M2,
           SD1,
           SD2,
           n1 = NA,
           n2 = NA,
           method = "hedges") {
    if(length(method) == 1) method <- rep(method, length(M1))
    #check data
    check_data(SD1 = SD1, SD2 = SD2)
    for(i in 1:length(n1)){
      if(method[i] == "hedges") {
          check_data(n1 = n1[i], n2 = n2[i])
        }
    }
    #check data end

    SMD <- c()

    for(i in seq_along(M1)){
      if(!(method[i] %in% c("hedges", "cohen"))) stop("method needs to be either 'hedges', 'cohen'")

      SMD[i] <- SMD_from_mean(M1[i], M2[i],
                         SDp_from_SD(SD1[i], SD2[i], n1[i], n2[i], method[i]))

      # Hedges
      if(method[i] == "hedges"){
        SMD[i] <- SMD[i] * hedges_factor(n1[i], n2[i])
      }
    }

    return(SMD)
  }


#' Calculates SMD from Matched Groups
#'
#' Calculates the standardized mean differences for matched groups. Needs either the mean of the groups or the difference between groups.
#' SD_within is usually not reported but can be calculated by the use of [SD_within_from_SD_r()].
#'
#' @param M_diff mean difference between groups
#' @param M1 mean group 1 (in case M_diff not provided)
#' @param M2 mean group 2 (in case M_diff not provided)
#' @param SD_within within standard deviation. CAVE this is usually not reported but needs to be computed from the difference standard deviation.
#' This can be done with [SD_within_from_SD_r()].
#'
#' @return
#' Standardized Mean Differences
#' @export
#'
#' @references
#' M., Hedges, L.V., Higgins, J.P.T. and Rothstein, H.R. (2009). Converting Among Effect Sizes. In Introduction to Meta-Analysis (eds M. Borenstein, L.V. Hedges, J.P.T. Higgins and H.R. Rothstein). https://doi.org/10.1002/9780470743386.ch7
#'
#' @examples
#' # Calcuation with group means
#' SMD_from_mean_matched(M1 = 103, M2 = 100, SD_within = 7.1005)
#'
#' # Calculation with group difference
#' SMD_from_mean_matched(M_diff = 3, SD_within = 7.1005)
#'
#' # Calculation with standard deviation between
#' # Correlation Coefficient between groups
#' r <- 0.7
#'
#' # SD between groups
#' SD_between <- 5.5
#'
#' SMD_from_mean_matched(M_diff = 3,
#'     SD_within = SD_within_from_SD_r(SD_between, r))
SMD_from_mean_matched <- function(M_diff = NA, M1 = NA, M2 = NA, SD_within) {
  #check data
  check_data(SD = SD_within)
  #check data end

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
