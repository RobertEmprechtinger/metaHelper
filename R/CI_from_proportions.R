#' Confidence Interval for Proportions
#'
#' Calculates a confidence interval for proportions. For a discussion on the differences between methods to calculate confidence intervals, see the Stack Overflow discussion under References.
#' This method uses the R package "confintr" to calculate the confidence intervals.
#'
#' @param events number of events
#' @param n sample size
#' @param method the method ("Clopper-Pearson", "Agresti-Coull", "Wilson") that should be used to calculate the confidence intervals.
#'
#' @return
#' List of confidence interval of proportions if input length > 1. If input length = 1 Lower CI and Upper CI.
#'
#' @export
#'
#' @references
#' \href{https://search.r-project.org/CRAN/refmans/confintr/html/ci_proportion.html}{Confintr Function Description}
#' \href{https://stats.stackexchange.com/questions/82720/confidence-interval-around-binomial-estimate-of-0-or-1}{Stackoverflow Method Discussion}
#'
#' @examples
#' # CI for 9 events in a sample of 10
#' CI_from_proportions(9, 10)
CI_from_proportions <- function(events,
                        n,
                        method = "Clopper-Pearson") {
  # data check
  check_data(n=n)
  check_data(n=events)

  for(i in 1:length(events)){
    if(events[i] > n[i]){stop("Events must be smaller than n")}
    if(is.na(method[i])) c_method <- method
    else c_method <- method[i]
  # end data check
    result_i <- confintr::ci_proportion(events[i], n[i], type = c_method)
    result_i <- result_i$interval
    names(result_i) <- c("Lower CI", "Upper CI")

    if(i == 1) result <- list(result_i)
    else result[i] <- list(result_i)
  }
  if(i == 1) result <- unlist(result)

  return(result)
}
