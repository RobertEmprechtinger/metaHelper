#' Absolute Risk Difference
#'
#' Calculates the Absolute Risk Difference (ARD) from a Risk Ratio and baseline risk using simulations. The result is ARD as a decimal. The number of replications is fixed at 100,000.
#'
#' @param BR baseline risk
#' @param BRLL baseline risk lower limit confidence interval
#' @param BRUL baseline risk upper limit confidence interval
#' @param RR risk ratio
#' @param RRLL risk ratio lower limit confidence interval
#' @param RRUL risk ratio upper limit confidence interval
#' @param seed seed that is used for the simulation to ensure reproducibility
#'
#' @return
#' Named numeric vector containing median ARD, the lower and upper CI of the ARD.
#'
#' @references
#' Murad M H, Wang Z, Zhu Y, Saadi S, Chu H, Lin L et al. Methods for deriving risk difference (absolute risk reduction) from a meta-analysis BMJ 2023; 381 :e073141 doi:10.1136/bmj-2022-073141
#'
#' @export
#'
#' @importFrom stats median quantile rbeta rlnorm
#'
#' @examples
#' # Input : Baseline risk and 95% CI (BR BRLL and BRUL), risk ratio and 95% CI (RR, RRLL, RRUL)
#' BR <- 0.053; BRLL <- 0.039; BRUL <- 0.072
#' RR <- 0.77; RRLL <- 0.63; RRUL <- 0.94
#' ARD_from_RR(BR, BRLL, BRUL, RR, RRLL, RRUL)
ARD_from_RR <- function(BR, BRLL, BRUL, RR, RRLL, RRUL, seed = 1){
  R <- 100000 # No. of simulations

  # data check
  if(any(c(length(BR), length(BRLL), length(BRUL),
           length(RR), length(RRLL)) != length(RRUL))) stop("All vectors (BR, BRLL, BRUL, RR, RRLL, RRUL) need to have the same length")
  # end data check

  for(i in 1:(length(BR))){
    set.seed(seed)
    # data check
    if(any(is.na(BR), is.na(BRLL), is.na(BRUL),
           is.na(RR), is.na(RRLL), is.na(RRUL))){
      warning("Skipping simulation due to NA value")
      next
    }

    # Get shape parameters of beta dist
    BR_variance <- ((BRUL[i] - BRLL[i])/3.92)^2
    alpha <- ((1 - BR[i]) / BR_variance - 1 / BR[i]) * BR[i] ^ 2
    beta <- alpha * (1 / BR[i] - 1)

    # Simulation
    sim_BR <- rbeta(R, alpha, beta, ncp = 0)
    sim_RR <- rlnorm(R, meanlog = log(RR[i]), sdlog = (log(RRUL[i]) - log(RRLL[i]))/3.92)
    RD <- sim_BR*(sim_RR - 1)


    # Output
    result <- c(median(RD), quantile(RD, c(0.025, 0.975)))
    names(result) <- c("Median ARD", "Lower CI", "Upper CI")

    if(i ==1) result_fin <- list(result) else result_fin[i] <- list(result)
  }

  # return vector input length == 1 or list if more elements are provided
  if(i == 1) return(result_fin[[1]])
    else return(result_fin)
}



