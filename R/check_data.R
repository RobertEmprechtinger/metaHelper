check_data <- function(
  n = NA,
  SD = NA,
  SDp = NA,
  SEp = NA,
  SE = NA,
  n1 = NA,
  n2 = NA,
  SD1 = NA,
  SD2 = NA,
  SD_diff = NA,
  r = NA,
  SD_within = NA,
  CI_low = NA,
  CI_up = NA,
  OR = NA,
  sig_level = NA
){
  if(any(!is.na(n))) if(any(n <= 0, na.rm = TRUE)) stop("n needs to be > 0")
  if(any(!is.na(OR))) if(any(OR <= 0, na.rm = TRUE)) stop("OR needs to be > 0")
  if(any(!is.na(SD))) if(any(SD < 0, na.rm = TRUE)) stop("SD needs to be >= 0")
  if(any(!is.na(SEp))) if(any(SEp < 0, na.rm = TRUE)) stop("SE needs to be >= 0")
  if(any(!is.na(SE))) if(any(SE < 0, na.rm = TRUE)) stop("SE needs to be >= 0")
  if(any(!is.na(SDp))) if(any(SDp < 0, na.rm = TRUE)) stop("SD needs to be >= 0")
  if(any(!is.na(SD_diff))) if(any(SD_diff < 0, na.rm = TRUE)) stop("SD needs to be >= 0")
    if(any(!is.na(n1)) & any(is.na(n2))) if(any(n1 < 0, na.rm = TRUE) | any(n2 < 0, na.rm = TRUE)) stop("n needs to be > 0")
  if(any(!is.na(SD1)) & any(is.na(SD2))) if(any(SD1 < 0, na.rm = TRUE) | any(SD2 < 0, na.rm = TRUE)) stop("SD needs to be > 0")
  if(any(!is.na(SD_within))) if(any(SD_within < 0, na.rm = TRUE)) stop("SD needs to be > 0")
  if(any(!is.na(CI_low)) & any(!is.na(CI_up))) if(any(CI_low > CI_up)) stop("Lower CI needs to be < than upper CI")
  if(any(!is.na(sig_level))) if(sig_level <= 0 | sig_level >= 1) stop("Significance level needs to be between 0 and 1")
  if(any(!is.na(r))) if(any(abs(r) < 0, na.rm = TRUE) | any(abs(r) > 1, na.rm = TRUE)) stop("Absolute r needs to be <= 1 and >= 0")
}

