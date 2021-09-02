#' Extends the length of a variable
#'
#' @keywords internal
#'
#' @param variable variable that should be extended
#' @param length length of the returned variable
#'
#' @return
#' @export
#'
#' @examples
#' x <- "test"
#' extend_var(x, 5)
extend_var <- function(variable, length){
  if(length(variable) == 1){
    return(rep(variable, length))
  } else return(variable)
}
