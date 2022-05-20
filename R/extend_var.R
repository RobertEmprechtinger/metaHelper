#' Extends the length of a variable
#'
#' @keywords internal
#'
#' @param variable variable that should be extended
#' @param length length of the returned variable
#'
#' @export
extend_var <- function(variable, length){
  if(length(variable) == 1){
    return(rep(variable, length))
  } else return(variable)
}

a <- "a"
b <- "b"

for(x in c(a, b)){
  assign(x, extend_var(x, 10))
}
