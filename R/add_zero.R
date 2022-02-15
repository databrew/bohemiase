#' Prepend zero(s) to a number
#'
#' Prepend one or more 0's to a number. Useful for alphabetizing facto levels named with numbers.
#' @param x The vector to be modified
#' @param n The number of characters that the resulting vector should have
#' @return A character vector of identical length to \code{x} in which all elements have n characters (or more, for those which already had more prior to processing)
#' @export

add_zero <- function(x, n){
  x <- as.character(x)
  adders <- n - nchar(x)
  adders <- ifelse(adders < 0, 0, adders)
  for (i in 1:length(x)){
    if(!is.na(x[i])){
      x[i] <- paste0(
        paste0(rep('0', adders[i]), collapse = ''),
        x[i],
        collapse = '')
    }
  }
  return(x)
}
