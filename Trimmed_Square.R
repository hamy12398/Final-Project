#' Trimmed Mean
#'
#' The trimmed mean is the calculation of the mean of a numeric vector x, excluding the s smallest number and l largest mean from the vector x.
#'
#' @param s The number of smallest numbers
#' @param l The number of largest numbers
#' @return The mean of a numeric vector x, ignoring the s smallest and l largest values
#' @examples
#' x <- c(5,3,1,90,43,23,14)
#' trimean(x, 2,2)
#' @export

#### Trimmed Mean

trimean <- function(x,s,l){
  if(length(x)< s+l+1){
    stop('Your vector of x should have at least s+l+1 values')
  }
  k <- sort(x)
  new_vec <- k[-c(1:s, (length(k)-l+1):length(k))]
  mean <- mean(new_vec)
  return(list(New_Vector = new_vec, Mean = mean))
}
