#' Pythagorean
#'
#' The Pythagorean theorem states that the square of the hypotenuse (the side opposite the right angle) is equal to the sum of the squares of the other two side
#'
#' @param a The adjacent side of the right triangle - leg 1
#' @param b The opposite side of the right triangle - leg 2
#' @param c The hypotenuse side of the right triangle - hypotenuse
#' @return Calculation of the length for the third side of triangle
#' @examples
#' pythag(a=12, b=8)
#' @export


### Pythagorean
pythag <- function(a=NULL,b=NULL,c=NULL){
  value <- c(a,b,c)
  if (!is.numeric(a) & !is.numeric(b) & !is.numeric(c)){
    stop('This function only works for numeric inputs in all values of a,b,c')
  }
  if (length(value) == 1 | length(value) == 3){
    stop('This function only works for two inputs of the rectangle sides,
         either (a,b); (a,c); or (b,c)')
  }
  if(is.numeric(b) & is.numeric(c)){
    leg1 <- sqrt(c^2-b^2)
    print(list(Leg1 = leg1))
  }
  else if(is.numeric(a) & is.numeric(c)){
    leg2 <- sqrt(c^2-a^2)
    print(list(Leg2 = leg2))
  }
  else if(is.numeric(a) & is.numeric(b)){
    hypo <- sqrt(b^2+a^2)
    print(list(Hypotenuse=hypo))
  }
}


