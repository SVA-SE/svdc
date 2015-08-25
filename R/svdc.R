#' Add two numbers
#'
#' @param a The first number to add
#' @param b The second number to add to the first
#'
#' @description A method for adding two numbers without using a plus sign
#'
#' @export
#'
#' @examples
#'
#' a <- 1
#' b <- 2
#' add(a,b)

add <- function(a,b) {
    if(!all(identical(class(a), "numeric"), identical(class(b), "numeric"))){
        stop("Both arguments must be numeric")
    }
    return(a+b)
}
