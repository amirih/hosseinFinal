#' Add two numbers together
#'
#' This function takes two numeric inputs and returns their sum.
#'
#' @param number1 A numeric value representing the first number to be added.
#' @param number2 A numeric value representing the second number to be added.
#' @return A numeric value representing the sum of the two input numbers.
#' @examples
#' add(3, 4) # returns 7
#' add(-2, 5) # returns 3
#' add(0, 0) # returns 0
#' add(1.5, 2.5) # returns 4
#' @export
add <- function(number1, number2) {
  (number1 + number2)
} 
