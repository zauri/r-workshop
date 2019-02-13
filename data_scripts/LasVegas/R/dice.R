#' throw_dice
#' 
#' Simulates rolling of dice
#' 
#' @param throws number of throws of dice
#' @param sides number of dice sides
#' @return a vector of class gamble
#' @examples 
#' throw_dice(throws = 1, sides = 6)
#' 
#' @export
throw_dice <- function(throws = 1, sides = 6) {
  
  # control function for number of sides
  if (sides <= 2) warning("this is no real dice, but I do it anyway :)")
  
  # control function: check if throws is a numeric value
  if (is.numeric(throws) == FALSE) stop("throws must be numeric")
  
  # rolling of dice
  results <- sample(x = sides, size = throws, replace = TRUE)
  
  # assign class gamble
  class(results) <- c("gamble", "character")
  
  # return result
  return(results)
}