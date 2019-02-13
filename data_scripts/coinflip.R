#' coinflip
#' 
#' Simulates flipping of a coin
#' 
#' @param flips number of coins to flip
#' @param str.out should the result be a value of 0 or 1 or a string of Kopf or Zahl
#' @param head which value represents head, should be a value of \code{c(0, 1)}
#' @return a vector of class gamble
#' @examples 
#' coinflip(10, str.out = TRUE)
#' 
#' @export
coinflip <- function(flips, str.out = FALSE, head = 0) {
  
  # Kontrollbedingung
  if(!is.numeric(flips)) stop("flips needs to be numeric")
  if(head != 0 & head != 1) stop("head must be 0 or 1")  
  
  # Münzwurf
  x <- rbinom(n = flips, size = 1, prob = .50)
  
  # string output
  if(str.out){
    x <- ifelse(x == head,
                yes = "Kopf",
                no = "Zahl")
  }
  
  class(x) <- c("gamble", "character")
  
  return(x)
}