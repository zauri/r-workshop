setwd(".../r-workshop/")


# Debugging -------------------------------------------------------------------------------------------

fun_f <- function(x) {
  fun_g(x)
}

fun_g <- function(y) {
  fun_h(y)
}

fun_h <- function(z) {
  res <- log(z)
  if (res < 0) {
    res <- res^2
  }
  return(res)
}

fun_f(-1)
traceback()

debug(fun_f)  # shows local environment 
debug(fun_g)
debug(fun_h)
fun_f(-1)


# breakpoints: script needs to be sourced for breakpoints to be active


flip_coins <- function(flips, output_as_strings = FALSE, head = 0) {
  if (head != 0 & head != 1) stop("head must be 0 or 1")
  if (is.numeric(flips) == FALSE) stop("flips must be numeric")
  if (flips >= 200) warning("high number of flips")
  if (flips == 1) message("test")
  
  x <- rbinom(n = flips, size = 1, prob = 0.5)
  if (output_as_strings) {
    x <- ifelse(x == head,
                yes = "Heads",
                no = "Tails")
    class(x) <- c("gamble", "character")   # needs known class
  }   else {
    class(x) <- c("gamble", "numeric")
  }
  
  return(x)
}


flip_coins(10)
flip_coins("Hallo")

tryCatch(flip_coins("10"), error = function(c){flip_coins(10)})
# catches error (flips = character) and restarts flip_coins with numeric value 















