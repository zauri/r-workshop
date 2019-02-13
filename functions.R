# Functions ----------------------------------------------------------------------

rbinom(n = 1, size = 1, prob = 0.5)
rbinom(n = 3, size = 1, prob = 0.5)
rbinom(n = 100, size = 1, prob = 0.5)


flip_coins <- function(flips = 50) { # set default n
  x <- rbinom(n = flips, size = 1, prob = 0.5)
  return(x)
}

flip_coins(flips = 100)

read.csv()
body(read.csv)        # body + options

formals(read.csv)     # default options
environment(read.csv) # environment/package

mean.default          # generic function: options for different input types


flip_coins_test <- function(flips = 50, ...) { # set default n, possibility for more arguments (e.g. prob)
  x <- rbinom(n = flips, size = 1, ...)
  return(x)
}

flip_coins_test(prob = 0.7)


# Exercise: dice with sample-function

throw_dice <- function(throws = 1, sides = 6) {
  results <- sample(x = sides, size = throws, replace = TRUE)
  return(results)
}

barplot(table(throw_dice(777)))

setwd("C:/Users/wenzl/Downloads/R-Schulung/")
# read.csv(file = "vgsales.csv")


# Exercise: vgsales
vg_sales <- read.csv("vgsales.csv", stringsAsFactors = FALSE)

vg_sales$Year <- as.numeric(vg_sales$Year)

(max(vg_sales$NA_Sales) - min(vg_sales$NA_Sales)) * 1000000
(max(vg_sales$EU_Sales) - min(vg_sales$EU_Sales)) * 1000000
(max(vg_sales$JP_Sales) - min(vg_sales$JP_Sales)) * 1000000
(max(vg_sales$Other_Sales) - min(vg_sales$Other_Sales)) * 1000000
(max(vg_sales$Global_Sales) - min(vg_sales$Global_Sales)) * 1000000


# sales_range <- function(dataset = vg_sales, col_name = "NA_Sales") {
#   range = max(dataset[, col_name]) - min(dataset[, col_name]) * 1000000
#   return(range)
#  }

# sales_range(col_name = "EU_Sales")

compute_range <- function(vector, adjustment_factor = 1000000) {
  result <- (max(vector) - min(vector)) * adjustment_factor
  return(result)
  }

compute_range(vg_sales$EU_Sales)


# base::range()    # use function of specific package (duplicate names)

# Environments ---------------------------------------------------------------------------------------

search() # shows all environments
# local environments for individual functions --> do not change global environment
# global env. has higher priority


ls(pos = -1)
newEnv <- new.env() # creates new environment
assign(x = "gx", value = 10:21)
ls(pos = 1)

assign(x = "mx", value = 10:21, envir = newEnv)
ls()

exists("mx", envir = newEnv)
get("mx", envir = newEnv)

# a <- function() {
#   z <<- 99 # maps also for global environment with double arrow!
# }


# If else -----------------------------------------------------------------------

new_seed <- 42
set.seed(new_seed)
coins <- flip_coins(10)


if (coins == 0) {
  print("Head")
} else {
  print("Tails")
}


ifelse(test = coins == 0, yes = "Heads", no = "Tails")
ifelse(test = coins == 0, yes = 1:10, no = -1:-10)

all(coins == 0)
any(coins == 0)


flip_coins <- function(flips, output_as_strings = FALSE, head = 0) {
  x <- rbinom(n = flips, size = 1, prob = 0.5)
  if (output_as_strings) {
    x <- ifelse(x == head,
                yes = "Heads",
                no = "Tails")
  }
  return(x)
}

flip_coins(10)
flip_coins(10, output_as_strings = TRUE)


# Stop conditions, warnings, error messages -------------------------------------------------------------

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
  }
  return(x)
}

flip_coins(10)
flip_coins(10, output_as_strings = TRUE)


throw_dice <- function(throws = 1, sides = 6) {
  if (sides <= 2) warning("this is no real dice, but I do it anyway :)")
  if (is.numeric(throws) == FALSE) stop("throws must be numeric")
    
  results <- sample(x = sides, size = throws, replace = TRUE)
  return(results)
}


# S3 classes ------------------------------------------------------------------------------------------

coin <- flip_coins(10)
class(coin)

plot(coin)

coin.f <- as.factor(coin)
class(coin.f)
plot(coin.f)

methods(plot) # see available methods for plot
coin

print.gamble <- function(x) {
  cat("Series of", length(x), "throws", "\n")
  cat(names(table(x)), "\n")
  cat(table(x), "\n")
  
  cat( "----------------------------------\n")
  print(data.frame(Round = seq_along(x), Throw = x))
}

methods(print)
print(coin)

coin

# Exercise

# add plot for gamble
plot.gamble <- function(x) {
  #if (is.character(x) == FALSE) stop("x is already ")
  
  x <- as.factor(x)
  plot(x)
}

# add class gamble to flip_coins
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



# add class gamble to throw_dice
throw_dice <- function(throws = 1, sides = 6) {
  if (sides <= 2) warning("this is no real dice, but I do it anyway :)")
  if (is.numeric(throws) == FALSE) stop("throws must be numeric")
  
  results <- sample(x = sides, size = throws, replace = TRUE)
  class(results) <- c("gamble", "character")
  return(results)
}


coin <- flip_coins(20, output_as_strings = TRUE)
plot.gamble(coin)


dice <- throw_dice(throws = 10)
plot.gamble(dice)



# Loops ------------------------------------------------------------------------------------------

for (i in 1:3) {
  if (i %% 2 == 0) next(i)
  print(i^2)
}

for (i in 1:100) {
  if (i^2 > 90) break
  print(i^2)
}


for (zvar in c(99, 5, -444)) {
  print(zvar)
}

for (i in c("Homer", "Peter", "Kermit")) {
  print(paste("Hello", i, sep = " "))
}

# seq_along
seq_along(c(2, 10, 20))

for (i in seq_along(vg_sales[, 1])) {
  print(i)
}

for (i in 1:length(vg_sales)) { # bad if i = 0, counts backwards
  print(i)
}

# repeat
i <- 1
repeat {
  i <- i + 1
  print(i)
  if (i > 9) break
}


# Loop for dice-function

dice_dataframe <- data.frame(matrix(nrow = 10000, ncol = 6))
par(mfrow = c(3, 2))
dice_throws <- 1
for (i in c(4, 6, 8, 10, 12, 20)) {
  dice_results <- throw_dice(10000, sides = i)
  dice_dataframe[, dice_throws] <- dice_results
  plot(dice_results)
  dice_throws <- dice_throws + 1
}
hist(dice_results)
write.csv(dice_results, file = "dice_results.csv")


number_of_throws <- 0
repeat {
  number_of_throws <- number_of_throws + 1
  dice1 <- throw_dice(throws = 1, sides = 6)
  dice2 <- throw_dice(throws = 1, sides = 6)
  if (dice1 == 6 && dice2 == 6) {
    print(number_of_throws)
    save(number_of_throws, file = "number_of_throws.RData")
    break
  }
}


# Apply Functions ---------------------------------------------------------------------------------------

# apply()
# lapply(list, function)
# sapply(list, function)
# tapply(vector, index, function)
# vapply(list, function, FUN.VALUE = type, ...)

vg_only_sales <- vg_sales[, grepl("Sales", colnames(vg_sales))]

apply(vg_only_sales, MARGIN = 2, FUN = max) # 1 rows, 2 columns
apply(vg_only_sales, MARGIN = 2, FUN = sum)
apply(vg_only_sales, MARGIN = 2, FUN = quantile, probs = c(.25, .5, .75))

vg_only_sales_total <- apply(vg_only_sales, c(1, 2), "*", 1000)

apply(vg_only_sales, 2, function(x) sum(is.na(x)))


# Exercise: apply

dice_tests <- read.csv(file = "dice_tests.csv", stringsAsFactors = FALSE, sep = ";", fileEncoding = "ISO8859-15")
sub_dice_tests <- dice_tests[, c(-1, -2)]
apply(sub_dice_tests, 2, function(x) quantile(x))
percent_dice_tests <- apply(sub_dice_tests, c(1, 2), "/", 100)









