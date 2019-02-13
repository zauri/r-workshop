# Apply ------------------------------------------------------------------------------------------------

setwd("C:/Users/wenzl/Downloads/R-Schulung/")
vg_sales <- read.csv("vgsales.csv", stringsAsFactors = FALSE)

throw_dice <- function(throws = 1, sides = 6) {
  if (sides <= 2) warning("this is no real dice, but I do it anyway :)")
  if (is.numeric(throws) == FALSE) stop("throws must be numeric")
  
  results <- sample(x = sides, size = throws, replace = TRUE)
  class(results) <- c("gamble", "character")
  return(results)
}


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


dice_tests <- read.csv(file = "dice_tests.csv", stringsAsFactors = FALSE, sep = ";", fileEncoding = "ISO8859-15")
sub_dice_tests <- dice_tests[, c(-1, -2)]
apply(sub_dice_tests, 2, function(x) quantile(x))
dice_result <- apply(sub_dice_tests, c(1, 2), "/", 100)

dice_result_list <- lapply(sub_dice_tests, quantile)
dice_result_simple <- sapply(sub_dice_tests, quantile)

dice_result_list <- lapply(sub_dice_tests, max)    # returns list
dice_result_simple <- sapply(sub_dice_tests, max)  # returns vector


# One Arm Bandit: Exercise

one_arm_bandit <- function(rounds = 10) {
  pictures <- (c("Sun", "Star", "Joker", "7"))
  result <- sample(pictures, 3, replace = TRUE)
  return(result)
}


bandit_results <- vector(mode = "list", length = 10000)

for (i in 1:10000) {
  bandit_results[[i]] <- one_arm_bandit(10000)
}

win_list <- lapply(bandit_results, function(x) {
  if ("Joker" %in% x) {
    return(sum("Joker" == x) * 100)
  } else if (all(c("Sun", "Sun", "Sun") == x)) {
    return(100)
  } else if (all(c("Star", "Star", "Star") == x)) {
    return(50)
  } else {
    return(0)
  }
}
) # lapply returns list


unlisted_win_list <- unlist(win_list)   # unlist returns vector


win_list <- sapply(bandit_results, function(x) {
  if ("Joker" %in% x) {
    return(sum("Joker" == x) * 100)
  } else if (all(c("Sun", "Sun", "Sun") == x)) {
    return(100)
  } else if (all(c("Star", "Star", "Star") == x)) {
    return(50)
  } else {
    return(0)
  }
}
) # sapply returns vector

mean(win_list)

win_list <- vapply(bandit_results, function(x) {
  if ("Joker" %in% x) {
    return(sum("Joker" == x) * 100)
  } else if (all(c("Sun", "Sun", "Sun") == x)) {
    return(100)
  } else if (all(c("Star", "Star", "Star") == x)) {
    return(50)
  } else {
    return(0)
  }
}, numeric(1))  # vapply defines expected output


tapply(vg_sales$Global_Sales, INDEX = vg_sales$Genre, mean)

tapply(vg_sales$Global_Sales, INDEX = list(vg_sales$Genre, vg_sales$Year), mean, na.rm = TRUE)

library(dplyr)
vg_sales[grepl("Zelda", vg_sales$Name), ] %>% select(Name, Global_Sales, Platform)

vg_sales %>%
  filter(grepl("Star Wars", Name)) %>%
  select(Name, Global_Sales, Platform, Year) %>%
  arrange(desc(Year))


# frequency of thrown sixes per dice table
tapply(dice_tests$sechs, INDEX = dice_tests$Tisch, quantile)
tapply(dice_tests$sechs, INDEX = dice_tests$Tisch, function(x) {
  mean(x/10000)
}) # compute relative frequency



# Profiling --------------------------------------------------------------------------------------------

test_df <- data.frame(matrix(sample(1:1500, 1500, replace = TRUE), nrow = 15000, ncol = 1500))
colnames(test_df) <- paste0("Spalte", 1:1500)

library(microbenchmark)

kennzahlen <- vector(mode = "numeric", 1500)

microbenchmark(
for (i in seq_along(colnames(test_df)))
  kennzahlen[i] <- sum(test_df[, 1])
)

microbenchmark(
  kennzahlen_2 <- vapply(test_df, FUN = sum, integer(1))
)

microbenchmark(
  kennzahlen_3 <- colSums(test_df)
)



create_vec <- function(x) {
  out <- numeric(length = x)
  for (i in 1:x) {
    out[i] <- runif(1, 0, 100)
  }
  out <- sqrt(out)
  out <- ifelse(out > 7, out, NA)
  out <- unique(out)
  out <- as.factor(out)
  return(out)
  
}



Rprof("test_create_vec.Rprof")
create_vec(10000)
Rprof(NULL)

summaryRprof("test_create_vec.Rprof")


install.packages("profvis")
library(profvis)

profvis(create_vec(1000000))


# Exercise: profile one_arm_bandit

write_one_arm_bandits_results <- function(n) {
  results <- list()
  
  for (i in 1:n) {
    results <- c(results, one_arm_bandit())
  }
  return(results)
}

results <- write_one_arm_bandits_results(10000)
profvis(write_one_arm_bandits_results(1000))

# improved (less copying :) )

write_one_arm_bandits_results <- function(n) {
  results <- vector(mode = "list", n)
  
  for (i in 1:n) {
    results[[i]] <- one_arm_bandit()
  }
  return(results)
}

profvis( {
  write_one_arm_bandits_results(10000)
  })



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






