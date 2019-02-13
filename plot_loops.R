# Load Data------------------------------------------------------------------


setwd("r-workshop/") # set working directory
getwd()

cars <- read.csv(file = "cars.csv")


# Overview--------------------------------------------------------------
head(cars, n = 10) # show first n rows
str(cars)          # structure
summary(cars)      # summary + some statistics

install.packages("Hmisc")
library(Hmisc)

levels(cars[, "Type"])
levels(cars[, "Continent"])

describe(cars) # overview for dataset, needs package Hmisc


# Change datatypes-----------------------------------------------------
cars[, "Modell"] <- as.character(cars[, "Modell"]) # change factor to char (154 levels...)
cars[, "Sales"] <- cars[, "Sales"] * 1000 # numbers didn't make sense

# Add new variable
cars[, "HP_3cl"] <- NA
cars[, "HP_3cl"][cars[, "HP"] < 155] <- 1 
cars[, "HP_3cl"][cars[, "HP"] >= 155 & cars[, "HP"] < 210] <- 2
# any(is.na(cars[, "HP_3cl"]))
cars[, "HP_3cl"][cars[, "HP"] >= 210] <- 3

# Search for NA values in HP_3cl
critical_row <- which(is.na(cars[, "HP_3cl"]))
cars[critical_row, ]

cars <- cars[-critical_row, ]     # delete row where HP_3cl is NA
# cars <- cars[!is.na(cars[, "HP_3cl"]), ]  # alternative

# Add labels + factors for HP_3cl
cars[, "HP_3cl"] <- factor(cars[, "HP_3cl"],
                           levels = 1:3,
                           labels = c("unter 155 PS", "155-210 PS", "mehr als 210 PS"))


cars[, 15:21] <- cbind(cars[, 21], cars[, 15:20]) # row 21 -> row 15, 15-20 after
colnames(cars)[15:21] <- c(colnames(cars[21]), colnames(cars[, 15:20])) # change colnames


# Exercise

cars_age <- 2017 - cars[, "Year"]
# cars_age <- cars_age(as.numeric(format(Sys.Date(), "%Y"))
cars_value <- cars[, "Purchase_price"] - cars[, "Resaleprice"]
cars[, "Ranking"] <- cars[, "Ranking"] * 0.85
cars[, "Width"] <- cars[, "Width"] * 2.54
cars_sales <- sum(cars[, "Sales"])


# Save data-----------------------------------------------------------------------------------------

write.table(x = cars, file = "write_cars.csv",
            sep = ";",
            dec = ".",
            col.names = TRUE,
            row.names = FALSE,
            na = "Auto")


save.image(file = "workspace_cars.RData")
# save(list = c("cars", "cars_age"), file = "workspace_cars.RData")

load(file = "workspace_cars.RData")


# Data manipulation
PKW <- cars[cars[, "Type"] == "PKW",]
PKW <- subset(cars, subset = (Type == "PKW"))



# Pipe Operator------------------------------------------------------------------------------------
mean(unique(c(2, 3, 2, 4)))

c(2, 3, 2, 4) %>% unique() %>% mean()

cars %>% select("HP", "HP_3cl", "Type") %>% filter(HP >= 210) %>% filter(Type == "PKW")

cars <- read.csv(file = "cars.csv", header = TRUE, sep =",", dec = ".")
cars_new <- read.csv(file = "carsNew.csv", header = TRUE, sep = ",", dec = ".")

joint_cars <- full_join(cars, cars_new)


# group_by, mutate

# Exercise
# cars_ger <- subset(cars, subset = (Country == "Germany"))
cars %>% filter(Country == "Germany") -> cars_ger
cars %>% select(Country, HP) -> country_HP
cars %>% select(Manufacturer, HP) %>% filter(Manufacturer == "Ford") -> ford_cars
cars %>% filter(Manufacturer == "Porsche" | HP > 250) -> porsche_250


# Statistics-----------------------------------------------------------------------------------------

cont_table <- table(cars[, "Continent"])
cars[, "Continent"] %>% table() %>% prop.table() %>% cumsum()

# Graphs---------------------------------------------------------------------------------------------

png(filename = "testplot.png")
plot(x = cars[, "HP"],
     y = cars[, "Purchase_price"],
     main = "Testplot",
     xlab = "HP",
     ylab = "Purchase price",
     sub = "Subtitle",
     col = c("darkcyan", "green"),
     type = "o",
     pch = 1,
     lwd = 1.5,
     xlim = c(50, 400),
     ylim = c(5, 95))
dev.off()


barplot(table(cars[, "Continent"]),
              horiz = TRUE,
              las = 1,
              legend = TRUE,
              col = c("darkcyan", "green", "firebrick1"))

table(cars[, "Continent"], cars[, "Type"])

barplot(table(cars[, "Continent"], cars[, "Type"]),
        las = 1,
        legend = TRUE,
        beside = TRUE,
        ylim = c(0, 100),
        col = c("darkcyan", "green", "firebrick1"))


boxplot(cars[, "HP"])
quantile(cars[, "HP"], na.rm = TRUE)

hist(cars[, "HP"], breaks = 20)
par(mfrow = c(1, 2))
hist(cars[, "HP"])
hist(cars[, "HP"], breaks = 20)

par(mfrow = c(1, 1))
split.screen(figs = c(1, 2))
screen(2)
hist(cars[, "HP"], col = c("ivory3", "cyan", "lightgreen"), main = "Histogram: HP of cars")


# Loops----------------------------------------------------------------------------------------------

for (i in seq_along(1:nrow(cars))) {
  print(i)
}


i <- 1
while (i < 10) {
  i <- i - 1
}


# if else -------------------------------------------------------------------------------------------

i <- c(1, 3, 20)
if(i < 10) {
  print(i)
} else {
  print("i is bigger than 10")
}


ifelse(i < 10, i, "i bigger than 10")


# for with next -------------------------------------------------------------------------------------

for (i in 1:10) {
  if(i == 5) {
    next # skip loop
  }
  print(i)
}

# break + while -------------------------------------------------------------------------------------
i <- 8
while (i < 10) {
  i <- i - 1
  if(i < 0) {
    break
  }
}


# repeat -------------------------------------------------------------------------------------------
# only ends with break

repeat{
  i <- i + 1
  if(i == 100) {
    break
  }
}


# apply --------------------------------------------------------------------------------------------

apply_df <- data.frame(x = sample(x = 1:100, size = 50, replace = FALSE),
                       y = sample(x = 1:100, size = 50, replace = FALSE),
                       z = sample(x = 1:100, size = 50, replace = FALSE))


apply(X = apply_df, MARGIN = 2, FUN = mean) # calculate mean for every column
apply(X = apply_df, MARGIN = 1, FUN = mean) # calculate mean for every row


lapply(X = apply_df, FUN = mean) # lapply: lists and dataframes, no margin necessary, returns list?

sapply(X = apply_df, FUN = mean) # list or vector, like lapply, but no fixed output

mapply(FUN = mean, c(apply_df, data.frame(1:10))) # multiple lists or vectors

tapply(X = cars[, c("HP")], FUN = mean, INDEX = cars[, "Type"])

cars_new <- na.omit(cars)

