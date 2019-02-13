# Funktionen ---------------------------------------------------------------------------------------

func1 <- function(x, Add = 1) { 
  result <- x + Add
  return(result)
}

#default = 1

# Datenstrukturen-----------------------------------------------------------------------------------

# Vektoren------------------------------------------------------------------------------
# Indizieren: Vektoren

vec1 <- vector(mode = "numeric", length = 10)
set.seed(42)
vec2 <- sample(x = c(1:10), size = 15, replace = TRUE)

vec2[1]
vec2[2:5]
vec2[vec2 < 4]
which(vec2 == 9) # an welcher Stelle Zahl x
vec2[-4] # Stelle 4 nicht ausgeben


# Dataframes-------------------------------------------------------------------------------------------

my_df <- data.frame(Zahl = c(1,2,3),
                    Text = c("A","B","C"))

options()
# options(stringsAsFactors = FALSE)
getOption("stringsAsFactors")

colnames(my_df)  # Spaltennamen anzeigen
row.names(my_df) # Reihennamen anzeigen

colnames(my_df)[2] <- "Character"

# Indizieren: Dataframes

my_df[1,2]
my_df[c(1,2), 2] # Zeilen 1 & 2, Spalte 2
my_df[,2]
my_df[3,]
my_df[,"Zahl"]

my_df$Zahl # nur bei Spaltennamen; nur eine Spalte auswählbar


exercise_df <- data.frame(Name = c("Jochen", "Anna", "Erik", "Sebastian", "Sabrina"),
                          Alter = c(12, 29, 41, 57, 23),
                          Geschlecht = factor(c(1, 2, 1, 1, 2),
                                              labels = c("männlich", "weiblich"),
                                              levels = c(1, 2)
                                              ),
                          Tag = c(1, 3, 5, 7, 2),
                          Woche = c(4, 2, 8, 11, 3)
                          )

exercise_df[2,3] # Zeile 2, Spalte 3
exercise_df[,1] # gesamte Spalte 1
exercise_df[c(1,2), c(3,4)] # Zeile 1 + 2 und Spalte 3 + 4
exercise_df[-3,5] # Werte Spalte 5 ohne Zeile 3
exercise_df[, "Alter"] # Werte für Alter

df_new <- data.frame(exercise_df[,c("Name", "Alter")]) # neuer Dataframe mit nur Name + Alter-Spalten
df_new[df_new < 20] # alle Werte kleiner 20


# Listen-----------------------------------------------------------------------------------------------

my_list <- list(Vector = c(2, 3, 5),
                Dataframe = df_new,
                Vector2 = c(2, 5, 7, 9, 11),
                Liste = list(
                  Vector = c(1, 5),
                  Vector2 = c(8, 5)
                 )
                )

# Indizieren: Listen

my_list[["Vector"]][3]
my_list[["Dataframe"]][, "Alter"][2:4]
my_list[["Liste"]][["Vector2"]][1]

my_list$Liste$Vector2

# doppelt eckige Klammern: Liste, einfach eckige: Dataframe (class) für bspw. Teillisten

class(my_list[["Dataframe"]]) # data.frame
class(my_list["Dataframe"])   # list

names(my_list)


list2 <- list(
  "Name" = c("Achim", "Ulrich", "Susanne", "Yvonne"),
  "Alter" = c(25, 36, 33, 42),
  "Land" = c("DE", "DE", "UK", "AT"),
  "DF" = exercise_df)

list2[[2]]
list2[[3]][4]
list2[[4]][, 3]
list2[[4]][4, 2]
list2[["Alter"]]


# Matrizen--------------------------------------------------------------------------------------------

mat <- matrix(1:16, nrow = 4, byrow = TRUE)

# Zugriff wie bei Dataframe

mat[9]


# Mengenoperatoren------------------------------------------------------------------------------------

vec1 <- c(5, 2, 3, 6, 4)
vec2 <- c(1, 8, 5, 3, 9)

union(vec1, vec2)          # Vereinigung (keine Dopplungen)
intersect(vec1, vec2)      # Schnittmenge
setdiff(vec1, vec2)        # Differenz vec1 zu vec2
setequal(vec1, vec2)       # gleichwertig?

is.element(vec1, vec2)
vec1 %in% vec2             # einzelner Wert irgendwo in vec2?


# Übung (S. 39)
x <- sample(1:50, 200, replace = TRUE)
y <- sample(3:55, 100, replace = TRUE)

length(union(x, y)) # 54
length(c(x, y))      # 300
intersect(x, y)
all(c(5, 12, 44, 50) %in% intersect(x, y))

setdiff(union(x, y), intersect(x, y))
union(setdiff(x, y), setdiff(y, x))




