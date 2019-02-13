getwd()


# Packages -------------------------------------

# export in function file determines public/private: with export public, rest private

library(devtools)

create("LasVegas", rstudio = FALSE)

document("LasVegas")

devtools::use_vignette("LasVegas")


# testthat ----------------------------------------------------

library(testthat)

devtools::use_testthat()

expect_equal(5, 5)
expect_equal(2, 2L)

expect_identical(2, 2L)
expect_identical(2, 2)



# Classes: R6 -------------------------------------------------
# https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html

library(R6)

# new class with public members

Person <- R6Class("Person",
                  public = list(
                    name = NULL,
                    hair = NULL,
                    initialize = function(name = NA, hair = NA) {
                      self$name <- name
                      self$hair <- hair
                      self$greet()
                    },
                    set_hair = function(val) {
                      self$hair <- val
                    },
                    greet = function() {
                      cat(paste0("Hello, my name is ", self$name, ".\n"))
                    }
                  )
)

ann <- Person$new("Ann", "black")


# new class with private members

Queue <- R6Class("Queue",
                 public = list(
                   initialize = function(...) {
                     for (item in list(...)) {
                       self$add(item)
                     }
                   },
                   add = function(x) {
                     private$queue <- c(private$queue, list(x))
                     invisible(self)
                   },
                   remove = function() {
                     if (private$length() == 0) return(NULL)
                     # Can use private$queue for explicit access
                     head <- private$queue[[1]]
                     private$queue <- private$queue[-1]
                     head
                   }
                 ),
                 private = list(
                   queue = list(),
                   length = function() base::length(private$queue)
                 )
)

q <- Queue$new(5, 6, "foo")










