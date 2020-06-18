rm(list = ls()) # Clear the environment's variables
RequirePackages <- function(packages) { # if package is installed import it else install it and import it
  lapply(packages, function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
}
