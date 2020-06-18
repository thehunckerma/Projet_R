source("helpers.R")

packages <- c("readxl") # List of this script's dependencies
RequirePackages(packages) # if package is installed import it else install it and import it

dataset <- read_excel("data/dataset.xlsx") # Import the dataset

hist(dataset)