source("helpers.R")

packages <- c( # List of this script's dependencies
  "dplyr",
  "readxl",
  "tidyr",
  "stringr"
)
RequirePackages(packages) # if package is installed import it else install it and import it

dataset <- read_excel("data/dataset_clean.xlsx") # Import the dataset
