source("helpers.R")

packages <- c("readxl") # List of this script's dependencies
RequirePackages(packages) # if package is installed import it else install it and import it

path <- "X:/R/Projet_R/dataset_clean_2.xlsx" # Dataset path
Dataset <- read_excel(path) # Import the dataset

hist()