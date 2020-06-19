source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "Hmisc"
)
RequirePackages(packages) # if package is installed import it else install it and import it

dataset <- read_excel("data/dataset_clean.xlsx") # Import the dataset

hist.data.frame(dataset[,1:5])
hist.data.frame(dataset[,15:26])
hist.data.frame(dataset[,27:37])
hist.data.frame(dataset[,38:48])
hist.data.frame(dataset[,49:59])
hist.data.frame(dataset[,60])
