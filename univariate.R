source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "Hmisc",
  "moments"
)
RequirePackages(packages) # if package is installed import it else install it and import it

dataset <- read_excel("data/dataset_clean.xlsx") # Import the dataset
attach(dataset)

#Max, min, mediane, mean, 1st and 3rd quantils
summary(A)
kurtosis(A)
skewness(A)

#frequence
a=lapply(colnames(dataset[,2:lenght(dataset)]),function(x)  table(dataset[x]))

hist.data.frame(dataset[,1:5])
hist.data.frame(dataset[,15:26])
hist.data.frame(dataset[,27:37])
hist.data.frame(dataset[,38:48])
hist.data.frame(dataset[,49:59])
hist.data.frame(dataset[,60])
