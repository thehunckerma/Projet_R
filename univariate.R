source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "Hmisc",
  "moments"
)
RequirePackages(packages) # if package is installed import it else install it and import it

dataset <- read_excel("data/dataset_clean.xlsx") # Import the dataset
attach(dataset)

#Max, min, mediane, mean, 1st and 3rd quantils, variace, square distance, kurtosis, skewness, frequences
summary(A)
var(A)
sd(A)
kurtosis(A) # 
skewness(A)
table(A)
hist(A)
shapiro.test(A) 

#frequence
frequence=lapply(colnames(dataset[,2:length(dataset)]),function(x)  table(dataset[x]))
names(frequence)=


hist.data.frame(dataset[,1:5])
hist.data.frame(dataset[,15:26])
hist.data.frame(dataset[,27:37])
hist.data.frame(dataset[,38:48])
hist.data.frame(dataset[,49:59])
hist.data.frame(dataset[,60])
