source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "Hmisc",
  "moments"
)
RequirePackages(packages) # if package is installed import it else install it and import it

dataset <- read_excel("data/dataset_clean.xlsx") # Import the dataset
attach(dataset)
dataset1 <- read_excel("data/dataset_clean_1.xlsx") # Import the dataset
attach(dataset1)
#Max, min, mediane, mean, 1st and 3rd quantils, variace, square distance, kurtosis, skewness, frequences
summary(A)
var(A)
sd(A)
kurtosis(A) # 
skewness(A)
table(A)
hist(A)
shapiro.test(A) 

p_OnlineCourses = nrow(dataset1)/nrow(dataset) # Ratio of people who had online courses 
p_NoOnlineCourses = 1 - p_OnlineCourses # Ratio of people who didn't have online courses 

# Frequency
frequence=lapply(colnames(dataset[,2:length(dataset)]),function(x){
    table(dataset[x])
})
# Pourcentage
pourcentage=lapply(lapply(frequence,"*",100),"/",nrow(dataset))


hist.data.frame(dataset[,2:12])
hist.data.frame(dataset[,13:23])
hist.data.frame(dataset[,24:34])
hist.data.frame(dataset[,35:45])
hist.data.frame(dataset[,46:56])
hist.data.frame(dataset[,57:60])
