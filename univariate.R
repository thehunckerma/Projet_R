source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "Hmisc",
  "moments",
  "vcd"
)
RequirePackages(packages) # if package is installed import it else install it and import it

dataset <- read_excel("data/dataset_clean.xlsx") # Import the dataset

df1 <- read_excel("data/dataset_clean_1.xlsx") # Import the dataset

dataset1 <- read_excel("data/dataset_clean_1.xlsx") # Import the dataset

#Max, min, mediane, mean, 1st and 3rd quantils, mode, variace, square distance, kurtosis, skewness, frequences

summary(dataset$A)
Mode(dataset$A)
var(dataset$dataset$A)
sd(dataset$A)
kurtosis(dataset$A) # 
skewness(dataset$A)
table(dataset$A)
hist(dataset$A)
shapiro.test(dataset$A) 

p_OnlineCourses = nrow(dataset1)/nrow(dataset) # Ratio of people who had online courses 
p_NoOnlineCourses = 1 - p_OnlineCourses # Ratio of people who didn't have online courses 
# Frequency
frequence=lapply(colnames(dataset[,2:length(dataset)]),function(x){table(dataset[x]))
# Pourcentage
pourcentage=lapply(lapply(frequence,"*",100),"/",nrow(dataset))


hist.data.frame(dataset[,2:12])
hist.data.frame(dataset[,13:23])
hist.data.frame(dataset[,24:34])
hist.data.frame(dataset[,35:45])
hist.data.frame(dataset[,46:56])
hist.data.frame(dataset[,57:60])
