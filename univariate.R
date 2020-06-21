source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "Hmisc",
  "moments",
  #"vcd",
  "grid",
  "ggplot2",
  "gridExtra",
  "purrr"
)
RequirePackages(packages) # if package is installed import it else install it and import it

dataset <- read_excel("data/dataset_clean.xlsx") # Import the dataset
dataset1 <- read_excel("data/dataset_clean_1.xlsx") # Import the dataset

#Max, min, mediane, mean, 1st and 3rd quantils, mode, variace, square distance, kurtosis, skewness, frequences
table(dataset$A)
summary(dataset$A)
quantile(dataset$A,prob=c(0.25,0.5,0.75))
Mode(dataset$A)
var(dataset$A)
sd(dataset$A)
kurtosis(dataset$A)
skewness(dataset$A)
histos[1]
shapiro.test(dataset$A) 

p_OnlineCourses = nrow(dataset1)/nrow(dataset) # Ratio of people who had online courses 
p_NoOnlineCourses = 1 - p_OnlineCourses # Ratio of people who didn't have online courses 
# Frequency
frequence=sapply(colnames(dataset[,2:length(dataset)]),function(x){table(dataset[x])})

# Pourcentage
pourcentage=sapply(lapply(frequence,"*",100),"/",nrow(dataset))

T=names(dataset)
T=set_names(x)
hist_fun = function(x) {
      ggplot(dataset, aes(x = .data[[x]]) ) + geom_histogram(color="darkblue",fill="white")
}
histos=lapply(T,hist_fun)

do.call(grid.arrange, histos[2:11])
do.call(grid.arrange, histos[12:21])
do.call(grid.arrange, histos[22:31])
do.call(grid.arrange, histos[32:41])
do.call(grid.arrange, histos[42:51])
do.call(grid.arrange, histos[52:60])
