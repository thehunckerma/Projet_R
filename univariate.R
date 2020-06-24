source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "DescTools", # Mode
  "moments", # kurtosis, skewness
  "ggplot2", # use ggplot geom_histogram
  "grid", # grid & gridExtra => grid.arrange
  "gridExtra",
  "rlist", # list.filter
  "purrr" # use set_names
)
RequirePackages(packages) # if package is installed import it else install it and import it

dataset <- read_excel("data/dataset_clean.xlsx") # Import the dataset
dataset1 <- read_excel("data/dataset_clean_1.xlsx") # Import the dataset
d_ordinal <- dataset1[!(names(dataset1) %in% c( # Ordinal variables ( Likert Scale )
  "A",
  "B",
  "C",
  "D",
  "H",
  "J",
  "L1",
  "L2",
  "F1",
  "F2",
  "F3",
  "F4",
  "F5",
  "G1",
  "G2",
  "G3",
  "G4"
))]

T <- set_names(names(dataset))


# Max, min, mediane, mean, 1st and 3rd quantils, mode, variace, square distance, kurtosis, skewness, frequences, histogram, normality test
table(dataset$A)
summary(dataset$A)
quantile(dataset$A, prob = c(0.25, 0.5, 0.75))
Mode(dataset$A)
var(dataset$A)
sd(dataset$A)
kurtosis(dataset$A)
skewness(dataset$A)

histos <- c(
  lapply(T[1:5], function(x) {
    ggplot(dataset[1:5], aes(x = .data[[x]])) +
      geom_histogram(color = "darkblue", fill = "white", binwidth = NULL)
  }),
  lapply(T[6:length(dataset)], function(x) {
    ggplot(dataset1[5:length(dataset1)], aes(x = .data[[x]])) +
      geom_histogram(color = "darkblue", fill = "white", binwidth = NULL)
  })
)
histos[1]

shapiro.test(dataset$A)

p_OnlineCourses <- nrow(dataset1) / nrow(dataset) # Ratio of people who had online courses
print(p_OnlineCourses)
p_NoOnlineCourses <- 1 - p_OnlineCourses # Ratio of people who didn't have online courses
print(p_NoOnlineCourses)

# Ordinal -----------------------------------------
# Frequencies
frequence <- sapply(colnames(dataset[, 2:length(dataset)]), function(x) {
  table(dataset[x])
})
print(frequence)

# Percentage
pourcentage <- sapply(lapply(frequence, "*", 100), "/", nrow(dataset))
print(pourcentage)
normalityTest <- apply(d_ordinal, 2, shapiro.test) # applying normality test on all d_ordinal elements
normalVarCount <- length(list.filter(normalityTest, normalityTest[[.i]]$p.value > 0.05)) # filtering result test by p-value
cat("in ", length(d_ordinal), " variables, there are ", normalVarCount, " with no difference between their evolution and the normal distribution")

# Plot grids of frequency histograms
do.call(grid.arrange, histos[2:13])
do.call(grid.arrange, histos[14:25])
do.call(grid.arrange, histos[26:37])
do.call(grid.arrange, histos[38:49])
do.call(grid.arrange, histos[50:60])
