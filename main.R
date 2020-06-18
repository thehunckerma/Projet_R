source("helpers.R")

packages <- c(
  "dplyr",
  "readxl",
  "tidyr",
  "stringr"
) # List of this script's dependencies
RequirePackages(packages) # if package is installed import it else install it and import it

path <- "X:/R/Projet_R/dataset_clean_2.xlsx" # Dataset path
Dataset <- read_excel(path) # Import the dataset

# **** Outliers treatement in column A ( Age ), as it is the only quantitative variable ****

if (FALSE) { # Ignore this block of code
  boxplot(df1$A, data = df1, main = "Age", boxwex = 0.2) # Box plot of Age column in df1
  # Based on the plot we can say that there is no outliers in the fisrt dataframe

  boxplot(df2$A, data = df2, main = "Age", boxwex = 0.2) # Box plot of Age column in df2
  # We can tell from the plot that there is a potential outlier with value = 30 in the second dataframe

  # Let's take a look at D value ( the school/faculty )
  print(paste("D = ", df2[df2 == 30, ]$D))
  # So the school is FLSH UIT
  # Therefore we can't tell if the value A = 30 is an outlier because there is no age limit in that faculty
}
