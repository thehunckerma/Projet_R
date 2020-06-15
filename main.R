libs <- c("dplyr") # List of this script's dependencies
lapply(libs, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}) # Check if the package is installed otherwise install it, and import it after that

Dataset <- readXL("X:/R/Projet_R/dataset_clean_1.xlsx",
  rownames = FALSE,
  header = TRUE, na = "na",
  stringsAsFactors = TRUE
) # Import the dataset

# Let's split the data into 2 depending on whether the person have taken distance courses
# We filter the lines based on the value of E
# ( which is 1 if the person have taken distance courses and 2 otherwise )

# We exclude the E column as it becomes useless after the split
df1 <- select(Dataset[Dataset$E == 1, ], -E)

# We exclude all the columns except A, B, C, D because they contain NA values
df2 <- Dataset[Dataset$E == 2, 1:4] 

# **** Outliers treatement in column A ( Age ), as it is the only quantitative variable ****

boxplot(df1$A, data = df1, main = "Age", boxwex = 0.2) # Box plot of Age column in df1
# Based on the plot we can say that there is no outliers in the fisrt dataframe 

boxplot(df2$A, data = df2, main = "Age", boxwex = 0.2) # Box plot of Age column in df2
# We can tell from the plot that there is a potential outlier with value = 30 in the second dataframe

# Let's take a look at D value ( the school/faculty )
print(paste('D = ',df2[df2 == 30,]$D))
# So the school is FLSH UIT
# Therefore we can't tell if the value A = 30 is an outlier because there is no age limit in that faculty