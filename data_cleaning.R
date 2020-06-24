source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "stringi",
  "writexl",
  "dplyr",
  "tidyr",
  "stringr",
  "mvoutlier"
)

RequirePackages(packages) # if package is installed import it else install it and import it

initial_dataset <- read_excel("data/dataset.xlsx") # Import the initial dataset

# Column Splitting --------------------------------------------------------------------

# Split the F and G columns into multiple atomic columns each with a single value
dataset <- initial_dataset %>% separate(F, c("F1", "F2", "F3", "F4", "F5"), ", ") # Split the F column into 5 more columns
dataset <- dataset %>% separate(G, c("G1", "G2", "G3", "G4"), ", ") # Split the G column into 5 more columns

lapply(initial_dataset$F, function(x) { # For each line assign to each column its value based on the string in the initial column F
  c <- str_split(x, ", ")
  for (s in c("1", "2", "3", "4", "5")) {
    if (!is.na(x)) {
      if (stri_detect_fixed(x, s, fixed = TRUE)) {
        text <- paste("dataset$F", s, "[", parent.frame()$i[], "]<<-1", sep = "")
        print(text)
        eval(parse(text = text)) # oui
      } else {
        eval(parse(text = paste("dataset$F", s, "[", parent.frame()$i[], "]<<-2", sep = ""))) # non
      }
    }
  }
})

lapply(initial_dataset$G, function(x) { # For each line assign to each column its value based on the string in the initial column G
  c <- str_split(x, ", ")
  for (s in c("1", "2", "3", "4")) {
    if (!is.na(x)) {
      if (stri_detect_fixed(x, s, fixed = TRUE)) {
        text <- paste("dataset$G", s, "[", parent.frame()$i[], "]<<-1", sep = "")
        print(text)
        eval(parse(text = text)) # oui
      } else {
        eval(parse(text = paste("dataset$G", s, "[", parent.frame()$i[], "]<<-2", sep = ""))) # non
      }
    }
  }
})
# Numerical conversion -------------------------------------------------------------------
dataset <- mutate_all(dataset, function(x) as.numeric(as.character(x))) # Convert everything to numeric

# Multivariate Outlier Detection ---------------------------------------------------------

age_year <- data.frame(dataset$A, dataset$C) # Dataframe of Age and Year

outliers <- color.plot(
  age_year,
  quan = 0.5
)$outliers # Plot the euclidean distance as a color and return a boolean vector of outliers

print(age_year[unlist(outliers), ]) # Print the outliers

dataset <- dataset[unlist(!outliers), ] # Filter the outliers from the dataset

write_xlsx(dataset, "data/dataset_clean.xlsx") # Export the dataset for later use

# Splitting dataset -------------------------------------------------------------

# Split the data in two, depending on whether the person have taken distance courses or not
# Filter the lines based on the value of E ( which is 1 if the person have taken distance courses and 2 otherwise )

# Exclude the E column as it becomes useless after the split
df1 <- cbind(dataset[dataset$E == 1,1:4],dataset[dataset$E == 1,6:length(dataset)])
write_xlsx(df1, "data/dataset_clean_1.xlsx") # Export the dataset for later use

# Exclude all the columns except A, B, C, D because they contain NA values
df2 <- as.data.frame(dataset[dataset$E == 2, 1:4])
write_xlsx(df2, "data/dataset_clean_2.xlsx") # Export the dataset for later use

