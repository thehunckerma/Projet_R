source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "dplyr",
  "questionr",
  "rlist",
  "psy",
  "data.table",
  "mokken",
  "DescTools"
)
RequirePackages(packages)


df1 <- read_excel("data/dataset_clean_1.xlsx")

kendallTauB <- function(x) {
  KendallTauB(table(factor(x[[1]], ordered = TRUE), factor(x[[2]], ordered = TRUE)), conf.level = 0.95)
}
verbosity <- list(
  chi2 = FALSE,
  cramerv = FALSE
) # Wether or not to print the tests results in the console
df1_colnames <- colnames(df1) # List of dataframe column names
counter_ <- 0 # Counts the number Chi-square tests with p-value < 0.05
counter <- 0 # Counts the total number of the Chi-square tests
chi2_df <- data.frame(
  row1 = numeric(),
  row2 = numeric(),
  name1 = character(),
  name2 = character(),
  p_value = numeric()
) # Initialize the dataframe that stores data about the variables that got a p-value < 0.05

# Perform Chi-square test on every pair of variables from the dataframe
for (row in 1:ncol(df1)) {
  for (row_ in row:ncol(df1)) {
    # Convert to ordinal variable
    x <- df1[row] %>%
      as.matrix() %>%
      as.table() %>%
      factor()
    y <- df1[row_] %>%
      as.matrix() %>%
      as.table() %>%
      factor()

    if (length(summary(x)) < 2 | row == row_) next

    p_value <- chisq.test(table(x, y), simulate.p.value = TRUE)$p.value # Chi-square test of the selected variables

    if (p_value <= 0.05) {
      print(paste("There is a correlation between", df1_colnames[row], "and", df1_colnames[row_]))
      writeLines(paste("p-value =", p_value, "\n"))

      chi2_df <- rbind(chi2_df, list(
        row1 = row,
        row2 = row_,
        name1 = df1_colnames[row],
        name2 = df1_colnames[row_],
        p_value = p_value
      )) # Store the information in the dataframe for later use

      counter_ <- counter_ + 1
    }
    counter <- counter + 1
  }
}
print(paste("There are", counter_, "correlations out of", counter))
View(chi2_df)

# Initialize the dataframes that stores data about the variables depending on their cramer's v coeff value
cramerv_df_low <- data.frame(
  row1 = numeric(),
  row2 = numeric(),
  name1 = character(),
  name2 = character(),
  p_value = numeric(),
  cramer_v = numeric()
)
cramerv_df_moderate <- data.frame(
  row1 = numeric(),
  row2 = numeric(),
  name1 = character(),
  name2 = character(),
  p_value = numeric(),
  cramer_v = numeric()
)
cramerv_df_high <- data.frame(
  row1 = numeric(),
  row2 = numeric(),
  name1 = character(),
  name2 = character(),
  p_value = numeric(),
  cramer_v = numeric()
)

# Calculate Cramer's V coeff for every pair of variables that passed the Chi-square test
# >.5 high association
# .3 to .5 moderate association
# .1 to .3 low association
# 0 to .1 little if any association

for (row in 1:nrow(chi2_df)) {
  r <- chi2_df$row1[row]
  r_ <- chi2_df$row2[row]
  name1 <- chi2_df$name1[row]
  name2 <- chi2_df$name2[row]
  p_value <- chi2_df$p_value[row]

  x <- df1[r] %>%
    as.matrix() %>%
    as.table() %>%
    factor()
  y <- df1[r_] %>%
    as.matrix() %>%
    as.table() %>%
    factor()

  if (length(summary(x)) < 2 | length(summary(y)) < 2) next

  c <- suppressWarnings(cramer.v(table(x, y))) # Cramer's v coeff of the selected variables

  # Seperate the data depending on the coefficient's value
  if (c >= 0.5) {
    print(paste("There is a high correlation between", df1_colnames[r], "and", df1_colnames[r_]))
    cramerv_df_high <- rbind(cramerv_df_high, list(
      row1 = r,
      row2 = r_,
      name1 = name1,
      name2 = name2,
      p_value = p_value,
      cramer_v = c
    )) # Store the information in the dataframe for later use
  } else if (c >= 0.3 & c < 0.5) {
    print(paste("There is a moderate correlation between", df1_colnames[r], "and", df1_colnames[r_]))
    cramerv_df_moderate <- rbind(cramerv_df_moderate, list(
      row1 = r,
      row2 = r_,
      name1 = name1,
      name2 = name2,
      p_value = p_value,
      cramer_v = c
    )) # Store the information in the dataframe for later use
  } else if (c >= 0.1 & c < 0.3) {
    print(paste("There is a low correlation between", df1_colnames[r], "and", df1_colnames[r_]))
    cramerv_df_low <- rbind(cramerv_df_low, list(
      row1 = r,
      row2 = r_,
      name1 = name1,
      name2 = name2,
      p_value = p_value,
      cramer_v = c
    )) # Store the information in the dataframe for later use
  }
  writeLines(paste("Cramer's v coeff =", c, "\n"))
}
View(cramerv_df_moderate)
View(cramerv_df_low)
View(cramerv_df_high)
