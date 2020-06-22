source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "dplyr",
  "questionr",
  "psy",
  "data.table",
  "mokken",
  "DescTools", # KendallTauB
  "ltm" # biserial.cor
)
RequirePackages(packages)

d <- suppressWarnings(read_excel("data/dataset_clean_1.xlsx"))
d_colnames <- colnames(d) # List of dataframe column names

verbosity <- list(
  continuous_ordinal = TRUE,
  continuous_dichotomous = TRUE,
  chi2 = FALSE,
  cramerv = FALSE
) # Wether or not to print the tests results in the console

# Split data by variable type --------------------------------------------------------

d_dichotomous <- cbind( # Dichotomous variables ( yes/no questions )
  B = d$B,
  d[, 5:13], # F and G
  L1 = d$L1,
  L2 = d$L2
)
d_nominal <- cbind( # Nominal variables ( unordered )
  C = d$C,
  D = d$D,
  H = d$H,
  J = d$J
)
d_ordinal <- d[, !(names(d) %in% c( # Ordinal variables ( Likert Scale )
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

Age <- d$A # The age is the only continuous variable

# *************** Continuous-ordinal *******************

continuous_ordinal_df <- data.frame(
  col1 = numeric(),
  col2 = numeric(),
  name1 = character(),
  name2 = character(),
  tau_b = numeric()
) # Initialize the dataframe that stores data (including Tau-b) variables that got a p-value < 0.05

colnames_ordinal <- colnames(d_ordinal)

for (col in 1:ncol(d_ordinal)) { # Iterate over the ordinal variables dataframe 
  x_ordered <- d_ordinal[col] %>%
    as.matrix() %>%
    as.table() %>%
    factor(
      ordered = TRUE,
      levels = c(1, 2, 3, 4, 5)
    ) # Convert each row to an ordered factor with 5 levels

  tau_b <- KendallTauB(
    table(Age, x_ordered), # Correlate Age with the other ordinal variables
    conf.level = 0.95
  )["tau_b"] # Kendall's Tau-b coeff to measure the direction and strength of the association

  if (!is.nan(tau_b) && abs(tau_b) >= 0.2) { # Check if tau_b is a number
    if (verbosity["continuous_ordinal"] == TRUE) { # Verbosity
      if (tau_b > 0) {
        sign <- "positive"
      } else {
        sign <- "negative"
      }
      cat(
        "There is a", sign, "correlation between",
        d_colnames[1], "and", colnames_ordinal[col], "\n",
        "tau-b =", tau_b, "\n\n"
      )
    }
    continuous_ordinal_df <- rbind(continuous_ordinal_df, list(
      col1 = 1,
      col2 = col,
      name1 = d_colnames[1],
      name2 = colnames_ordinal[col],
      tau_b = tau_b
    )) # Store the information in the dataframe
  }
}

if (verbosity["continuous_ordinal"] == TRUE) { # Verbosity
  print(continuous_ordinal_df)
  cat("\n")
  for (row in 1:nrow(continuous_ordinal_df)) {
    if (continuous_ordinal_df$tau_b[row] > 0) {
      sign <- "positivement"
    } else {
      sign <- "negativement"
    }
    paste(
      map[continuous_ordinal_df$name1[row]],
      "correle", sign, "avec",
      map[continuous_ordinal_df$name2[row]]
    ) %>%
      print() # Print a formated version of conclusion
  }
}

# ******** Continuous-dichotomous *************  --------------------------------------------

continuous_dichotomous_df <- data.frame(
  col1 = numeric(),
  col2 = numeric(),
  name1 = character(),
  name2 = character(),
  biserial = numeric()
) # Initialize the dataframe that stores data

colnames_dichotomous <- colnames(d_dichotomous)

for (col in 1:ncol(d_dichotomous)) { # Loop over the dichotomous variables dataframe
  # Convert to ordinal variable

  x <- d_dichotomous[col] %>%
    as.matrix() %>%
    as.table() %>%
    factor() # Convert each row to factor

  biserial <- biserial.cor(Age, x) # Mesure the Point-Biserial correlation coefficient
  
  if (!is.nan(biserial) & abs(biserial) >= 0.2) { # Check if the biserial coeff is a number
    if (verbosity["continuous_dichotomous"] == TRUE) { # Verbosity
      if (biserial > 0) {
        sign <- "positive"
      } else {
        sign <- "negative"
      }
      cat(
        "There is a", sign, "correlation between",
        d_colnames[1], "and", colnames_dichotomous[col], "\n",
        "biserial =", biserial, "\n\n"
      )
    }
    continuous_dichotomous_df <- rbind(continuous_dichotomous_df, list(
      col1 = 1,
      col2 = col,
      name1 = d_colnames[1],
      name2 = colnames_dichotomous[col],
      biserial = biserial
    )) # Store the information in the dataframe
  }
}

if (verbosity["continuous_dichotomous"] == TRUE) { # Verbosity
  print(continuous_dichotomous_df)
  cat("\n")
  for (row in 1:nrow(continuous_dichotomous_df)) {
    if (continuous_dichotomous_df$biserial[row] > 0) {
      sign <- "positivement"
    } else {
      sign <- "negativement"
    }
    paste(
      map[continuous_dichotomous_df$name1[row]],
      "correle", sign, "avec",
      map[continuous_dichotomous_df$name2[row]]
    ) %>%
      print() # Print a formated version of conclusion
  }
}

# Eperimental ------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

counter_ <- 0 # Counts the number Chi-square tests with p-value < 0.05
counter <- 0 # Counts the total number of the Chi-square tests
chi2_df <- data.frame(
  col1 = numeric(),
  col2 = numeric(),
  name1 = character(),
  name2 = character(),
  p_value = numeric()
) # Initialize the dataframe that stores data about the variables that got a p-value < 0.05

# Perform Chi-square test on every pair of variables from the dataframe
for (col in 1:ncol(d)) {
  for (col_ in col:ncol(d)) {
    # Convert to ordinal variable
    x <- d[col] %>%
      as.matrix() %>%
      as.table() %>%
      factor()
    y <- d[col_] %>%
      as.matrix() %>%
      as.table() %>%
      factor()

    if (
      length(summary(x)) < 2 # Chi-square requires the contingency table to be 2x2
      | col == col_
    ) {
      next
    }

    p_value <- chisq.test(table(x, y), simulate.p.value = TRUE)$p.value # Chi-square test of the selected variables

    if (p_value <= 0.05) {
      if (verbosity["chi2"] == TRUE) { # Verbosity
        cat("There is a correlation between", d_colnames[col], "and", d_colnames[col_])
        writeLines("p-value =", p_value, "\n")
      }

      chi2_df <- rbind(chi2_df, list(
        col1 = col,
        col2 = col_,
        name1 = d_colnames[col],
        name2 = d_colnames[col_],
        p_value = p_value
      )) # Store the information in the dataframe for later use

      counter_ <- counter_ + 1
    }
    counter <- counter + 1
    cat("\r Performing test n°", counter, " out of ~1620 tests") # Information about the loop
  }
}

cat("There are", counter_, "correlations out of", counter)

if (verbosity["chi2"] == TRUE) { # Verbosity
  View(chi2_df)
}

# Initialize the dataframes that store data about the variables based on their cramer's v coeff value
cramerv_df_low <- data.frame(
  col1 = numeric(),
  col2 = numeric(),
  name1 = character(),
  name2 = character(),
  p_value = numeric(),
  cramer_v = numeric()
)
cramerv_df_moderate <- data.frame(
  col1 = numeric(),
  col2 = numeric(),
  name1 = character(),
  name2 = character(),
  p_value = numeric(),
  cramer_v = numeric()
)
cramerv_df_high <- data.frame(
  col1 = numeric(),
  col2 = numeric(),
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
  col1 <- chi2_df$col1[row]
  col2 <- chi2_df$col2[row]
  name1 <- chi2_df$name1[row]
  name2 <- chi2_df$name2[row]
  p_value <- chi2_df$p_value[row]

  x <- d[col1] %>%
    as.matrix() %>%
    as.table() %>%
    factor()
  y <- d[col2] %>%
    as.matrix() %>%
    as.table() %>%
    factor()

  if (length(summary(x)) < 2) next # Cramer's v requires the contingency table to be 2x2 or bigger

  cramer_v <- suppressWarnings(cramer.v(table(x, y))) # Cramer's v coeff of the selected variables

  # Seperate the data depending on the coefficient's value
  if (cramer_v >= 0.5) {
    if (verbosity["cramerv"] == TRUE) { # Verbosity
      cat("There is a high correlation between", d_colnames[r], "and", d_colnames[r_])
    }
    cramerv_df_high <- rbind(cramerv_df_high, list(
      col1 = col1,
      col2 = col2,
      name1 = name1,
      name2 = name2,
      p_value = p_value,
      cramer_v = cramer_v
    )) # Store the information in the dataframe for later use
  } else if (cramer_v >= 0.3 & cramer_v < 0.5) {
    if (verbosity["cramerv"] == TRUE) { # Verbosity
      cat("There is a moderate correlation between", d_colnames[r], "and", d_colnames[r_])
    }
    cramerv_df_moderate <- rbind(cramerv_df_moderate, list(
      col1 = col1,
      col2 = col2,
      name1 = name1,
      name2 = name2,
      p_value = p_value,
      cramer_v = cramer_v
    )) # Store the information in the dataframe for later use
  } else if (cramer_v >= 0.1 & cramer_v < 0.3) {
    if (verbosity["cramerv"] == TRUE) { # Verbosity
      cat("There is a low correlation between", d_colnames[r], "and", d_colnames[r_])
    }
    cramerv_df_low <- rbind(cramerv_df_low, list(
      col1 = col1,
      col2 = col2,
      name1 = name1,
      name2 = name2,
      p_value = p_value,
      cramer_v = cramer_v
    )) # Store the information in the dataframe for later use
  }
  if (verbosity["cramerv"] == TRUE) { # Verbosity
    writeLines("Cramer's v coeff =", cramer_v, "\n")
  }
  cat("\r Performing test n°", row, "/", nrow(chi2_df)) # Information about the loop
}

if (verbosity["cramerv"] == TRUE) { # Verbosity
  View(cramerv_df_moderate)
  View(cramerv_df_low)
  View(cramerv_df_high)
}


tauB_moderate <- data.frame(
  col1 = numeric(),
  col2 = numeric(),
  name1 = character(),
  name2 = character(),
  p_value = numeric(),
  cramer_v = numeric(),
  tauB = numeric()
)
for (row in 1:nrow(cramerv_df_moderate)) {
  col1 <- cramerv_df_moderate$col1[row]
  col2 <- cramerv_df_moderate$col2[row]
  name1 <- cramerv_df_moderate$name1[row]
  name2 <- cramerv_df_moderate$name2[row]
  p_value <- cramerv_df_moderate$p_value[row]
  cramer_v <- cramerv_df_moderate$cramer_v[row]

  x <- d[col1] %>%
    as.matrix() %>%
    as.table() %>%
    factor()
  y <- d[col2] %>%
    as.matrix() %>%
    as.table() %>%
    factor()
  tauB <- KendallTauB(table(x, y), conf.level = 0.95)["tau_b"]
  print(tauB)
  tauB_moderate <- rbind(tauB_moderate, list(
    col1 = col1,
    col2 = col2,
    name1 = name1,
    name2 = name2,
    p_value = p_value,
    cramer_v = cramer_v,
    tauB = tauB
  )) # Store the information in the dataframe for later use
}
View(tauB_moderate)
