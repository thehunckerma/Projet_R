source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "dplyr",
  "questionr", # cramer.v
  "psych", # phi
  "DescTools", # KendallTauB
  "ltm" # biserial.cor
)
RequirePackages(packages)

d <- suppressWarnings(read_excel("data/dataset_clean_1.xlsx"))

# Script options --------------------------------------------------------------------

verbosity <- list(
  continuous_ordinal = TRUE,
  continuous_dichotomous = TRUE,
  categorical_dichotomous = TRUE,
  nominal_nominal = TRUE,
  dichotomous_dichotomous = TRUE,
  ordinal_ordinal = TRUE
) # Wether or not to print the tests results in the console

# User defined functions -------------------------------------------------------------

RankBiserial <- function(x, y) {
  x <- as.numeric(x) # dichotomous
  y <- as.numeric(y) # categorical
  y1 <- data.frame(y = numeric()) # Hold y values for x == 1
  y2 <- data.frame(y = numeric()) # Hold y values for x == 2
  n <- length(x) # Number of data pairs in the sample
  for (i in 1:n) {
    if (x[i] == 1) y1 <- rbind(y1, list(y = y[i]))
    if (x[i] == 2) y2 <- rbind(y2, list(y = y[i]))
  }
  return(2 * (colMeans(y1) - colMeans(y2)) / n) # Return the Rank-biseral coeff
}

# Split data by variable type --------------------------------------------------------

d_dichotomous <- as.data.frame(cbind( # Dichotomous variables ( yes/no questions )
  B = d$B,
  F1 = d$F1,
  F2 = d$F2,
  F3 = d$F3,
  F4 = d$F4,
  F5 = d$F5,
  G1 = d$G1,
  G2 = d$G2,
  G3 = d$G3,
  G4 = d$G4,
  L1 = d$L1,
  L2 = d$L2
))
d_nominal <- as.data.frame(cbind( # Nominal variables ( unordered )
  C = d$C,
  D = d$D,
  H = d$H,
  J = d$J
))
d_ordinal <- as.data.frame(d[, !(names(d) %in% c( # Ordinal variables ( Likert Scale )
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
))])

Age <- d$A # The age is the only continuous variable

d_colnames <- colnames(d) # List of dataframe column names

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||
# *************** Continuous-ordinal *******************
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||

continuous_ordinal_df <- data.frame(
  col1 = numeric(),
  col2 = numeric(),
  name1 = character(),
  name2 = character(),
  tau_b = numeric()
) # Initialize the dataframe that stores data (including Tau-b) variables that got a p-value < 0.05

colnames_ordinal <- colnames(d_ordinal) # Tables of ordinal variables names
n <- ncol(d_ordinal) # Number of ordinal variables

for (col in 1:n) { # Iterate over the ordinal variables dataframe

  x_ordered <- d_ordinal[col] %>%
    as.matrix() %>%
    as.table() %>%
    factor(
      ordered = TRUE,
      levels = c(1, 2, 3, 4, 5)
    ) # Convert to an ordered factor with 5 levels

  tau_b <- KendallTauB(
    table(Age, x_ordered), # Correlate Age with the other ordinal variables
    conf.level = 0.95
  )["tau_b"] # Kendall's Tau-b coeff to measure the direction and strength of the association

  if (!is.nan(tau_b)) { # Check if tau_b is a number
    if (abs(tau_b) >= 0.2) { # Significance level for moderate correlation
      if (verbosity["continuous_ordinal"] == TRUE) { # Verbosity
        if (tau_b > 0) {
          sign <- "positive"
        } else {
          sign <- "negative"
        }
        cat(
          "*** There is a", sign, "correlation between",
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
    } else if (abs(tau_b) < 0.2) { # Significance level for weak correlation
      if (tau_b > 0) {
        sign <- "positive"
      } else {
        sign <- "negative"
      }
      cat(
        "There is a weak/little", sign, "correlation between",
        d_colnames[1], "and", colnames_ordinal[col], "\n",
        "tau-b =", tau_b, "\n\n"
      )
    }
  }
}

continuous_ordinal_df <- continuous_ordinal_df[order(abs(continuous_ordinal_df$tau_b)), ] # Sort results by coeff

if (verbosity["continuous_ordinal"] == TRUE) { # Verbosity
  print(continuous_ordinal_df)
  cat("\n")
  n <- nrow(continuous_ordinal_df)
  if (n > 0) {
    for (row in 1:n) {
      if (continuous_ordinal_df$tau_b[row] > 0) {
        sign <- "positivement"
      } else {
        sign <- "negativement"
      }
      paste(
        map[continuous_ordinal_df$name1[row]],
        "correle", sign, "avec",
        map[continuous_ordinal_df$name2[row]],
        "(tau_b =", continuous_ordinal_df$tau_b[row], ")"
      ) %>%
        print() # Print a formated version of conclusion
    }
  } else {
    print("There are no correlations between these variables")
  }
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||||
# ************ Continuous-dichotomous ****************
# ||||||||||||||||||||||||||||||||||||||||||||||||||||

continuous_dichotomous_df <- data.frame(
  col1 = numeric(),
  col2 = numeric(),
  name1 = character(),
  name2 = character(),
  point_biserial = numeric()
) # Initialize the dataframe that stores data

colnames_dichotomous <- colnames(d_dichotomous) # Table of dichotomous variables names
n <- ncol(d_dichotomous) # Numberof dichotomous variables

for (col in 1:n) { # Loop over the dichotomous variables dataframe

  x <- d_dichotomous[col] %>%
    as.matrix() %>%
    as.table() %>%
    factor() # Convert to factor

  point_biserial <- biserial.cor(Age, x) # Mesure the Point-Biserial correlation coefficient

  if (!is.nan(point_biserial)) { # Check if the point biserial coeff is a number
    if (abs(point_biserial) >= 0.2) { # Significance level for moderate correlation
      if (verbosity["continuous_dichotomous"] == TRUE) { # Verbosity
        if (point_biserial > 0) { # Check the direction of the correlation
          sign <- "positive"
        } else {
          sign <- "negative"
        }
        cat(
          "*** There is a", sign, "correlation between",
          d_colnames[1], "and", colnames_dichotomous[col], "\n",
          "point_biserial =", point_biserial, "\n\n"
        ) # Print some information
      }
      continuous_dichotomous_df <- rbind(continuous_dichotomous_df, list(
        col1 = 1, # Age variable
        col2 = col,
        name1 = d_colnames[1], # Age variable
        name2 = colnames_dichotomous[col],
        point_biserial = point_biserial
      )) # Store the information in the dataframe
    }
  } else if (abs(point_biserial) < 0.2) {
    if (point_biserial > 0) {
      sign <- "positive"
    } else {
      sign <- "negative"
    }
    cat(
      "There is a weak/little", sign, "correlation between",
      d_colnames[1], "and", colnames_dichotomous[col], "\n",
      "point_biserial =", point_biserial, "\n\n"
    )
  }
}

continuous_dichotomous_df <- continuous_dichotomous_df[order(abs(continuous_dichotomous_df$point_biserial)), ] # Sort results by coeff

if (verbosity["continuous_dichotomous"] == TRUE) { # Verbosity
  print(continuous_dichotomous_df)
  cat("\n")
  n <- nrow(continuous_dichotomous_df)
  if (n > 0) {
    for (row in 1:n) {
      if (continuous_dichotomous_df$point_biserial[row] > 0) {
        sign <- "positivement"
      } else {
        sign <- "negativement"
      }
      paste(
        map[continuous_dichotomous_df$name1[row]],
        "correle", sign, "avec",
        map[continuous_dichotomous_df$name2[row]],
        "(point_biserial =", continuous_dichotomous_df$point_biserial[row], ")"
      ) %>%
        print() # Print a formated version of conclusion
    }
  } else {
    print("There are no correlations between these variables")
  }
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||
# ******** Ordinal/Nominal-dichotomous *************
# ||||||||||||||||||||||||||||||||||||||||||||||||||

categorical_dichotomous_df <- data.frame(
  col1 = numeric(),
  col2 = numeric(),
  name1 = character(),
  name2 = character(),
  rank_biserial = numeric()
) # Initialize the dataframe that stores data

d_categorical <- cbind(d_nominal, d_ordinal) # Merge the norminal and ordinal variables in one data frame

colnames_categorical <- colnames(d_categorical) # Table of categorical variables names
colnames_dichotomous <- colnames(d_dichotomous) # Table of dichotomous variables names

n <- ncol(d_dichotomous) # Number of dichotomous variables
m <- ncol(d_categorical) # Number of categorical variables

for (col in 1:n) { # Loop over the dichotomous variables dataframe
  for (col_ in col:m) { # Loop over the categorical variables dataframe

    x <- d_dichotomous[col] %>%
      as.matrix() %>%
      as.table() %>%
      factor() # Convert to factor

    y <- d_categorical[col_] %>%
      as.matrix() %>%
      as.table() %>%
      factor() # Convert to factor

    rank_biserial <- RankBiserial(x, y) # Mesure the Rank-Biserial correlation coefficient

    if (!is.nan(rank_biserial) # Check if the rank biserial coeff is a number
    & abs(rank_biserial) >= 0.1 # Significance level for moderate correlation
    ) {
      if (verbosity["categorical_dichotomous"] == TRUE) { # Verbosity
        if (rank_biserial > 0) { # Check the direction of the correlation
          sign <- "positive"
        } else {
          sign <- "negative"
        }
        cat(
          "*** There is a", sign, "correlation between",
          colnames_dichotomous[col], "and", colnames_categorical[col_], "\n",
          "rank_biserial =", rank_biserial, "\n\n"
        ) # Print some information
      }
      categorical_dichotomous_df <- rbind(categorical_dichotomous_df, list(
        col1 = col,
        col2 = col_,
        name1 = colnames_dichotomous[col],
        name2 = colnames_categorical[col_],
        rank_biserial = rank_biserial
      )) # Store the information in the dataframe
    } else if (abs(rank_biserial) < 0.2) {
      if (rank_biserial > 0) {
        sign <- "positive"
      } else {
        sign <- "negative"
      }
      cat(
        "There is a weak/little", sign, "correlation between",
        colnames_dichotomous[col], "and", colnames_categorical[col_], "\n",
        "rank_biserial =", rank_biserial, "\n\n"
      )
    }
  }
}

categorical_dichotomous_df <- categorical_dichotomous_df[order(abs(categorical_dichotomous_df$rank_biserial)), ] # Sort results by coeff

if (verbosity["categorical_dichotomous"] == TRUE) { # Verbosity
  print(categorical_dichotomous_df)
  cat("\n")
  n <- nrow(categorical_dichotomous_df)
  if (n > 0) {
    for (row in 1:n) {
      if (categorical_dichotomous_df$rank_biserial[row] > 0) {
        sign <- "positivement"
      } else {
        sign <- "negativement"
      }
      paste(
        map[categorical_dichotomous_df$name1[row]],
        "correle", sign, "avec",
        map[categorical_dichotomous_df$name2[row]],
        "(rank_biserial =", categorical_dichotomous_df$rank_biserial[row], ")"
      ) %>%
        print() # Print a formated version of conclusion
    }
  } else {
    print("There are no correlations between these variables")
  }
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||
# *************** Nominal-nominal ******************
# ||||||||||||||||||||||||||||||||||||||||||||||||||

nominal_nominal_df <- data.frame(
  col1 = numeric(),
  col2 = numeric(),
  name1 = character(),
  name2 = character(),
  chi2_p_value = numeric(),
  cramer_v = numeric()
) # Initialize the dataframe that stores data

colnames_nominal <- colnames(d_nominal) # Table of dichotomous variables names=
n <- ncol(d_nominal) # Number of nominal variables

for (col in 1:n) { # Loop over the nominal variables dataframe
  for (col_ in col:n) { # Loop over the nominal variables dataframe
    # Convert to ordinal variable

    x <- d_nominal[col] %>%
      as.matrix() %>%
      as.table() %>%
      factor() # Convert each row to factor

    y <- d_nominal[col_] %>%
      as.matrix() %>%
      as.table() %>%
      factor() # Convert each row to factor

    if (length(summary(x)) < 2 | col == col_) next # Cramer's v requires the contingency table to be 2x2 or bigger

    chi2_p_value <- chisq.test(table(x, y), simulate.p.value = TRUE)$p.value # Chi-square test
    cramer_v <- suppressWarnings(cramer.v(table(x, y))) # Mesure the the Cramer's V correlation coefficient

    if (chi2_p_value <= 0.05) # Significance level for chi2 correlation
      {
        if (verbosity["nominal_nominal"] == TRUE) { # Verbosity
          cat(
            "*** There is a correlation between",
            colnames_nominal[col], "and", colnames_nominal[col_], "\n",
            "p_value =", chi2_p_value, "cramer_v =", cramer_v, "\n\n"
          )
        }
        nominal_nominal_df <- rbind(nominal_nominal_df, list(
          col1 = col,
          col2 = col_,
          name1 = colnames_nominal[col],
          name2 = colnames_nominal[col_],
          chi2_p_value = chi2_p_value,
          cramer_v = cramer_v
        )) # Store the information in the dataframe
      } else if (chi2_p_value > 0.05) {
      cat(
        "There is a weak/little correlation between",
        colnames_nominal[col], "and", colnames_nominal[col_], "\n",
        "p_value =", chi2_p_value, "cramer_v =", cramer_v, "\n\n"
      )
    }
  }
}

nominal_nominal_df <- nominal_nominal_df[order(abs(nominal_nominal_df$cramer_v)), ] # Sort results by coeff

if (verbosity["nominal_nominal"] == TRUE) { # Verbosity
  print(nominal_nominal_df)
  cat("\n")
  n <- nrow(nominal_nominal_df)
  if (n > 0) {
    for (row in 1:n) {
      paste(
        map[nominal_nominal_df$name1[row]],
        "correle avec",
        map[nominal_nominal_df$name2[row]],
        "(cramer_v =", nominal_nominal_df$cramer_v[row], ")"
      ) %>%
        print() # Print a formated version of conclusion
    }
  } else {
    print("There are no correlations between these variables")
  }
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||
# ************ Dichotomous-dichotomous *************
# ||||||||||||||||||||||||||||||||||||||||||||||||||

dichotomous_dichotomous_df <- data.frame(
  col1 = numeric(),
  col2 = numeric(),
  name1 = character(),
  name2 = character(),
  phi_coeff = numeric()
) # Initialize the dataframe that stores data

colnames_nominal <- colnames(d_dichotomous) # Table of dichotomous variables names=
n <- ncol(d_dichotomous) # Number of dichotomous variables

for (col in 1:n) { # Loop over the dichotomous variables dataframe
  for (col_ in col:n) { # Loop over the dichotomous variables dataframe
    # Convert to ordinal variable

    x <- d_dichotomous[col] %>%
      as.matrix() %>%
      as.table() %>%
      factor() # Convert each row to factor

    y <- d_dichotomous[col_] %>%
      as.matrix() %>%
      as.table() %>%
      factor() # Convert each row to factor

    if (col == col_) next

    phi_coeff <- phi(as.matrix(table(x, y))) # Mesure the Phi correlation coefficient

    if (abs(phi_coeff) >= 0.2) # Significance level for phi coefficient
      {
        if (verbosity["dichotomous_dichotomous"] == TRUE) { # Verbosity
          if (phi_coeff > 0) { # Check the direction of the correlation
            sign <- "positive"
          } else {
            sign <- "negative"
          }
          cat(
            "There is a", sign, "correlation between",
            colnames_dichotomous[col], "and", colnames_dichotomous[col_], "\n",
            "phi_coeff =", phi_coeff, "\n\n"
          )
        }
        dichotomous_dichotomous_df <- rbind(dichotomous_dichotomous_df, list(
          col1 = col,
          col2 = col_,
          name1 = colnames_nominal[col],
          name2 = colnames_nominal[col_],
          phi_coeff = as.numeric(phi_coeff)
        )) # Store the information in the dataframe
      } else if (abs(phi_coeff) < 0.2) {
      if (phi_coeff > 0) {
        sign <- "positive"
      } else {
        sign <- "negative"
      }
      cat(
        "There is a weak/little", sign, "correlation between",
        colnames_dichotomous[col], "and", colnames_dichotomous[col_], "\n",
        "phi_coeff =", phi_coeff, "\n\n"
      )
    }
  }
}

dichotomous_dichotomous_df <- dichotomous_dichotomous_df[order(abs(dichotomous_dichotomous_df$phi_coeff)), ] # Sort results by coeff

if (verbosity["nominal_nominal"] == TRUE) { # Verbosity
  print(dichotomous_dichotomous_df)
  cat("\n")
  n <- nrow(dichotomous_dichotomous_df)
  if (n > 0) {
    for (row in 1:n) {
      if (dichotomous_dichotomous_df$phi_coeff[row] > 0) {
        sign <- "positivement"
      } else {
        sign <- "negativement"
      }
      paste(
        map[dichotomous_dichotomous_df$name1[row]],
        "correle", sign, "avec",
        map[dichotomous_dichotomous_df$name2[row]],
        "(phi_coeff =", dichotomous_dichotomous_df$phi_coeff[row], ")"
      ) %>%
        print() # Print a formated version of conclusion
    }
  } else {
    print("There are no correlations between these variables")
  }
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||
# ************ Ordinal-ordinal *************
# ||||||||||||||||||||||||||||||||||||||||||||||||||

ordinal_ordinal_df <- data.frame(
  col1 = numeric(),
  col2 = numeric(),
  name1 = character(),
  name2 = character(),
  tau_b = numeric()
) # Initialize the dataframe that stores data

colnames_ordinal <- colnames(d_ordinal) # Table of ordinal variables names=
n <- ncol(d_ordinal) # Number of ordinal variables

for (col in 1:n) { # Loop over the ordinal variables dataframe
  for (col_ in col:n) { # Loop over the ordinal variables dataframe
    # Convert to ordinal variable

    x <- d_ordinal[col] %>%
      as.matrix() %>%
      as.table() %>%
      factor(
        ordered = TRUE,
        levels = c(1, 2, 3, 4, 5)
      ) # Convert to an ordered factor with 5 levels
    y <- d_ordinal[col_] %>%
      as.matrix() %>%
      as.table() %>%
      factor(
        ordered = TRUE,
        levels = c(1, 2, 3, 4, 5)
      ) # Convert to an ordered factor with 5 levels

    if (col == col_) next

    tau_b <- KendallTauB(
      table(x, y),
      conf.level = 0.95
    )["tau_b"] # Kendall's Tau-b coeff to measure the direction and strength of the association

    if (!is.nan(tau_b)) { # Check if tau_b is a number
      if (abs(tau_b) >= 0.2) { # Significance level for moderate correlation
        if (verbosity["ordinal_ordinal"] == TRUE) { # Verbosity
          if (tau_b > 0) {
            sign <- "positive"
          } else {
            sign <- "negative"
          }
          cat(
            "*** There is a", sign, "correlation between",
            colnames_ordinal[col], "and", colnames_ordinal[col_], "\n",
            "tau-b =", tau_b, "\n\n"
          )
        }
        ordinal_ordinal_df <- rbind(ordinal_ordinal_df, list(
          col1 = col,
          col2 = col_,
          name1 = colnames_ordinal[col],
          name2 = colnames_ordinal[col_],
          tau_b = tau_b
        )) # Store the information in the dataframe
      } else if (abs(tau_b) < 0.2) { # Significance level for weak correlation
        if (tau_b > 0) {
          sign <- "positive"
        } else {
          sign <- "negative"
        }
        cat(
          "There is a weak/little", sign, "correlation between",
          colnames_ordinal[col], "and", colnames_ordinal[col_], "\n",
          "tau-b =", tau_b, "\n\n"
        )
      }
    }
  }
}

ordinal_ordinal_df <- ordinal_ordinal_df[order(abs(ordinal_ordinal_df$tau_b)), ] # Sort results by coeff

if (verbosity["ordinal_ordinal"] == TRUE) { # Verbosity
  print(ordinal_ordinal_df)
  cat("\n")
  n <- nrow(ordinal_ordinal_df)
  if (n > 0) {
    for (row in 1:n) {
      if (ordinal_ordinal_df$tau_b[row] > 0) {
        sign <- "positivement"
      } else {
        sign <- "negativement"
      }
      paste(
        map[ordinal_ordinal_df$name1[row]],
        "correle", sign, "avec",
        map[ordinal_ordinal_df$name2[row]],
        "(tau_b =", ordinal_ordinal_df$tau_b[row], ")"
      ) %>%
        print() # Print a formated version of conclusion
    }
  } else {
    print("There are no correlations between these variables")
  }
}
