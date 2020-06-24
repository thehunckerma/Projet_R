source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl"
)
RequirePackages(packages) # if package is installed import it else install it and import it

dataset <- read_excel("data/dataset.xlsx") # Import the dataset

gender <- factor(dataset$B) # Gender column
school <- factor( # School column
  as.data.frame(
    lapply( # Join levels 3,4,5,6 and 7 into one level 3
      dataset$D[dataset$D != 8], # Filter "Hors UIT" occurences ( value = 8 )
      function(x) {
        if (x == 1 | x == 2) { # ENSAK & ENCGK
          return(x)
        } else { # Faculty
          return(3)
        }
      }
    )
  )
)
t <- 63458 # Total number of UIT students
# Total number of students in each school:
num_1 <- 1338 # ENSAK
num_2 <- 2501 # ENCGK
num_3 <- 59619 # Faculty

# Percentages:
per_1 <- num_1 / t # ENSAK
per_2 <- num_2 / t # ENCGK
per_3 <- num_3 / t # Faculty

# Chi-squared test for representativeness in terms of students distribution by school/faculty:
chisq.test(
  table(school),
  p = c(per_1, per_2, per_3),
  simulate.p.value = TRUE
) # p-value << 0.05 therefore the sample is not representative of the population

# Chi-squared test for representativeness in terms of students distribution by gender:
chisq.test(
  table(gender),
  p = c(1 / 2, 1 / 2),
  simulate.p.value = TRUE # We know that the number of females is almost equal to the number of males
) # p-value > 0.05 therefore the sample is representative of the population

# Percentage of obsservations by departement
s <- summary(school)
round(s / sum(s) * 100, 2)
