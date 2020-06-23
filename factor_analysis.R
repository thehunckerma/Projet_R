source("helpers.R")

packages <- c( # List of this script's dependencies7
  "readxl",
  "dplyr",
  "REdaS", # KMOS
  "parameters"
)

RequirePackages(packages)

d <- suppressWarnings(read_excel("data/dataset_clean_1.xlsx"))

# User defined functions -------------------------------------------------------------
InvertScale <- function(x) { # Return the variable with inversed Likert scale
  for (i in 1:length(x)) {
    if (x[i] == 1) x[i] <- 5
    if (x[i] == 2) x[i] <- 4
    if (x[i] == 4) x[i] <- 2
    if (x[i] == 1) x[i] <- 5
  }
  return(x)
}
# Sampling Adequacy ------------------------------------------------------------------

d_ordinal <- as.data.frame(d[, !(names(d) %in% c( # Ordinal variables ( Likert Scale )
  "B", "C", "D", "H", "J", "L1", "L2",
  "F1", "F2", "F3", "F4", "F5",
  "G1", "G2", "G3", "G4"
))])

KMOS(d_ordinal) # Calculate the Kaiser-Meyer-Olkin Statistics

# Let's choose our variables carefully

d_ordinal <- as.data.frame(cbind( # Choose the meaningful Likert Scale variables to predict the efficiency of distance learning during lockdown
  K4 = InvertScale(d$K4), K3 = InvertScale(d$K3), P5 = InvertScale(d$P5), # Invert the Likert Scale such as 1 = bad and 5 = good
  K2 = d$K2, I1 = d$I1, I2 = d$I2, I3 = d$I3, I4 = d$I4, I5 = d$I5,
  R1 = d$R1, R2 = d$R2, R3 = d$R3, R4 = d$R4, R5 = d$R5, R6 = d$R6, R7 = d$R7, R8 = d$R8
))

KMOS(d_ordinal) # Calculate the Kaiser-Meyer-Olkin Statistics for the new data frame

# Factor analysis ---------------------------------------------------------------

od.data <- as.matrix(d_ordinal)

for( i in 1:5 ){
  # Print the numbers of factors that are not sufficient for the model
  p_value <- factanal(x = od.data, factors = i)$PVAL
  if(p_value < 0.05) cat("i =",i,", p-value =", p_value, "\n")
}

# Let's try with 4 factors

fa <- factanal(x = od.data, factors = 4, n.obs = 70, rotation = "varimax")
print(fa)

1 - apply(fa$loadings^2,1,sum) # Uniqueness

apply(fa$loadings^2,1,sum) # Communality

resid <- round(fa$correlation - fa$loadings%*% t(fa$loadings) + diag(fa$uniquenesses),6) #  The residual matrix
print(resid)

# Let's plot the unrotated model for Factors 1 and 2
fa.none <- factanal(x = od.data, n.obs = 70, factors = 4, rotation = "none")
plot(fa.none$loadings[,1], fa.none$loadings[,2], xlim = c(-0.5, 1), ylim = c(-0.5, 1),
     xlab = "Factor 1", ylab = "Factor 2", main = "No Rotation")
abline(h = 0, v = 0)
text(fa.none$loadings[,1]-0.08, 
     fa.none$loadings[,2]+0.08,
     colnames(d_ordinal),
     col="blue")

# Let's the loadings with varimax rotation for Factors 1 and 2
fa.none <- factanal(x = od.data, n.obs = 70, factors = 4, rotation = "varimax")
plot(fa.none$loadings[,1], fa.none$loadings[,2], xlim = c(-0.5, 1), ylim = c(-0.5, 1),
     xlab = "Factor 1", ylab = "Factor 2", main = "Varimax Rotation")
abline(h = 0, v = 0)
text(fa$loadings[,1]-0.08, 
     fa$loadings[,2]+0.08,
     colnames(d_ordinal),
     col="blue")

# Let's the loadings with varimax rotation for Factors 1 and 3
fa.none <- factanal(x = od.data, n.obs = 70, factors = 4, rotation = "varimax")
plot(fa.none$loadings[,1], fa.none$loadings[,3], xlim = c(-0.5, 1), ylim = c(-0.5, 1),
     xlab = "Factor 1", ylab = "Factor 3", main = "Varimax Rotation")
abline(h = 0, v = 0)
text(fa$loadings[,1]-0.08, 
     fa$loadings[,3]+0.08,
     colnames(d_ordinal),
     col="blue")

# Let's the loadings with varimax rotation for Factors 1 and 4
fa.none <- factanal(x = od.data, n.obs = 70, factors = 4, rotation = "varimax")
plot(fa.none$loadings[,1], fa.none$loadings[,4], xlim = c(-0.5, 1), ylim = c(-0.5, 1),
     xlab = "Factor 1", ylab = "Factor 4", main = "Varimax Rotation")
abline(h = 0, v = 0)
text(fa$loadings[,1]-0.08, 
     fa$loadings[,4]+0.08,
     colnames(d_ordinal),
     col="blue")
