source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "psy" # cronbach function
)
RequirePackages(packages)

df1 <- read_excel("data/dataset_clean_1.xlsx")
d_ordinal <- df1[, !(names(df1) %in% c( # Ordinal variables ( Likert Scale )
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

alphaOrd=cronbach(as.data.table(d_ordinal)[,1:length(as.data.table(d_ordinal))]) #applying the Conbach alpha test on ordinal data

if(alphaOrd$alpha>0.7){
  cat(
    "alpha is equal to",alphaOrd$alpha,", so the ordinal scale are reliable for early stages of research."
  )
}else{
  cat(
    "alpha is equal to",alphaOrd$alpha,", so the items are not reliable"
  )
}