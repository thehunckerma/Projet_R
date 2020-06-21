source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "dplyr",
  "questionr",
  "psy",
  "data.table",
  "mokken",
  "DescTools",
  "rlist"
)
RequirePackages(packages)

df <- read_excel("data/dataset_clean_1.xlsx")
d_ordinal <- df[!(names(df) %in% c( # Ordinal variables ( Likert Scale )
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
  "G4",
))]
normalityTest=apply(d_ordinal,2,shapiro.test)
normalVarCount=length(list.filter(normalityTest,normalityTest[[.i]]$p.value > 0.05))
cat("in ", length(d_ordinal), " variables, there are ", normalVarCount," with no difference between their evolution and the normal distribution")

# df1=df[unlist(lapply(colnames(df),function(x){length(table(df[x]))==5}))] # selecting only elements with more than 4 parametres



