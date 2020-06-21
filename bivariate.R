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

df1=df[c(FALSE,unlist(lapply(colnames(df[,2:length(df)]),function(x){length(table(df[x]))>=4})))] # selecting only elements with more than 4 parametres

normalityTest=apply(df1,2,shapiro.test)
normalVarCount=length(list.filter(normalityTest,normalityTest[[.i]]$p.value < 0.05))
cat("in ", length(df1), " variables, there are ", normalVarCount," normal ones")


