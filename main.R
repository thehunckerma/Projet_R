rm(list = ls()) # Clean the local environment
options(Rcmdr = list(suppress.menus = FALSE)) # Supress Rcmdr menu bar and tool bar. Set it to TRUE in production

packages <- c("dplyr", "Rcmdr", "tidyr", "stringr") # List of this script's dependencies
lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}) # Check if the package is installed otherwise install it, and import it after that

path <- "X:/R/Projet_R/dataset_clean_2.xlsx" # Dataset path
Dataset <- readXL(path,
  rownames = FALSE,
  header = TRUE, na = "na",
  stringsAsFactors = TRUE
) # Import the dataset
if(FALSE){ # Ignore this block ( used to split F and G into multiple columns with single values )
  Dataset0 <- Dataset
  Dataset = Dataset %>% separate(F,c("F1","F2","F3","F4","F5"),", ")
  Dataset = Dataset %>% separate(G,c("G1","G2","G3","G4"),", ")
  lapply(Dataset0$F, function(x) {
    c <- str_split(x,", ")
    for (s in c("1","2","3","4","5")) {
      if(!is.na(x)){
        if(grepl(x,s,fixed=TRUE)){
          text = paste("Dataset$F",s,"[",parent.frame()$i[],"]<<-1",sep="")
          print(text)
          eval(parse(text=text)) # oui
        } else {
          eval(parse(text=paste("Dataset$F",s,"[",parent.frame()$i[],"]<<-2",sep=""))) # non
        }
      }
    }
  })
  lapply(Dataset0$G, function(x) {
    c <- str_split(x,", ")
    for (s in c("1","2","3","4")) {
      if(!is.na(x)){
        if(grepl(x,s,fixed=TRUE)){
          text = paste("Dataset$G",s,"[",parent.frame()$i[],"]<<-1",sep="")
          print(text)
          eval(parse(text=text)) # oui
        } else {
          eval(parse(text=paste("Dataset$G",s,"[",parent.frame()$i[],"]<<-2",sep=""))) # non
        }
      }
    }
  })
}

# Let's split the data into 2 depending on whether the person have taken distance courses
# We filter the lines based on the value of E
# ( which is 1 if the person have taken distance courses and 2 otherwise )

# We exclude the E column as it becomes useless after the split
df1 <- select(Dataset[Dataset$E == 1, ], -E)

# We exclude all the columns except A, B, C, D because they contain NA values
df2 <- Dataset[Dataset$E == 2, 1:4]

# **** Outliers treatement in column A ( Age ), as it is the only quantitative variable ****

if (FALSE) { # Ignore this block of code
  boxplot(df1$A, data = df1, main = "Age", boxwex = 0.2) # Box plot of Age column in df1
  # Based on the plot we can say that there is no outliers in the fisrt dataframe

  boxplot(df2$A, data = df2, main = "Age", boxwex = 0.2) # Box plot of Age column in df2
  # We can tell from the plot that there is a potential outlier with value = 30 in the second dataframe

  # Let's take a look at D value ( the school/faculty )
  print(paste("D = ", df2[df2 == 30, ]$D))
  # So the school is FLSH UIT
  # Therefore we can't tell if the value A = 30 is an outlier because there is no age limit in that faculty
}

