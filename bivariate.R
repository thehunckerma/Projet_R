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

# making two sub samples, dividing by sex
men=df[df$B==1,]
women=df[df$B==2,]

# normalizing the samples to the same number of subjects
men=men[1:min(nrow(men),nrow(women)),]
women=women[1:min(nrow(men),nrow(women)),]

# applying the wilcox test on every variable on both men and women samples and checking 
# if there is a difference between the results based o the p-value

messages=unlist(lapply(names(c(map1[1],map1[3:length(map1)])),function(x){
   p_value=wilcox.test(unlist(men[x]),unlist(women[x]),paired=TRUE)$p.value
   if(p_value>=0.05){
    paste(
        "There is no significant difference the", map1[x], "between men and women"
    )
   }else{
    paste(
        "There is a significant difference the", map1[x], "between men and women"
    )
   }
}))
messages