#A Homme->1 Femme->2
#L1-L2 Présentiel->1 distance->2
source("helpers.R")

packages <- c( # List of this script's dependencies
  "dplyr",
  "readxl",
  "tidyr",
  "stringr",
  "psych"
)
RequirePackages(packages) # if package is installed import it else install it and import it

dataset <- read_excel("data/dataset_clean_1.xlsx") # Import the dataset


sampleM=filter(dataset,dataset["B"]==1) # Echantillon des hommes
sampleW=filter(dataset,dataset["B"]==2) # Echantillon des femmes

sample_P_P=filter(filter(dataset,dataset["L1"]==1),filter(dataset,dataset["L1"]==1)["L2"]==1) # Echantillon des personnes qui prefere les cours presentiels en tout les cas
sample_P_R=filter(filter(dataset,dataset["L1"]==1),filter(dataset,dataset["L1"]==1)["L2"]==2) # Echantillon des personnes qui prefere les cours présentiels sous les condition actuelles et à distance en cas d'amelioration des conditions
sample_R_P=filter(filter(dataset,dataset["L1"]==2),filter(dataset,dataset["L1"]==2)["L2"]==1) # Echantillon des personnes qui prefere les cours à distance sous les condition actuelles et presentiels en cas d'amelioration des conditions
sample_R_R=filter(filter(dataset,dataset["L1"]==2),filter(dataset,dataset["L1"]==2)["L2"]==2) # Echantillon des personnes qui prefere les cours à distance en tout les cas
