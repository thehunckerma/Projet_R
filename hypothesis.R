source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "questionr"
)
RequirePackages(packages)
#hypothesis
# I1 impacte (-) P5
# J impacte (-) P5
# K1 impacte (-) R4 et R5 ?
# Q1  impacte (+) P4 et P5
# Q2  impacte (+) P4 et P5
# Q3  impacte (+) P4 et P5
# N impacte (-) R4
# A impacte (-) P5



df1 <- read_excel("data/dataset_clean_1.xlsx")
attach(df1)

CramerV= function(x){
    cramer.v(table(factor(x[[1]],ordered=TRUE),factor(x[[2]],ordered=TRUE)))^2
}
PearsonKhi2= function(x){
    coningencyTable=table(factor(x[[1]],ordered=TRUE),factor(x[[2]],ordered=TRUE))
    chisq.test(coningencyTable)
}

a=list(
  list(I1,P5),
  list(J,P5),
  list(K1,P4),
  list(K1,P5),
  list(Q1,P4),
  list(Q1,P5),
  list(Q2,P4),
  list(Q2,P5),
  list(Q3,P4),
  list(Q3,P5),
  list(N,R4),
  list(A,P5)
)
a=lapply(a,CramerV)
a=lapply(a,PearsonKhi2)
# #hypothese: nombre de sceance par semaine impacte l'assiduité
# chi2CramereV(J,P5)
# #hypothese: nombre de devoir impacte l'assurance desprofs de l'assiduité des eleves
# chi2CramereV(K1,P4)
# #hypothese: nombre de devoir impacte l'assiduité
# chi2CramereV(K1,P5)
# #hypothese: Les quiz impacte l'assurance desprofs de l'assiduité des eleves
# chi2CramereV(Q1,P4)
# #hypothese: Les quiz impacte l'assiduité
# chi2CramereV(Q1,P5)
# #hypothese: l'appel impacte l'assurance desprofs de l'assiduité des eleves
# chi2CramereV(Q2,P4)
# #hypothese: l'appel impacte l'assiduité
# chi2CramereV(Q2,P5)
# #hypothese: Les Qs durants le cours impacte l'assurance desprofs de l'assiduité des eleves
# chi2CramereV(Q3,P4)
# #hypothese: Les Qs durants le cours impacte l'assiduité
# chi2CramereV(Q3,P5)
# #hypothese: nombre de sceance par semaine impacte le stress
# chi2CramereV(N,R4)
# #hypothese: l'age impacte l'assiduité
# chi2CramereV(A,P5)
