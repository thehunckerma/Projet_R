source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "questionr",
  "rlist",
  "psy",
  "data.table",
  "mokken"
)
RequirePackages(packages)

df1 <- read_excel("data/dataset_clean_1.xlsx")
attach(df1)

CramerV= function(x){
  cramer.v(table(factor(x[[1]],ordered=TRUE),factor(x[[2]],ordered=TRUE)))^2
}
PearsonKhi2= function(x){
  chisq.test(table(factor(x[[1]],ordered=TRUE),factor(x[[2]],ordered=TRUE)),simulate.p.value = TRUE)
}
kendallTauB=function(x){
  KendallTauB(table(factor(x[[1]],ordered=TRUE),factor(x[[2]],ordered=TRUE)), conf.level=0.95)
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
pKhi2=lapply(a,PearsonKhi2)
depPValue=list.filter(a,PearsonKhi2(a[[.i]])$p.value < 0.05,simulate.p.value = TRUE)
cramer=lapply(depPValue,CramerV)
kendallTB=lapply(depPValue,kendallTauB)
tbl=as.data.table(df1)
alphaCronbach=cronbach(tbl[,1:length(tbl)])
check.reliability(as.data.frame(df1), MS = TRUE, alpha = TRUE, lambda.2 = TRUE, LCRC = FALSE, nclass = nclass.default, irc = FALSE)

#hypothesis
# I1 impacte (-) P5
# J impacte (-) P5
# K1 impacte (-) R4 et R5 ?
# Q1  impacte (+) P4 et P5
# Q2  impacte (+) P4 et P5
# Q3  impacte (+) P4 et P5
# N impacte (-) R4
# A impacte (-) P5
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