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

c1= c("Toujours" ,"Fréquement","De temps à l'autre","Rarement","Jamais")
c2= c("Tout à fait d'accord","Ni d'accord ni pas d'accord","Pas d'accord","Totalement pas d'accord")
c3= c("Très mal","Pas de différence","Mieux","Beaucoup mieux")
c4= c("très négativement","négativement","neutre","positivemnt","très positivemnt")
c5= c(18,19,20,21,22,23)

df1 <- read_excel("data/dataset_clean_1.xlsx")
attach(df1)

CramerV= function(x,y){
    cramer.v(table(factor(x,ordered=TRUE),factor(y,ordered=TRUE)))^2
}
#hypothese: perte de connection impacte l'assiduité
CramerV(I1,P5)



#hypothese: nombre de sceance par semaine impacte l'assiduité
chi2CramereV(J,P5,c("moins 5 séances/semaine","5 séances/semaine","entre 6 et 10 séances/semaine"),c2)
#hypothese: nombre de devoir impacte l'assurance desprofs de l'assiduité des eleves
chi2CramereV(K1,P4,c2,c2)
#hypothese: nombre de devoir impacte l'assiduité
chi2CramereV(K1,P5,c2,c2)
#hypothese: Les quiz impacte l'assurance desprofs de l'assiduité des eleves
chi2CramereV(Q1,P4,c1,c2)
#hypothese: Les quiz impacte l'assiduité
chi2CramereV(Q1,P5,c1,c2)
#hypothese: l'appel impacte l'assurance desprofs de l'assiduité des eleves
chi2CramereV(Q2,P4,c1,c2)
#hypothese: l'appel impacte l'assiduité
chi2CramereV(Q2,P5,c1,c2)
#hypothese: Les Qs durants le cours impacte l'assurance desprofs de l'assiduité des eleves
chi2CramereV(Q3,P4,c1,c2)
#hypothese: Les Qs durants le cours impacte l'assiduité
chi2CramereV(Q3,P5,c1,c2)
#hypothese: nombre de sceance par semaine impacte le stress
chi2CramereV(N,R4,c4,c3)
#hypothese: l'age impacte l'assiduité
chi2CramereV(A,P5,c5,c2)
