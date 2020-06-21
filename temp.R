attach(df1)
tbl=as.data.table(df1)
a=list(
  list(I1,P5), #Perte de connexion ou debit faible / l'assiduité
  list(J,P5), #nombre de sceance par semaine / l'assurance desprofs de l'assiduité des eleves
  list(K1,P4), #nombre de devoir / l'assurance desprofs de l'assiduité des eleves
  list(K1,P5), #Les quiz / l'assiduité
  list(Q1,P4), #Les quiz / l'assurance des profs de l'assiduité des eleves
  list(Q1,P5), #Les quiz / l'assiduité
  list(Q2,P4), #l'appel / l'assurance des profs de l'assiduité des eleves
  list(Q2,P5), #l'appel / l'assiduité
  list(Q3,P4), #Les Qs durants le cours / l'assurance desprofs de l'assiduité des eleves
  list(Q3,P5), #Les Qs durants le cours / l'assiduité
  list(N,R4), #nombre de sceance par semaine / le stress
  list(A,P5) #l'age / l'assiduité
)


#execution of the tests
pKhi2=lapply(a,PearsonKhi2) #applying Khi2 test to all elements of the list of "a" --lists of questions--

depPValue=list.filter(a,PearsonKhi2(a[[.i]])$p.value < 0.05,simulate.p.value = TRUE) #filtering list and keeping only elements with p-value < 0.05

cramer=lapply(depPValue,CramerV) #applying cramerV test to the remaining elements after filter

kendallTB=lapply(depPValue,kendallTauB) #applying kendallTB test to the remaining elements after filter

alphaCronbach=cronbach(tbl[,1:length(tbl)]) #applying the Conbach alpha test on the dataframe (as a data table) to check data reliability based on the alpha value

#applying the mokken scaling, Conbach alpha test, the Guttman's lambda test, on the dataframe to check data reliability
check.reliability(as.data.frame(df1), MS = TRUE, alpha = TRUE, lambda.2 = TRUE, LCRC = FALSE, nclass = nclass.default, irc = FALSE)





# #hypothese: l'appel impacte l'assiduité
# chi2CramereV(Q2,P5)
# #hypothese: Les Qs durants le cours impacte l'assurance desprofs de l'assiduité des eleves
# chi2CramereV(Q3,P4)
# #hypothese: Les Qs durants le cours impacte l'assiduité
# chi2CramereV(Q3,P5)
