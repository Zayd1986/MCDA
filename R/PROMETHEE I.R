PROMETHEEI<-function(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,GaussParameter,CriteriaWeights,CriteriaMinMax)
# This function is the  PROMETHEE I partial ranking which  is obtained from the positive
  #and the negative outranking flows. This function returns three matrices P (for Preference relations), I(for indifference relations) and  R(for incomparability relations).
  #Each matrix contains only 0 and 1. 1 (at the position (i,j) ) means that a_i P a_j (in the matrix P), or a_i I a_j (in the matrix I)
  # or a_i R a_j (in the matrix R)  and 0 else.
{ 
numAlt<-dim(EvaluationTable)[1] # number of alternatives
# Call of the function PROMETHEEOutrankingFlows
outranking<-PROMETHEEOutrankingFlows(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,GaussParameter,CriteriaWeights,CriteriaMinMax)
outrankingflowspos<-outranking[1]
outrankingflowsneg<-outranking[2]
outrankingflowspos<-matrix(as.numeric(unlist(outrankingflowspos)),1,numAlt)
outrankingflowsneg<-matrix(as.numeric(unlist(outrankingflowsneg)),1,numAlt)
P<-matrix(rep(0,numAlt*numAlt),numAlt,numAlt) #matrix containig the preference relations between alternatives
I<-matrix(rep(0,numAlt*numAlt),numAlt,numAlt) #matrix containig the indifference relations between alternatives
R<-matrix(rep(0,numAlt*numAlt),numAlt,numAlt) #matrix containig the incomparability relations between alternatives
for (i in (1:numAlt)){
  for (j in (1:numAlt)){
if (((outrankingflowspos[i]>outrankingflowspos[j])&(outrankingflowsneg[i]<outrankingflowsneg[j]))||((outrankingflowspos[i]==outrankingflowspos[j])&(outrankingflowsneg[i]<outrankingflowsneg[j]))||((outrankingflowspos[i]>outrankingflowspos[j])&(outrankingflowsneg[i]==outrankingflowsneg[j])))
      {
  #a_i P a_j
  P[i,j]=1
  }
else if ((outrankingflowspos[i]== outrankingflowspos[j])&(outrankingflowsneg[i]== outrankingflowsneg[j]))
  {
  # a_i I a_j
  I[i,j]=1
  }
  else if (((outrankingflowspos[i]>outrankingflowspos[j])&(outrankingflowsneg[i]>outrankingflowsneg[j]))||((outrankingflowspos[i]<outrankingflowspos[j])&(outrankingflowsneg[i]<outrankingflowsneg[j])))
  {
    #a_i R a_j
    R[i,j]=1
  }
  }
}

list(P=P,I=I,R=R)
}


