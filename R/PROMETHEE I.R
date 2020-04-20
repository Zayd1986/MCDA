PROMETHEEI<-function(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,GaussParameter,CriteriaWeights,CriteriaMinMax)
  
{ 
numAlt<-dim(EvaluationTable)[1] # number of alternatives
# Call of the function PROMETHEEOutrankingFlows
outrankingflowspos<-PROMETHEEOutrankingFlows(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,GaussParameter,CriteriaWeights,CriteriaMinMax)[1]
outrankingflowsneg<-PROMETHEEOutrankingFlows(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,GaussParameter,CriteriaWeights,CriteriaMinMax)[2]
outrankingflowspos<-matrix(as.numeric(unlist(outrankingflowspos)),1,numAlt)
outrankingflowsneg<-matrix(as.numeric(unlist(outrankingflowsneg)),1,numAlt)
P<-matrix(rep(0,numAlt*numAlt),numAlt,numAlt) #matrix containig the preference relations between alternatives
I<-matrix(rep(0,numAlt*numAlt),numAlt,numAlt) #matrix containig the indifference relations between alternatives
R<-matrix(rep(0,numAlt*numAlt),numAlt,numAlt) #matrix containig the incomparability relations between alternatives
for (i in (1:numAlt)){
  for (j in (1:numAlt)){
if (((outrankingflowspos[i]>outrankingflowspos[j])&(outrankingflowsneg[i]<outrankingflowsneg[j]))||((outrankingflowspos[i]==outrankingflowspos[j])&(outrankingflowsneg[i]<outrankingflowsneg[j]))||((outrankingflowspos[i]>outrankingflowspos[j])&(outrankingflowsneg[i]==outrankingflowsneg[j])))
      {
  P[i,j]=1
  }
else if ((outrankingflowspos[i]== outrankingflowspos[j])&(outrankingflowsneg[i]== outrankingflowsneg[j]))
  {
  I[i,j]=1
  }
  else if (((outrankingflowspos[i]>outrankingflowspos[j])&(outrankingflowsneg[i]>outrankingflowsneg[j]))||((outrankingflowspos[i]<outrankingflowspos[j])&(outrankingflowsneg[i]<outrankingflowsneg[j])))
  {
    R[i,j]=1
  }
  }
}

list(P=P,I=I,R=R)
}


