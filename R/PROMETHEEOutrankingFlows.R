PROMETHEEOutrankingFlows<- function(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,GaussParameter,CriteriaWeights,CriteriaMinMax)
  
{ 
  numAlt<-dim(EvaluationTable)[1] # number of alternatives
  Outrankingflowspos<-matrix(rep(0,numAlt),1,numAlt)  #the positive outranking flow
  Outrankingflowsneg<-matrix(rep(0,numAlt),1,numAlt)  #the negative outranking flow
  PreferenceTable<-PROMETHEEPreferenceIndices(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,GaussParameter,CriteriaWeights,CriteriaMinMax)
  for(i in (1:numAlt)){
    for(j in (1:numAlt)){
      Outrankingflowspos[i]=Outrankingflowspos[i]+(1/(numAlt-1))*PreferenceTable[i,j]
      Outrankingflowsneg[i]=Outrankingflowsneg[i]+(1/(numAlt-1))*PreferenceTable[j,i]
    }
  }
  list(Outrankingflowspos=Outrankingflowspos,Outrankingflowsneg=Outrankingflowsneg)
}