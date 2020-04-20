PROMETHEEII<-function(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,GaussParameter,CriteriaWeights,CriteriaMinMax)
  
{ 
  numAlt<-dim(EvaluationTable)[1] # number of alternatives
  # Call of the function PROMETHEEOutrankingFlows
  outrankingflowspos<-PROMETHEEOutrankingFlows(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,GaussParameter,CriteriaWeights,CriteriaMinMax)[1]
  outrankingflowsneg<-PROMETHEEOutrankingFlows(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,GaussParameter,CriteriaWeights,CriteriaMinMax)[2]
  outrankingflowspos<-matrix(as.numeric(unlist(outrankingflowspos)),1,numAlt)
  outrankingflowsneg<-matrix(as.numeric(unlist(outrankingflowsneg)),1,numAlt)
  P<-matrix(rep(0,numAlt*numAlt),numAlt,numAlt)  #matrix containig the preference relations between the alternatives
  I<-matrix(rep(0,numAlt*numAlt),numAlt,numAlt)  #matrix containig the indifference relations between the alternatives
  outrankingflowsnet<-matrix(rep(0,numAlt),1,numAlt)  #The net outranking flow
  for (i in (1:numAlt)){
    outrankingflowsnet[i]<-outrankingflowspos[i]-outrankingflowsneg[i]
    for (j in (1:numAlt)){
      outrankingflowsnet[j]<-outrankingflowspos[j]-outrankingflowsneg[j]
      if (outrankingflowsnet[i]>outrankingflowsnet[j])
        P[i,j]=1
      else if (outrankingflowsnet[i]==outrankingflowsnet[j])
        I[i,j]=1
      }
  }
list(P=P,I=I)  
}