PROMETHEEII<-function(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,GaussParameter,CriteriaWeights,CriteriaMinMax)
  # This function consists of the (P, I) complete ranking which  is obtained from the net outranking
  # flow which is the balance between the positive and the negative outranking flows.This function returns two matrices P (for Preference relations) and I(for indifference relations).
  #Each matrix contains only 0 and 1. 1 (at the position (i,j) ) means that a_i P a_j (in the matrix P), or a_i I a_j (in the matrix I)
  # and 0 else.  
{ 
  numAlt<-dim(EvaluationTable)[1] # number of alternatives
  # Call of the function PROMETHEEOutrankingFlows
  outranking<-PROMETHEEOutrankingFlows(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,GaussParameter,CriteriaWeights,CriteriaMinMax)
  outrankingflowspos<-outranking[1]
  outrankingflowsneg<-outranking[2]
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
        #a_i P a_j
        P[i,j]=1
      else if (outrankingflowsnet[i]==outrankingflowsnet[j])
        #a_i I a_j
        I[i,j]=1
      }
  }
list(P=P,I=I)  
}