PROMETHEEPreferenceIndices<- function(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,GaussParameter,CriteriaWeights,CriteriaMinMax)
  
{ 
  numAlt<-dim(EvaluationTable)[1] # number of alternatives
  numCrit<-dim(EvaluationTable)[2] # number of criteria
  PreferenceTablePos<-matrix(rep(0,(numAlt-1)*numAlt),numAlt,(numAlt-1))
  PreferenceTableNeg<-matrix(rep(0,(numAlt-1)*numAlt),numAlt,(numAlt-1))
#Pairwise comparisons of evaluation criteria
  for(i in (1:numAlt)){
    j<-0
    for(k in (1:numAlt)){
      if ((k!=i) & (j< numAlt-1)){
        j<-j+1
      for(l in (1:numCrit)){
        d<-EvaluationTable[i,l]-EvaluationTable[k,l]
          d1<-EvaluationTable[k,l]-EvaluationTable[i,l]
#Definition of the six types of preference functions
          if (PreferenceFunction[l]=='Usual' & CriteriaMinMax[l]=='max'){
            if (d>0){
              Pl=1
#Definition of matrix (numAlt x (numAlt-1)) containing the aggregated preference indices expressing how an alternative a is preferred to b.
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='Usual' & CriteriaMinMax[l]=='min'){
            if (d1>0){
              Pl=1
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
          }
          else if (PreferenceFunction[l]=='U-shape' & CriteriaMinMax [l]=='max'){
            if (d>IndifferenceThreshold[l]){
              Pl=1
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='U-shape' & CriteriaMinMax[l]=='min'){
            if (d1>IndifferenceThreshold[l]){
              Pl=1
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
            
          }
          
          else if (PreferenceFunction[l]=='V-shape' & CriteriaMinMax[l]=='max'){
            if (d>PreferenceThreshold[l]){
              Pl=1
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
            
            else if ((d<=PreferenceThreshold[l])&(d>=0)){
              Pl=d/(PreferenceThreshold[l])
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
          }
          else if (PreferenceFunction[l]=='V-shape' & CriteriaMinMax[l]=='min'){
            if (d1>PreferenceThreshold[l]){
              Pl=1
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
            else if ((d1<=PreferenceThreshold[l])&(d1>=0)){
              Pl=d1/(PreferenceThreshold[l])
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='Level' & CriteriaMinMax[l]=='max'){
            if (d>PreferenceThreshold[l]){
              Pl=1
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
            
            else if ((d<=PreferenceThreshold[l])&(d>IndifferenceThreshold[l])){
              Pl=0.5
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
          }
          else if (PreferenceFunction[l]=='Level' & CriteriaMinMax[l]=='min'){
            if (d1>PreferenceThreshold[l]){
              Pl=1
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
            else if ((d1<=PreferenceThreshold[l])&(d1>IndifferenceThreshold[l])){
              Pl=0.5
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='V-shape-Indiff' & CriteriaMinMax[l]=='max'){
            if (d>PreferenceThreshold[l]){
              Pl=1
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
            
            else if ((d<=PreferenceThreshold[l])&(d>IndifferenceThreshold[l])){
              Pl=(d-IndifferenceThreshold[l])/(PreferenceThreshold[l]-IndifferenceThreshold[l])
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='V-shape-Indiff' & CriteriaMinMax[l]=='min'){
            if (d1>PreferenceThreshold[l]){
              Pl=1
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
            else if ((d1<=PreferenceThreshold[l])&(d1>IndifferenceThreshold[l])){
              Pl=(d1-IndifferenceThreshold[l])/(PreferenceThreshold[l]-IndifferenceThreshold[l])
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='Gaussian' & CriteriaMinMax[l]=='max'){
            if (d>0){
              Pl=1-exp(-((d^2)/(2*GaussParameter[l]^2)))
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
          }
          else if (PreferenceFunction[l]=='Gaussian' & CriteriaMinMax[l]=='min'){
            if (d1>0){
              Pl=1-exp(-(((d1)^2)/(2*GaussParameter[l]^2)))
              PreferenceTablePos[i,j]<-PreferenceTablePos[i,j]+Pl*CriteriaWeights[l]}
            
          }
# Definition of matrix (numAlt x (numAlt-1)) containing the aggregated preference indices expressing how an alternative b is preferred to a.
          
          if (PreferenceFunction[l]=='Usual' & CriteriaMinMax[l]=='max'){
            if (d1>0){
              Pl=1
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='Usual' & CriteriaMinMax[l]=='min'){
            if (d>0){
              Pl=1
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
          }
          else if (PreferenceFunction[l]=='U-shape' & CriteriaMinMax [l]=='max'){
            if (d1>IndifferenceThreshold[l]){
              Pl=1
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='U-shape' & CriteriaMinMax[l]=='min'){
            if (d>IndifferenceThreshold[l]){
              Pl=1
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
            
          }
          
          else if (PreferenceFunction[l]=='V-shape' & CriteriaMinMax[l]=='max'){
            if (d1>PreferenceThreshold[l]){
              Pl=1
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
            
            else if ((d1<=PreferenceThreshold[l])&(d1>=0)){
              Pl=d1/(PreferenceThreshold[l])
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
          }
          else if (PreferenceFunction[l]=='V-shape' & CriteriaMinMax[l]=='min'){
            if (d>PreferenceThreshold[l]){
              Pl=1
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
            else if ((d<=PreferenceThreshold[l])&(d>=0)){
              Pl=d/(PreferenceThreshold[l])
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='Level' & CriteriaMinMax[l]=='max'){
            if (d1>PreferenceThreshold[l]){
              Pl=1
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
            
            else if ((d1<=PreferenceThreshold[l])&(d1>IndifferenceThreshold[l])){
              Pl=0.5
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
          }
          else if (PreferenceFunction[l]=='Level' & CriteriaMinMax[l]=='min'){
            if (d>PreferenceThreshold[l]){
              Pl=1
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
            else if ((d<=PreferenceThreshold[l])&(d>IndifferenceThreshold[l])){
              Pl=0.5
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='V-shape-Indiff' & CriteriaMinMax[l]=='max'){
            if (d1>PreferenceThreshold[l]){
              Pl=1
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
            
            else if ((d1<=PreferenceThreshold[l])&(d1>IndifferenceThreshold[l])){
              Pl=(d1-IndifferenceThreshold[l])/(PreferenceThreshold[l]-IndifferenceThreshold[l])
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='V-shape-Indiff' & CriteriaMinMax[l]=='min'){
            if (d>PreferenceThreshold[l]){
              Pl=1
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
            else if ((d<=PreferenceThreshold[l])&(d>IndifferenceThreshold[l])){
              Pl=(d-IndifferenceThreshold[l])/(PreferenceThreshold[l]-IndifferenceThreshold[l])
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='Gaussian' & CriteriaMinMax[l]=='max'){
            if (d1>0){
              Pl=1-exp(-(((d1)^2)/(2*GaussParameter[l]^2)))
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
          }
          else if (PreferenceFunction[l]=='Gaussian' & CriteriaMinMax[l]=='min'){
            if (d>0){
              Pl=1-exp(-(((d)^2)/(2*GaussParameter[l]^2)))
              PreferenceTableNeg[i,j]<-PreferenceTableNeg[i,j]+Pl*CriteriaWeights[l]}
          }
          
        }
        
      }
      
    }
  }

list(PreferenceTablePos=PreferenceTablePos,PreferenceTableNeg=PreferenceTableNeg)
}




























