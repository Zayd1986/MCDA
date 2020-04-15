PROMETHEEPreferenceIndices<- function(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,GaussParameter,CriteriaWeights,CriteriaMinMax)
  
{ 
  # check the input data
  
  numAlt<-dim(EvaluationTable)[1] # number of alternatives
  numCrit<-dim(EvaluationTable)[2] # number of criteria
  
  if (!(is.matrix(EvaluationTable)))
    stop("wrong EvaluationTable, should be a matrix")
  
  if (!(is.vector(PreferenceFunction)))
    stop("PreferenceFunction should be a vector")
  
  for (j in (1:numCrit))
  {
    if (!(PreferenceFunction[j] %in% c("Usual","U-shape","V-shape","Level","V-shape-Indiff","Gaussian")))
    {
      stop("wrong PreferenceFunction, should be equal to Usual,U-shape,V-shape,Level,V-shape-Indiff or Gaussian")
    }
  }
   
  
  if (!(is.vector(PreferenceThreshold)))
    stop("PreferenceThreshold should be a vector")
  
  if (!(is.vector(IndifferenceThreshold)))
    stop("IndifferenceThreshold should be a vector")
  
  if (!(is.vector(GaussParameter)))
    stop("GaussParameter should be a vector")
  
  
  if (!(is.vector(CriteriaMinMax)))
    stop("CriteriaMinMax should be a vector")
  
  if (!(is.vector(CriteriaWeights)))
    stop("CriteriaWeights should be a vector")
  # -------------------------------------------------------
  

  PreferenceTable<-matrix(rep(0,numAlt*numAlt),numAlt,numAlt)
#Pairwise comparisons of evaluation criteria
  for(i in (1:numAlt)){
    for(j in (1:numAlt)){
      if (i==j)
        PreferenceTable[i,j]=0
      else
      {
      for(l in (1:numCrit)){
        d<-EvaluationTable[i,l]-EvaluationTable[j,l]
          d1<- -d
#Definition of the six types of preference functions
          if (PreferenceFunction[l]=='Usual' & CriteriaMinMax[l]=='max'){
            if (d>0){
              Pl=1
#Definition of matrix (numAlt x numAlt) containing the aggregated preference indices 
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='Usual' & CriteriaMinMax[l]=='min'){
            if (d1>0){
              Pl=1
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
          }
          else if (PreferenceFunction[l]=='U-shape' & CriteriaMinMax [l]=='max'){
            if (d>IndifferenceThreshold[l]){
              Pl=1
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='U-shape' & CriteriaMinMax[l]=='min'){
            if (d1>IndifferenceThreshold[l]){
              Pl=1
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
            
          }
          
          else if (PreferenceFunction[l]=='V-shape' & CriteriaMinMax[l]=='max'){
            if (d>PreferenceThreshold[l]){
              Pl=1
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
            
            else if ((d<=PreferenceThreshold[l])&(d>=0)){
              Pl=d/(PreferenceThreshold[l])
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
          }
          else if (PreferenceFunction[l]=='V-shape' & CriteriaMinMax[l]=='min'){
            if (d1>PreferenceThreshold[l]){
              Pl=1
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
            else if ((d1<=PreferenceThreshold[l])&(d1>=0)){
              Pl=d1/(PreferenceThreshold[l])
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='Level' & CriteriaMinMax[l]=='max'){
            if (d>PreferenceThreshold[l]){
              Pl=1
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
            
            else if ((d<=PreferenceThreshold[l])&(d>IndifferenceThreshold[l])){
              Pl=0.5
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
          }
          else if (PreferenceFunction[l]=='Level' & CriteriaMinMax[l]=='min'){
            if (d1>PreferenceThreshold[l]){
              Pl=1
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
            else if ((d1<=PreferenceThreshold[l])&(d1>IndifferenceThreshold[l])){
              Pl=0.5
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='V-shape-Indiff' & CriteriaMinMax[l]=='max'){
            if (d>PreferenceThreshold[l]){
              Pl=1
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
            
            else if ((d<=PreferenceThreshold[l])&(d>IndifferenceThreshold[l])){
              Pl=(d-IndifferenceThreshold[l])/(PreferenceThreshold[l]-IndifferenceThreshold[l])
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='V-shape-Indiff' & CriteriaMinMax[l]=='min'){
            if (d1>PreferenceThreshold[l]){
              Pl=1
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
            else if ((d1<=PreferenceThreshold[l])&(d1>IndifferenceThreshold[l])){
              Pl=(d1-IndifferenceThreshold[l])/(PreferenceThreshold[l]-IndifferenceThreshold[l])
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
          }
          
          else if (PreferenceFunction[l]=='Gaussian' & CriteriaMinMax[l]=='max'){
            if (d>0){
              Pl=1-exp(-((d^2)/(2*GaussParameter[l]^2)))
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
          }
          else if (PreferenceFunction[l]=='Gaussian' & CriteriaMinMax[l]=='min'){
            if (d1>0){
              Pl=1-exp(-(((d1)^2)/(2*GaussParameter[l]^2)))
              PreferenceTable[i,j]<-PreferenceTable[i,j]+Pl*CriteriaWeights[l]}
            
          }
    }
    }
    }
    }
  return(PreferenceTable)
    }



























