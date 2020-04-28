
# The evaluation table

EvaluationTable <- rbind(
  c(1,10,1),
  c(4,20,2),
  c(2,20,0),
  c(6,40,0),
  c(30,30,3))
rownames(EvaluationTable) <- c("RER","METRO1","METRO2","BUS","TAXI")
colnames(EvaluationTable) <- c("Price","Time","Comfort")

# The preference functions 
PreferenceFunction<-c("Gaussian","Level","V-shape-Indiff")

#Preference threshold
PreferenceThreshold<-c(5,15,3)
names(PreferenceThreshold)<-colnames(EvaluationTable)

#Indifference threshold
IndifferenceThreshold<-c(3,11,1)
names(IndifferenceThreshold)<-colnames(EvaluationTable)

#Parameter of the Gaussian preference function
GaussParameter<-c(4,0,0)
names(GaussParameter)<-colnames(EvaluationTable)

#weights

CriteriaWeights<-c(0.2,0.3,0.5)
names(CriteriaWeights)<-colnames(EvaluationTable)

# criteria to minimize or maximize

CriteriaMinMax<-c("min","min","max")
names(CriteriaMinMax)<-colnames(EvaluationTable)


# Matrix P containing preference relations between alternatives and matrix I containing indifference relations between alternatives 

PROMETHEEII(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,GaussParameter,CriteriaWeights,CriteriaMinMax)