\name{PROMETHEE II}
\alias{PROMETHEEII}
\usage{
PROMETHEEII(EvaluationTable, PreferenceFunction,PreferenceThreshold,IndifferenceThreshold,
GaussParameter,CriteriaWeights,CriteriaMinMax)
}
\arguments{
  \item{EvaluationTable}{Matrix containing the evaluation table. 
    Each row corresponds to an alternative, and each column to a criterion. 
    Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
    
  \item{PreferenceFunction}{A vector with preference functions.PreferenceFunction should be equal to Usual,U-shape,V-shape,
  Level,V-shape-Indiff or Gaussian. The elements are named according to the IDs of the criteria.}
  \item{PreferenceThreshold} {A vector containing threshold of strict preference. The elements are named according to the IDs of the criteria.}
  \item{IndifferenceThreshold}{A vector containing threshold of indifference. The elements are named according to the IDs of the criteria.}
  \item{GaussParameter}{A vector containing parameter  of the Gaussian preference function. The elements are named according to the IDs of the criteria.}
    \item{CriteriaWeights}{Vector containing the weights of the criteria. 
    The elements are named according to the IDs of the criteria.}
  \item{CriteriaMinMax}{Vector containing the preference direction on each of the criteria. 
    "min" (resp. "max") indicates that the criterion has to be minimized (maximized). 
    The elements are named according to the IDs of the criteria.}
  
}

\value{
  The function returns two matrices: The first one contains the preference relations between the alternatives and the second one contains the indifference relations between the alternatives.