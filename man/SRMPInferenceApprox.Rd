\name{SRMPInferenceApprox}
\alias{SRMPInferenceApprox}

\title{Approximative inference of an SRMP model}

\description{Approximative inference approach from pairwise comparisons of alternatives for the SRMP ranking model. This method outputs an SRMP model that fulfils as many pairwise comparisons as possible. Neither the number of reference profiles, nor the lexicographic order are fixed beforehand, however a maximum value for the number of reference profiles needs to be provided.}

\usage{
SRMPInferenceApprox(performanceTable, criteriaMinMax,
                    maxProfilesNumber, preferencePairs, indifferencePairs = NULL,
                    alternativesIDs = NULL, criteriaIDs = NULL,
                    timeLimit = 60,populationSize = 100, mutationProb = 0.5)
}

\arguments{
  \item{performanceTable}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria. "min" (resp. "max") indicates that the criterion has to be minimized (maximized).  The elements are named according to the IDs of the criteria.}
  \item{maxProfilesNumber}{The maximum number of reference profiles of the SRMP model.}
  \item{preferencePairs}{A two column matrix containing on each row a pair of alternative names where the first alternative is considered to be strictly preferred to the second.}
    \item{indifferencePairs}{A two column matrix containing on each row a pair of alternative names the two alternatives are considered to indifferent with respect to each other.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the datashould be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
  \item{timeLimit}{Allows to fix a time limit of the execution, in seconds (default 60).}
  \item{populationSize}{Allows to change the size of the population used by the genetic algorithm (default 100).}
  \item{mutationProb}{Allows to change the mutation probability used by the genetic algorithm (default 0.5).}
}

\value{
  The function returns a list containing:
  \item{weights}{The inferred criteria weights.}
  \item{referenceProfilesNumber}{The number of inferred reference profiles.}
  \item{referenceProfiles}{The inferred reference profiles.}
  \item{lexicographicOrder}{The inferred lexicographic order of the reference profiles.}
  \item{fitness}{The percentage of fulfilled pair-wise relations.}
}

\references{
A-L. OLTEANU, V. MOUSSEAU, W. OUERDANE, A. ROLLAND, Y. ZHENG, Preference Elicitation for a
Ranking Method based on Multiple Reference Profiles, forthcoming 2018.
}

\examples{
# the performance table

performanceTable <- rbind(c(10,10,9),c(10,9,10),c(9,10,10),c(9,9,10),c(9,10,9),c(10,9,9),
                          c(10,10,7),c(10,7,10),c(7,10,10),c(9,9,17),c(9,17,9),c(17,9,9),
                          c(7,10,17),c(10,17,7),c(17,7,10),c(7,17,10),c(17,10,7),c(10,7,17),
                          c(7,9,17),c(9,17,7),c(17,7,9),c(7,17,9),c(17,9,7),c(9,7,17))

criteriaMinMax <- c("max","max","max")

rownames(performanceTable) <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12",
                                "a13","a14","a15","a16","a17","a18","a19","a20","a21","a22",
                                "a23","a24")

colnames(performanceTable) <- c("c1","c2","c3")

names(criteriaMinMax) <- colnames(performanceTable)

preferencePairs <- matrix(c("a16","a13","a3","a14","a17","a1","a18","a15","a2","a11","a5",
                            "a10","a4","a12","a13","a3","a14","a17","a1","a18","a15","a2",
                            "a11","a5","a10","a4","a12","a6"),14,2)
indifferencePairs <- matrix(c("a3","a1","a2","a11","a11","a20","a10","a10","a19","a12","a12",
                              "a21","a9","a7","a8","a20","a22","a22","a19","a24","a24","a21",
                              "a23","a23"),12,2)

result<-SRMPInferenceApproxFixedProfilesNumber(performanceTable, criteriaMinMax,
                                              3, preferencePairs, indifferencePairs)
}
\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}