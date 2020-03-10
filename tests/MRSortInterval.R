library(MCDA)


# the performance table

performanceTable <- as.list(numeric(6*5))
dim(performanceTable)=c(6,5)
performanceTable[[1,1]]<-2
performanceTable[[1,2]]<-2
performanceTable[[1,3]]<-2
performanceTable[[1,4]]<-0
performanceTable[[1,5]]<-0
performanceTable[[2,1]]<-2
performanceTable[[2,2]]<-2
performanceTable[[2,3]]<-1
performanceTable[[2,4]]<-0
performanceTable[[2,5]]<-0
performanceTable[[3,1]]<-2
performanceTable[[3,2]]<-2
performanceTable[[3,3]]<-0
performanceTable[[3,4]]<-0
performanceTable[[3,5]]<-0
performanceTable[[4,1]]<-2
performanceTable[[4,2]]<-2
performanceTable[[4,3]]<-0:1
performanceTable[[4,4]]<-0
performanceTable[[4,5]]<-0
performanceTable[[5,1]]<-2
performanceTable[[5,2]]<-2
performanceTable[[5,3]]<-NA
performanceTable[[5,4]]<-0
performanceTable[[5,5]]<-0
performanceTable[[6,1]]<-2
performanceTable[[6,2]]<-2
performanceTable[[6,3]]<-2
performanceTable[[6,4]]<-0
performanceTable[[6,5]]<-NA

rownames(performanceTable)<-c("a1","a2","a3","a4","a5","a6")
colnames(performanceTable)<-c("crit1","crit2","crit3","crit4","crit5")

# lower profiles of the categories (best category in the first position of the list)

categoriesLowerProfiles <- rbind(c(1,1,1,1,1),c(2,2,2,2,2))
colnames(categoriesLowerProfiles) <- colnames(performanceTable)

rownames(categoriesLowerProfiles)<-c("C3","C2")

# weights

criteriaWeights <- c(1/5,1/5,1/5,1/5,1/5)
names(criteriaWeights) <- colnames(performanceTable)

#pessimistic and optimistic majority thresholds
majoritythresholdpes=majoritythresholdopt=3/5

# criteria to minimize or maximize

criteriaMinMax <- c("max","max","max","max","max")
names(criteriaMinMax) <- colnames(performanceTable)

#MRSortInterval

assignments<-MRSortInterval(performanceTable,categoriesLowerProfiles,criteriaWeights,criteriaMinMax,majoritythresholdpes,majoritythresholdopt)