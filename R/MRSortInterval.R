MRSortInterval<-function(performanceTable,categorieslowerprofiles,criteriaweights,criteriaminmax,majoritythresholdpes,majoritythresholdopt){
  
  numcrit<-dim(performanceTable)[2]
  numalt<-dim(performanceTable)[1]
  #Definition of pessimistic and optimistic  versions of an alternative
  performanceTablepes=performanceTableopt<-as.list(rep(0,numalt*numcrit),c(numalt,numcrit))
  dim(performanceTablepes)=dim(performanceTableopt)<- c(numalt,numcrit)
  for (i in (1:numalt)){
    for (j in (1:numcrit)){
      if(is.numeric(performanceTable[[i,j]])) 
      {
        
        performanceTablepes[[i,j]]<-range(performanceTable[[i,j]])[1]
        performanceTableopt[[i,j]]<-range(performanceTable[[i,j]])[2]
      }
      
      else 
      {
        performanceTablepes[[i,j]]<-range(performanceTable[,j],na.rm=TRUE)[1]
        performanceTableopt[i,j]<-range(performanceTable[,j],na.rm=TRUE)[2]
      }
      
    }
  }
  performanceTableopt<-matrix(as.numeric(unlist(performanceTableopt)),numalt,numcrit)
  performanceTablepes<-matrix(as.numeric(unlist(performanceTablepes)),numalt,numcrit)
  rownames(performanceTablepes)=rownames(performanceTableopt)<-rownames(performanceTable)  
  colnames(performanceTablepes)=colnames(performanceTableopt)<-colnames(performanceTable)
  # The parameters of the MRSort function 
  categoriesranks<-c( 1:((dim(categorieslowerprofiles)[1])+1))
  names(categoriesranks)<-rownames(categorieslowerprofiles)
  names(categoriesranks)[categoriesranks == length(categoriesranks)]<-"C1"
  #The lower bound and the upper bound for an assignment using its pessimistic and its optimistic versions respectively  
  assignmentspes<-MRSort(performanceTablepes,categorieslowerprofiles,categoriesranks,criteriaweights,criteriaminmax,majoritythresholdpes)
  assignmentsopt<-MRSort(performanceTableopt,categorieslowerprofiles,categoriesranks,criteriaweights,criteriaminmax,majoritythresholdopt)
  hpes<-NULL
  hopt<-NULL
  for (i in 1:length(assignmentsopt))
  {
    hpes[i]<-categoriesranks[assignmentspes[i]]
    hopt[i]<-categoriesranks[assignmentsopt[i]]
  }
  assignments<-mapply(seq,hpes,hopt)
  names(assignments)<-rownames(performanceTable)
  
  assignmentsfinal<-vector(mode = "list", length = length(assignments))
  
  for (i in 1:length(assignments))
  {
    for (j in 1:length(assignments[[i]]))
    {
      assignmentsfinal[[i]][j]<-names(categoriesranks[assignments[[i]][j]])
    }
  }
  names(assignmentsfinal)<-rownames(performanceTable)
  return(assignmentsfinal)
}
