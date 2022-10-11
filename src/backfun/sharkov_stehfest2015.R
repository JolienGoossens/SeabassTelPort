# Kilian M. Stehfest, Toby A. Patterson, Adam Barnett, Jayson M. Semmens

# Markov models and network analysis reveal sex-specific differences in the space-use of a coastal apex predator

# R code for the 'sharkov' function which computes the transition probability matrix
# or the dominant eigenvector of the transpose of the transition probability matrix from passive acoustic telemetry data

# Input data is a dataframe of hourly acoustic detections from an array of passive acoustic receivers with columns:
# id = factor vector of individual identifiers,
# time = the hourly detection time/date stamps in POSIXct format (YYYY-MM-DD hh:mm:ss)
# state = the receiver or receiver group identifier, can be numeric or character but must not be '0' as this is defined as 'no detection' i.e. the absent state in the code

# If return.matrix=TRUE, the Markov probability matrix is returned, otherwise the dominant eigenvetor of the probability matrix is returned
# niter determines the number of iterations for the power method computation of the dominant eigenvector

# Please note that this method is likely to be slow for large datasets and could be made more efficient with compiled code

sharkov<-function(data, return.matrix=TRUE, niter=10000, time_res = "hours", include_absence = FALSE) {
  
  # Create regular hourly time sequence and add states (state 0 = unknown (i.e. no detection))
  
  tseq<-expand.grid(id=unique(data$id), time=seq(min(data$time), max(data$time), time_res))
  tseq = left_join(tseq, data)
  if (include_absence){
    tseq$state[is.na(tseq$state)]<-"Absent"
  } else {
    tseq = tseq %>%  filter(!is.na(state))
  }
  
  newdata = as.data.frame(tseq)
  
  # newdata<- plyr::ldply(split(data, as.factor(data$id)), makeseq)
  
  
  # Compute Markov transition probability matrix
  maketran<-function(X, states) {
    # Create empty transition matrix array
    tran<-array(dim=c(length(states),length(states)))
    colnames(tran)<-states
    rownames(tran)<-states
    tran[,]<-0
    
    #Fill transition matrices with transition counts
    for (i in 1:nrow(X)) {
      tran[as.character(X[i-1, "state"]), as.character(X[i, "state"])]<-tran[as.character(X[i-1, "state"]), as.character(X[i, "state"])]+1
    }
    return(tran)
  }
  
  states<-unique(newdata$state)
  tranmat<-lapply(split(newdata, newdata$id), maketran, states)
  
  # Sum number of transitions for all individuals and calculate transition probabilities by dividing each row element by the row sum
  total<-Reduce('+', tranmat)
  for (i in 1:nrow(total)) {
    total[i,]<-total[i,]/rowSums(total)[i]
  }
  
  if (return.matrix) {return(total)}
  else {
    # Compute dominant eigenvector of the transpose of the transition probability matrix using the power method
    power.method<-function(Mat, iter){
      n<-nrow(Mat)
      x <- rep(1,n)
      for (i in 1:iter) {x <- Mat%*%x}
      de<-x/sum(x)
      return(de)
    }
    eigenv<-power.method(t(total), niter)
    return(eigenv)
  }
}

