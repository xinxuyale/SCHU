# Calculate p-values

# Input:

# diagram: a p by 3 matrix. The first element of outputs from function phDTM().
# res: result object from function bootDTM().

# Output:

# diagram: a p by 4 matrix. The original diagram variable with a new column p-value added.

pValue <- function(diagram, res){
  
  # boot variables save different dimensional bottleneck distances from bootstrap samples and 
  # the original persistence diagram.
  boot0 <- rep(0, length(res))
  boot1 <- rep(0, length(res))
  boot2 <- rep(0, length(res))
  
  # Use function bottleneck() from TDA package to calculate different dimensional
  # bottleneck distances. 
  for (i in 1:length(res)){
    boot0[i] <- bottleneck(diagram, res[[i]], dimension = 0)
    boot1[i] <- bottleneck(diagram, res[[i]], dimension = 1)
    boot2[i] <- bottleneck(diagram, res[[i]], dimension = 2)
  }
  
  # Add two new columns: half persistence and p-value.
  # Calculate half persistences by column 2 and 3.
  # Calculate p-values using the bootstrap distribution of bottleneck distances. 
  # The proportion of bottleneck distances larger than the half persistence of 
  # a homology group is its p-value.
  diagram <- cbind(diagram, rep(0, dim(diagram)[1]), rep(0, dim(diagram)[1]))
  colnames(diagram)[4:5] <- c("HalfPersistence", "PValue")
  diagram[,4] <- (diagram[,3]-diagram[,2])/2
  for (i in 1:dim(diagram)[1]) {
    if(diagram[i,1] == 0)  diagram[i,5] <- sum(boot0>diagram[i,4])/length(boot0)
    if(diagram[i,1] == 1)  diagram[i,5] <- sum(boot1>diagram[i,4])/length(boot1)
    if(diagram[i,1] == 2)  diagram[i,5] <- sum(boot2>diagram[i,4])/length(boot2)
  }
  
  
  # Return the new diagram variable. A new column p-value is added.
  return(diagram[,c(1:3, 5)])
  
}

