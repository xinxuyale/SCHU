# Bootstrap for DTM persistent homology

# Input variables:

# X: dataset to use, which should be an n by 3 matrix. 
# by: grid size parameter, indicating the space between grid points, which is a number.
# m0: smoothing parameter for DTM function, which is a number.
# B: times of bootstraps.

# Output variable:

# res: a list of length B. Each element of this list is a p by 3 matrix. (p is 
# the number of homology group generators.) The three columns are dimension (dimension 
# of this homology group), Birth (birth time of this homology group) and Death 
# (death time of this homology group).


bootDTM <- function(X, by, m0, B){

# Create a list to save results.
res <- list()

# Repeat for B times.
for (i in 1:B){
  
  # I: index of randomly sampled points from original dataset.
  I <- sample(NROW(X), replace = TRUE, size = NROW(X))
  # Xstar: bootstrap sample. 
  Xstar <- X[I, , drop = FALSE]
  # Run function phDTM.
  res[[i]] <- phDTM(Xstar, by, m0)$diagram
}

# Return the list res.
return(res)
  
}

