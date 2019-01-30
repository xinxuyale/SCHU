# Calculate confidence bands.

# Input variables:

# res: result object from function bootDTM().
# dimension: confidence band for which dimension.
# level: confidence level wanted.

# Output:

# band: confidence band width used for plotting.

confBand <- function(res, dimension, level){

# boot variable saves bottleneck distances from bootstrap samples and 
# the original persistence diagram.
boot <- rep(0, length(res))

# Use function bottleneck() from TDA package to calculate bottleneck distances. 
for (i in 1:length(res)){
  boot[i] <- bottleneck(diagram, res[[i]], dimension = dimension)
}

# Use quantile() function to get confidence band width.
band <- 2*quantile(boot, level)

return(band)
}
