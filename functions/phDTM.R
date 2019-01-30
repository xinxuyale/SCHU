# DTM Persistent Homology

# Input variables:

# X: the dataset, which should be an n by 3 matrix. 
# by: the grid size parameter, indicating the space between grid points, which is a number.
# m0: the smoothing parameter for DTM function, which is a number.

# Output variable:

# diagram: a p by 3 matrix. (p is the number of homology group generators.) The three columns are
# dimension (dimension of this homology group), Birth (birth time of this homology group) and 
# Death (death time of this homology group).
# birthLocation: a p by 3 matrix. (p is the number of homology group generators.) Each row gives 
# the coordinate of the grid point finishing the simplex that gives birth to a homology group.
# deathLocation: a p by 3 matrix. (p is the number of homology group generators.) Each row gives 
# the coordinate of the grid point finishing the simplex that kills a homology group.
# cycleLocation: a list of length p. (p is the number of homology group generators.) In each element 
# of the list, it is a k by h by 3 matrix. (k is the number of simplices in this homology group generator.
# h is the number of points constitutes the simplices: 1 for points, 2 for edges, 3 for faces.) 

phDTM <- function(X, by, m0){


# Build the grid ranges. 
# Xlim is the range vector of the grid for dimension X. 
# Xlim[1] is minimum and Xlim[2] is the maximum of the range for dimension X.
# Ylim and Zlim are similar.
# We set the grid ranges to be slightly (1/20 of the dataset ranges) larger than the dataset ranges. 
# Different grid ranges could also be considered.
Xlim <- c(range(X[,1])[1]-1/20*(range(X[,1])[2]-range(X[,1])[1]), range(X[,1])[2]+1/20*(range(X[,1])[2]-range(X[,1])[1]))
Ylim <- c(range(X[,2])[1]-1/20*(range(X[,2])[2]-range(X[,2])[1]), range(X[,2])[2]+1/20*(range(X[,2])[2]-range(X[,2])[1]))
Zlim <- c(range(X[,3])[1]-1/20*(range(X[,3])[2]-range(X[,3])[1]), range(X[,3])[2]+1/20*(range(X[,3])[2]-range(X[,3])[1]))


# Run DTM persistent homology.
# gridDiag is a function from package TDA. 
# Setting FUN to dtm will run persistent homology with DTM function. 
Diag <- gridDiag(X = X, FUN = dtm, m0=m0, lim = cbind(Xlim, Ylim, Zlim),
                 by = by, sublevel = TRUE, library = "Dionysus",
                 printProgress = TRUE, location=TRUE)

# Return the persistent homology object, which is a list of digram, birthLocation, deathLocation and cycleLocation.
return(Diag)

}


