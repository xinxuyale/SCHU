# Significant Cosmic Holes in Universe (SCHU)

source("functions/confBand.R")
source("functions/bootDTM.R")
source("functions/loopModify.R")
source("functions/phDTM.R")
source("functions/pValue.R")

# Load in TDA package and rgl package (for 3D image).
library("TDA")
library("rgl")

# Load in your dataset. 
# Here I am creating a toy example by sampling from a sphere and a circle.
set.seed(2018)
X1 <- sphereUnif(150, 2, r = 2) 
X2 <- cbind(circleUnif(150, r=3), runif(150, 2, 2.5))
X <- rbind(X1, X2)

# Plot in 3D.
plot3d(X, xlab=expression("X"[1]), ylab=expression("X"[2]), zlab=expression("X"[3]))

# Run persistent homology with DTM function. 
# Assign values for parameters "by" and "m0" first. To compute DTM, it generates a grid 
# and parameter "by" is grid size. m0 is the smoothing parameter in the definition of DTM.
# Use function phDTM() to get persistence homology results.
# Diag is a list, containing diagram, birthLocation, deathLocation and cycleLocation.
by <- 0.25
m0 <- 0.05
Diag <- phDTM(X, by, m0)

# Show persistence diagram.
diagram <- Diag$diagram
plot(diagram)

# Do bootstrap using function bootDTM().
# B: Bootstrap time. 
B <- 10

# Save results into variable res. 
# res is a list. Each element of this list is a "diagram" object.
res <- bootDTM(X, by, m0, B)

# Calculate confidence bands.
# level: confidence level.
# dimension: dimension of confidence band.
# band is a numeric variable saving the width to draw the confidence band.
level <- 0.9
dimension <- 1
band <- confBand(res, dimension, level)

# Plot a persistence diagram with confidence band.
plot(diagram, band = band)

# Calculate P values
# The new diagram variable has a new column: p-value.
diagram <- pValue(diagram, res)

# Select filament loops with p-values less than or equal to0.1.
# loop_ind is a vector (or integer) saving indices (or idnex) of significant 
# filament loop(s).
loop_ind <- which(diagram[,1]==1 & diagram[,4]<=0.1)
diagram[loop_ind,]

# Show the representation of filament loop.
plot3d(X, xlab=expression("X"[1]), ylab=expression("X"[2]), zlab=expression("X"[3]))
color <- rainbow(length(loop_ind))
for (i in 1:length(loop_ind)){
  temp <- unique(rbind(Diag[["cycleLocation"]][[loop_ind[i]]][,1,], Diag[["cycleLocation"]][[loop_ind[i]]][,2,]))
  points3d(temp, col=color[i])
}

# Run loop modification.
# rep is a list. Each element is representation points for the corresponding loop.
rep <- loopModify(Diag, loop_ind)

# Show the modified representation of filament loop.
plot3d(X, xlab=expression("X"[1]), ylab=expression("X"[2]), zlab=expression("X"[3]))
color <- rainbow(length(loop_ind))
for (i in 1:length(loop_ind)){
  points3d(rep[[i]], col=color[i])
}



# Select cosmic voids with p-values less than or equal to 0.1.
# void_ind is a vector (or integer) saving indices (or idnex) of significant 
# cosmic void(s).
void_ind <- which(diagram[,1]==2 & diagram[,4]<=0.1)
diagram[void_ind,]

# Show the representation of cosmic void.
plot3d(X, xlab=expression("X"[1]), ylab=expression("X"[2]), zlab=expression("X"[3]))
color <- rainbow(length(void_ind))
for (i in 1:length(void_ind)){
  temp <- unique(rbind(Diag[["cycleLocation"]][[void_ind[i]]][,1,], Diag[["cycleLocation"]][[void_ind[i]]][,2,], Diag[["cycleLocation"]][[void_ind[i]]][,3,]))
  points3d(temp, col=color[i])
}


