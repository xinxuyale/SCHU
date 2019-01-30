# Analysis of Voronoi foam simulation data.
# Set the working directory to be the folder containing README.txt.

source("functions/bootBand.R")
source("functions/bootDTM.R")
source("functions/loopModify.R")
source("functions/phDTM.R")
source("functions/pValue.R")

library(rgl)
library(TDA)
load("VoronoiFoam/VoronoiFoam.RData")

# Show persistence diagram.
plot(Diag$diagram)

# Analyze cosmic voids.
# Select cosmic voids with p-values less than 0.1.
# void_ind save the indices of these significant cosmic voids.
void_ind <- which(diagram[,1]==2 & diagram[,4]<.1)
diagram[void_ind,]

# Show the representations of cosmic voids.
# plot3d is a function from package rgl that can plot 3D images.
plot3d(X, alpha=0.3, size=2, xlab=expression("X"[1]), ylab=expression("X"[2]), 
       zlab=expression("X"[3]))
color <- rainbow(length(void_ind))
for (i in 1:length(void_ind)){
  temp <- unique(rbind(Diag[["cycleLocation"]][[void_ind[i]]][,1,], Diag[["cycleLocation"]][[void_ind[i]]][,2,], Diag[["cycleLocation"]][[void_ind[i]]][,3,]))
  points3d(temp, col=color[i])
}



# Analyze filament loops.
# Select filament loops with p-values less than 0.1.
# void_ind save the indices of these significant cosmic voids.
loop_ind <- which(diagram[,1]==1 & diagram[,4]<.1)
diagram[loop_ind,]

# Show the representations of filament loops.
plot3d(X, alpha=0.3, size=2, xlab=expression("X"[1]), ylab=expression("X"[2]), 
       zlab=expression("X"[3]))
color <- rainbow(length(loop_ind))
for (i in 1:length(loop_ind)){
  temp <- unique(rbind(Diag[["cycleLocation"]][[loop_ind[i]]][,1,], Diag[["cycleLocation"]][[loop_ind[i]]][,2,]))
  points3d(temp, col=color[i])
}


# Run loop modification.
rep <- loopModify(Diag, loop_ind)

# Show the modified representations of filament loops.
plot3d(X, alpha=0.3, size=2, xlab=expression("X"[1]), ylab=expression("X"[2]), 
       zlab=expression("X"[3]))
color <- rainbow(length(loop_ind))
for (i in 1:length(loop_ind)){
  points3d(rep[[i]], col=color[i])
}

