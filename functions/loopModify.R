# Filament Loop Modification
# Exclude extra loops.

# Input variables:

# Diag: result object from function phDTM().
# loop_ind: indices (in Diag$diagram) of filament loops to modify.

# Output variable:

# rep: a list of the same length as loop_ind. Each element is the 
# modified representation for the corresponding filament loop.

loopModify <- function(Diag, loop_ind){

# Create a list to save results.
rep <- list()

# Repeat for each filament loop.
for(i in 1:length(loop_ind)){
  
  # Create a variable temp to save representation returned by phDTM().
  temp <- Diag[["cycleLocation"]][[loop_ind[i]]]
 
  # Representations for loops are composed of edges. Save the first vertex of
  # each edge to variable first and save the second vertex of each edge to 
  # variable second.
  first <- temp[,1,]
  second <- temp[,2,]
  
  # Combine the two matrices to one matrix called points. Each row of matrix points
  # are coordinates for two vertices that compose an edge in the representation.
  points <- cbind(first, second)
  
  # Create a list variable calle loop to save loops.
  loop <- list()
  
  # k is index for each loop. Start at 1.
  k <- 1
  
  # Create a variable to save loop lengths.
  loop_len <- 0
  
  # Put the first row of matrix points to the first loop. 
  loop[[k]] <- rbind(points[1,1:3], points[1,4:6])
  
  # Save the second vertex of the first edge to be 'current point'.
  cur <- points[1,4:6]
  
  # Delete the first row from matrix points.
  points <- points[-1,]
  
  # While matrix points contains points, do the following.
  while (length(dim(points))>0){
    
    # Check if any vertex in matrix points equals the current point. 
    if (any(points[,1]==cur[1] & points[,2]==cur[2] & points[,3]==cur[3]))  a <- 0
    if (any(points[,4]==cur[1] & points[,5]==cur[2] & points[,6]==cur[3]))  a <- 3
    
    # If there exists a vertex that equals the current point, do the following.
    if (a >= 0){
      
      # Save the index of this vertex in variable ind.
      ind <- which(points[,1+a]==cur[1] & points[,2+a]==cur[2] & points[,3+a]==cur[3])[1]
      
      # Add this vertex into loop[[k]].
      loop[[k]] <- rbind(loop[[k]], points[ind,1:3+(3-a)])
      
      # Update the vertex in the same row of this vertex to be current point.
      cur <- points[ind,1:3+(3-a)]
      
      # Delete this row.
      points <- points[-ind,]
      
      # Set a to -1.
      a <- -1
    } 
    
    # If there is not a vertex that equals the current point, do the following.
    else {
      
      # Save the curent loop length into variable loop_len.
      loop_len <- c(loop_len, dim(loop[[k]])[1])
      
      # Increase k by 1.
      k <- k+1
      
      # Start a new loop by adding the first row into loop[[k]] again.
      loop[[k]] <- rbind(points[1,1:3], points[1,4:6])
      
      # Set current point.
      cur <- points[1,4:6]
      
      # Delete this row.
      points <- points[-1,]
    }
  }
  
  # Add the last loop length into variable loop_len.
  loop_len <- c(loop_len, dim(loop[[k]])[1])
  
  # Delete the first 0 from variable loop_len.
  loop_len <- loop_len[-1]

  # Save the largest loop as the modified representation.
  rep[[i]] <- loop[[which.max(loop_len)]]

}

return(rep)

}

