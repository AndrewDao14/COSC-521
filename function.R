#extract all available paths
all.available.paths <- function(graph, from, to, hops = NA){
  paths <- all_simple_paths(graph, from=from, to = to)
  if(!is.na(hops)){
    paths <- Filter(function(path) length(path) == hops + 1, paths)
  }
  # Convert paths to a list of numeric vectors and filter out paths with Inf weights
  valid_paths <- Filter(function(path) {
    path <- as.numeric(path)  # Convert the path to a simple numeric vector
    edges <- E(graph, path = path)  # Get the edges along the path
    all(!is.infinite(edges$weight))  # Keep the path if no edge weight is Inf
  }, paths)
  
  # Convert each path to a numeric vector for easier handling
  valid_paths <- lapply(valid_paths, as.numeric)
  
  return(valid_paths)
}


#Best path between nodes on a graph base on distance
optimal.path <- function(graph, from, to, hops = NA){
  paths <- all_simple_paths(graph, from=from, to = to)
  if(!is.na(hops)){
    paths <- Filter(function(path) length(path) == hops + 1, paths)
  }
  best_path <- NULL
  min_longest_link <- Inf
  for (path in paths){
    path <- as.numeric(path) #turn the path into a list
    
    longest.link <- 0 #The longest link of each path
    for (i in 1:(length(path)-1)){
      #The distance of each hop
      hop.distance <- E(graph)[path[i] %--% path[i+1]]$weight
      
      if (hop.distance > longest.link){
        longest.link <- hop.distance
      }
      
    }
    if (min_longest_link > longest.link){
      min_longest_link <- longest.link
      best_path <- path
    }
  }
  
  return(best_path)
}

##Best path between nodes on a graph base on outage probability
#d_sd: distance between transmitter and receiver
optimal.path.op <- function(graph, from, to, hops = 2, d_sd){
  #paths <- all_simple_paths(graph, from=from, to = to)
  #paths <- Filter(function(path) length(path) == hops + 1, paths)
  
  paths <- all.available.paths(graph, from, to, hops)
  
  best_path <- NULL
  lowest.end.to.end.op <- Inf
  
  
  for (path in paths){
    #path <- as.numeric(path) #turn the path into a list
    edge.op <- list() #list of coutage probability of each edge
    K <- length(path) - 2 #minus the transmitter and receiver
    #print(path)
    for (k in 1:length(path)-1){
      dk <- E(graph)[path[k] %--% path[k+1]]$weight
      individual_op <- individual.op(dk = dk, K = K, d_sd = d_sd)
      edge.op <- append(edge.op, individual_op)
    }
    
    success.prob <- lapply(edge.op, function(x) 1 - x) #1-op for each element in the list
    end.to.end.op <- 1 - prod(unlist(success.prob)) # end to end outage probability
    #print(end.to.end.op)
    if (end.to.end.op < lowest.end.to.end.op){
      lowest.end.to.end.op <- end.to.end.op
      best_path <- path
    }
  }
  
  return(best_path)
  
}
#Individual OP of each hop
individual.op <- function(dk,#distance between the first and second node, in meter
                          P_M_dB = 5,#in dB 
                          K, #initialize to one relay node for now
                          d_sd, #distance between receiver and transmitter,
                          sigma = 0.1/1000, #m^-1
                          C2 = 10^(-14.5),
                          lambda = 1550 *10^-9 #wave function
                          ){
  k <- 2*pi/lambda
  L_dk <- ((d_sd/dk)^2)*exp(-sigma*(dk-d_sd))
  P_M_linear <- 10^(P_M_dB / 10)
  term1 <- 0.124 * k^(7/6)
  term2 <- C2 * dk^(11/6)
  sigma_k_squared <- min(term1*term2, 0.5)
  mu_k <- -sigma_k_squared
  # Calculate sigma_k (square root of sigma_k^2)
  sigma_k <- sqrt(sigma_k_squared)
  # Calculate the expression inside the Q-function
  numerator <- log((L_dk * P_M_linear) / (K + 1)) + 2 * mu_k
  denominator <- 2 * sigma_k
  z <- numerator / denominator
  
  # Calculate Q-function (tail probability of normal distribution)
  Q <- 1 - pnorm(z)
  return(Q)
}


# Define the calculate_distance function
calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

#Creating distance matrix from field data
dist.matrix.from.field <- function(field.data, obstacles = NA){
  #Creating graph object base on nodes
  n <- nrow(field.data) #n = number of nodes 
  distance_matrix <- matrix(Inf, n, n, dimnames = list(field.data$name, nodes$field.data))
  
  for (i in 1:n) {
    for (j in i:n) {
      link.avalable = TRUE
      if (all((i != j) & (!is.na(obstacles)))){
        ob.num <- nrow(obstacles)
        
        for (k in 1:ob.num){
          
          A <- c(field.data$x[i], field.data$y[i]) #node i
          B <- c(field.data$x[j], field.data$y[j]) #node j
          C <- c(obstacles$x[k], obstacles$y[k]) #center of the circle
          
          project.point <- project_point(A, B, C) #Projection of C on line AB
          C.on.AB <- is_point_on_segment(A, B, project.point) #If C is between A and B
          if (is_point_on_segment(A, B, project.point)){
            dist <- calculate_distance(project.point[1], project.point[2], C[1], C[2])
          }
          else{
            dist <- min(calculate_distance(A[1], A[2], C[1], C[2]),
                        calculate_distance(B[1], B[2], C[1], C[2]))
          }
          radius = obstacles$r[k]
          if (radius > dist){
            link.avalable = FALSE
            break
          }
        }
      }
      
      
      if (i == j |!link.avalable){
        dist <- Inf
      }
      else{
        dist <- calculate_distance(field.data$x[i], field.data$y[i],field.data$x[j], field.data$y[j])
        distance_matrix[i, j] <- dist
      }
      distance_matrix[j, i] <- dist  # Since the distance is symmetric
    }
  }
  return(distance_matrix)
}

#EDGE DISTANCE
edge.distance <-function(path, nodes, obstacles){#nodes = nodes dataframe. obstacles = obstacles dataframe
  path <- path[[1]]# to extract the vector from the structure that it was in
  edge.num = length(path)-1 #number of edges
  obs.num = nrow(obstacles) #number of obstacles
  #path <- as.numeric(path)
  return.table <- data.frame(matrix(NA, nrow = edge.num, ncol = obs.num))
  for (e in 1:edge.num){
    for (o in 1:obs.num){
      obs.radius <- obstacles$r[o]
      node.ID.1 <- path[e]
      node.ID.2 <- path[e+1]

      #Calculate node to pass on tp projection algorithm
      A <- c(nodes$x[node.ID.1], nodes$y[node.ID.1])
      B <- c(nodes$x[node.ID.2], nodes$y[node.ID.2])
      
      C <- c(obstacles$x[o], obstacles$y[o])
      
      #pass on to projection
      D <- project_point(A, B, C)
      #Calculate distance and compare to the radius
      project.dist <- calculate_distance(C[1], C[2], D[1], D[2])
      diff <- project.dist - obs.radius
      return.table[e, o] <- diff
    }
  }
  return(return.table)
}



########################################################
#Helper Function area
#######################################################

#Depth first search 
dfs <- function(current_node, path){
  # Stop if we've reached the exact step count
  if (length(path) == steps + 1) {
    # Check if the path ends at the target node
    if (current_node == end) {
      paths <<- append(paths, list(path))
    }
    return}
}

# Function to find the projection of C onto the line AB
project_point <- function(A, B, C) {
  # A, B, and C are vectors with x and y coordinates: c(x, y)
  
  # Calculate vectors AB and AC
  AB <- B - A
  AC <- C - A
  
  # Calculate the projection scalar t
  t <- sum(AC * AB) / sum(AB * AB)
  
  # Calculate the projection point D on the line AB
  D <- A + t * AB
  
  return(D)
}
is_point_on_segment <- function(A, B, C) {
  # A, B, and C are vectors with x and y coordinates: c(x, y)
  
  # Calculate vectors AB and AC
  AB <- B - A
  AC <- C - A
  
  
  # Check if C lies within the bounds defined by A and B
  within_bounds <- (
    C[1] >= min(A[1], B[1]) && C[1] <= max(A[1], B[1]) &&
      C[2] >= min(A[2], B[2]) && C[2] <= max(A[2], B[2])
  )
  
  #Return TRUE if within bounds
  if(C[1] >= min(A[1], B[1]) & C[1] <= max(A[1], B[1]) &
     C[2] >= min(A[2], B[2]) & C[2] <= max(A[2], B[2])){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

is_within_circle <- function(x, y, center, radius) {
  dist <- sqrt((x - center[1])^2 + (y - center[2])^2)
  return(dist < radius)
}


#Gaussian Q-function
# Define the Q-function
Q_function <- function(x) {
  return(1 - pnorm(x))
}