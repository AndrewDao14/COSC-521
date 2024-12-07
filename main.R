library(igraph)
library(openxlsx)
source("function.R")

node.count = 10
obstacles <- data.frame(# all obstacles are round for now
  name = c(1),
  x = c(2500), #x coordinates
  y = c(2500), #y coordinates
  r = c(1000) #radius
)
#empty data frame to save the data
relay.count <- node.count - 2
edge.data <- data.frame(matrix(nrow = 0, ncol = number.of.hops*choose(relay.count, number.of.hops-1))) 
colnames(edge.data) <- seq_len(ncol(edge.data))

#edge data for optimal path
path.data.op <- data.frame()

#path data for all path
path.data <- data.frame(matrix(nrow = 0, ncol = 3*(relay.count - 2))) 
colnames(path.data) <- seq_len(ncol(path.data))

#path data for all path

for (turn.num in 1:25){
  print(sprintf("Turn num %d", turn.num))
  set.seed(turn.num)
  # Initialize nodes data frame
  nodes <- data.frame(name = 1:node.count, x = NA, y = NA)
  
  # Generate each node's coordinates
  for (i in 1:node.count) {
    repeat {
      # Generate random coordinates
      x <- ifelse(i == 1 || i == node.count, (i - 1)*1000, runif(1, min = 0, max = 5000))
      y <- ifelse(i == 1 || i == node.count, (i - 1)*1000, runif(1, min = 0, max = 5000))
      
      # Check if the point is outside the circle
      if (!is_within_circle(x, y, c(obstacles$x[1], obstacles$y[1]), obstacles$r[1])) {
        nodes$x[i] <- x
        nodes$y[i] <- y
        break
      }
    }
    nodes$x[node.count] = 5000
    nodes$y[node.count] = 5000
  }
  
  d_matrix <- dist.matrix.from.field(nodes, obstacles)
  
  graph.object <- graph_from_adjacency_matrix(d_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
  #Plot the field
  #plot nodes
  plot(nodes$x, nodes$y, 
       xlab = "X Coordinate", ylab = "Y Coordinate", 
       main = paste("Plot ", turn.num), 
       pch = 16,  # Use solid circles for the points
       col = "blue",  # Color of the points
       xlim = c(0, 5000), ylim = c(0, 5000),# Set limits for x and y axes
       asp = 1)  
  
  #plot obstacles
  symbols(obstacles$x[1], obstacles$y[1], circles = obstacles$r[1], inches = FALSE, add = TRUE, 
          fg = "red", bg = NA, lwd = 2)  # Add a circle at (2, 2) with radius 1
  
  #Only uncomment this second line if there is a second obstacle
  # symbols(obstacles$x[2], obstacles$y[2], circles = obstacles$r[2], inches = FALSE, add = TRUE, 
  #         fg = "red", bg = NA, lwd = 2)  # Add a circle at (2, 2) with radius 1
  text(nodes$x, nodes$y, labels = nodes$name, pos = 4, col = "red", cex = 1.2)
  
  #This is the shortest longest path
  #optimal.path(graph.object,1,node.count, hops = 2)
  d_sd <- calculate_distance(nodes$x[1], nodes$y[1], nodes$x[node.count], nodes$y[node.count])
  optimal_path <- optimal.path.op(graph.object,1,node.count, hops = 2, d_sd = d_sd)
  print(optimal_path)
  
  paths <- all.available.paths(graph.object,1,node.count, hops = 2)
  print(paths)
  if (length(paths) > 0){
    row.data <- data.frame(matrix(nrow = number.of.hops, ncol = 0)) 
    path.row.data <- data.frame(matrix(nrow = 1, ncol = 0))
    for (path in paths){
      edge.dist.table <- edge.distance(list(path), nodes, obstacles)
      row.data <- cbind(row.data, edge.dist.table)
      path.row.data <- cbind(path.row.data, as.data.frame(t(path))) #Have to transpose path
    }
    #edge.dist.table <- edge.distance(paths[1], nodes, obstacles)
    remaining.col <- number.of.hops*choose(relay.count, number.of.hops-1) - ncol(row.data)
    row.data[paste0(1:remaining.col)] <- NA
    colnames(row.data) <- seq_len(ncol(row.data))
    
    #same thing but for path
    path.remaining.col <- 3*relay.count - ncol(path.row.data)
    path.row.data[paste0(1:path.remaining.col)] <- NA
    colnames(path.row.data) <- seq_len(ncol(path.row.data))
    
    #Bind rows
    edge.data <- rbind(edge.data, row.data)
    path.data <- rbind(path.data, path.row.data)
    
    #Bind rows for best edge data
    edge.dist.table.op <- edge.distance(list(optimal_path), nodes, obstacles)
    edge.data.op <- rbind(edge.data.op,edge.dist.table.op)
    path.data.op <- rbind(path.data.op,optimal_path)
    
  } else{
    print("No Available paths")
  }
}



#Run This code so that data can be saved to Excel file only
#work.book <- createWorkbook()
addWorksheet(work.book, "1-10,2,AAP,PATH")
addWorksheet(work.book, "1-10,2,OPO,PATH")

writeData(work.book, sheet = "1-10,2,AAP,PATH", path.data)
writeData(work.book, sheet = "1-10,2,OPO,PATH", path.data.op)
saveWorkbook(work.book, "Data_collect.xlsx", overwrite = TRUE)