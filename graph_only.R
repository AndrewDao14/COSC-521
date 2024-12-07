library(igraph)
library(openxlsx)
source("function.R")
set.seed(as.numeric(Sys.time()))
node.count = 10
obstacles <- data.frame(# all obstacles are round for now
  name = c(1,2),
  x = c(2500,5000), #x coordinates
  y = c(2500, 0), #y coordinates
  r = c(1000, 1000) #radius
)



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
     main = "Scatter Plot of 6 Nodes", 
     pch = 16,  # Use solid circles for the points
     col = "blue",  # Color of the points
     xlim = c(0, 5000), ylim = c(0, 5000),# Set limits for x and y axes
     asp = 1)  

#plot obstacles
symbols(obstacles$x[1], obstacles$y[1], circles = obstacles$r[1], inches = FALSE, add = TRUE, 
        fg = "red", bg = NA, lwd = 2)  # Add a circle at (2, 2) with radius 1
symbols(obstacles$x[2], obstacles$y[2], circles = obstacles$r[2], inches = FALSE, add = TRUE, 
        fg = "red", bg = NA, lwd = 2)  # Add a circle at (2, 2) with radius 1
text(nodes$x, nodes$y, labels = nodes$name, pos = 4, col = "red", cex = 1.2)
optimal.path(graph.object,1,node.count, hops = 2)
d_sd <- calculate_distance(nodes$x[1], nodes$y[1], nodes$x[node.count], nodes$y[node.count])
optimal.path.op(graph.object,1,node.count, hops = 2, d_sd = d_sd)
paths <- all.available.paths(graph.object,1,node.count, hops = 2)
paths
edge.dist.table <- edge.distance(paths[1], nodes, obstacles)

#Some excel experiment
work.book <- createWorkbook()
addWorksheet(work.book, "1-2,2,OPO")


writeData(work.book, sheet = "1-2,2,OPO", edge.data)
saveWorkbook(work.book, "Data_collect.xlsx", overwrite = TRUE)

plot(graph_copy, 
     vertex.color = "orange", 
     vertex.size = 2, 
     edge.width = E(graph.object)$weight, 
     layout = layout_with_kk, 
     main = "Weighted Graph")

graph_copy <- delete_edges(graph.object, E(graph.object)[is.infinite(E(graph.object)$weight)])
plot(graph_copy, 
          vertex.color = "orange", 
           vertex.size = 20, 
           edge.width = 2, 
           layout = layout_with_fr, 
           main = "Graph with Positive Edge Weights")