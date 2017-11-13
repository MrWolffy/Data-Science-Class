#step 1
library(igraph)
g = make_empty_graph(n = 5, directed = T)
plot(g)
V(g)$color = 'yellow'
V(g)$shape = 'sphere'
plot(g)
#these lines of code create an empty directed graph (an graph without any edge) with 5 nodes, 
#and change the color and shape of the nodes when plotting the graph.

#step 2
g = add_edges(g, c(1,2, 1,3, 2,4, 3,4, 4,5))
plot(g)
#these lines of code add 5 directed edges to the graph.

#step 3
g = add_vertices(g, 1, color = 'red', shape = 'sphere')
plot(g)
g = add_edges(g, c(3,6, 6,5))
plot(g)
#these lines of code add a new node (numbered 6) to the graph, 
#and add two edges connected to the new node.

#step 4
E(g)
g = delete_edges(g, 2)
plot(g)
g = add_edges(g, c(3, 1))
plot(g)
#these lines of code show the changes when deleting an edge from the graph.

#step 5
V(g)$name = LETTERS[1:6]
V(g)
E(g)
#these lines of code rename the nodes from A to F, that is, using the big letters.

#step 6
shortest_paths(g, "A", "E", output="epath")$epath[1]
#this line of code find the shortest path from node A to node E.

#step 7
plot(g, layout = layout_nicely, vertex.size = degree(g, V(g), "in")*15+15,
     vertex.label.dist = 0.5, edge.arrow.size = 0.5)
#this line of code plot the graph with the size of nodes measured by degree.

#step 8
plot(degree_distribution(g), main = "Degree distribution", 
     xlab = "Degree", ylab = "Frequency")
#this line of code draw the plot of the distribution of degree of nodes, 
#which is equal to the numbers of edge connected to that node.

#step 9
pal = colorRampPalette(c('lightblue', 'blue'))
a = as.matrix(get.adjacency(g))
a
heatmap(a, Rowv = NA, Colv = 'Rowv', col = pal(100))
#these lines of code draw a map to show the edges of the graph.

#step 10
sg = induced_subgraph(g, V(g)[degree(g, V(g), "in") >= 1])
plot(sg, layout = layout_nicely, vertex.size = degree(sg, V(sg), "out")*10+15,
     vertex.color = "green", vertex.shape = "square", vertex.label.dist = 0.5, 
     edge.arrow.size = 0.5)
#these lines of code draw a plot of a subgraph which deletes node(s)
#with less or equal to 1 degree, in graph g, node C. 



