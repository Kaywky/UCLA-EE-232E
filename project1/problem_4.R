library("igraph")

g = read.graph("/Users/kay/Documents/course/ee232/project1/facebook_combined.txt", directed = FALSE)

# choose the core node with id=1, which has more than 200 neighbors.
sub_graph = induced_subgraph(g, neighbors(g, 1))
fg_com = fastgreedy.community(sub_graph)
eb_com = edge.betweenness.community(sub_graph)
im_com = infomap.community(sub_graph)
vertex_size = rep(4, vcount(sub_graph))
vertex_size[1] = 8
plot(sub_graph, vertex.size = vertex_size,
     vertex.label = NA, edge.arrow.size = 0, main = "subgraph")
plot(fg_com, sub_graph, vertex.size = vertex_size,
     vertex.label = NA, edge.arrow.size = 0, main = "Fast-Greedy Community Structure")
plot(eb_com, sub_graph, vertex.size = vertex_size,
     vertex.label = NA, edge.arrow.size = 0, main = "Edge-Betweenness Community Structure")
plot(im_com, sub_graph, vertex.size = vertex_size,
     vertex.label = NA, edge.arrow.size = 0, main = "Infomap Community Structure")

modularity(fg_com)
# 0.4418533
modularity(eb_com)
# 0.4161461
modularity(im_com)
# 0.4180077

hist(fg_com$membership, xlab = "membership", ylab = "frequency",
     main = "Histogram of community membership using Fast-Greedy",
     col = "red")
hist(eb_com$membership, xlab = "membership", ylab = "frequency",
     main = "Histogram of community membership using Edge-Betweenness",
     col = "green")
hist(im_com$membership, xlab = "membership", ylab = "frequency",
     main = "Histogram of community membership using Infomap",
     col = "blue")