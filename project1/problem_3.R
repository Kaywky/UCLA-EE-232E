library("igraph")

g = read.graph("/Users/kay/Documents/course/ee232/project1/facebook_combined.txt", directed = FALSE)

core_node = numeric(0)
core_degree = numeric(0)
for(i in 1:vcount(g))
{
  if(length(neighbors(g, i))>200)
  {
    core_node = c(core_node, i)
    core_degree = c(core_degree, degree(g, i))
  }
}
length(core_node)
# 40
mean(core_degree)
# 279.375

# choose the core node with id=1, which has more than 200 neighbors.
sub_graph = induced_subgraph(g, c(1, neighbors(g, 1)))
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
# 0.4131014
modularity(eb_com)
# 0.3533022
modularity(im_com)
# 0.3891185

hist(fg_com$membership, xlab = "membership", ylab = "frequency",
     main = "Histogram of community membership using Fast-Greedy",
     col = "red")
hist(eb_com$membership, xlab = "membership", ylab = "frequency",
     main = "Histogram of community membership using Edge-Betweenness",
     col = "green")
hist(im_com$membership, xlab = "membership", ylab = "frequency",
     main = "Histogram of community membership using Infomap",
     col = "blue")
