library("igraph")

g = read.graph("/Users/kay/Documents/course/ee232/project1/facebook_combined.txt", directed = FALSE)
sub_graph = induced_subgraph(g, c(1, neighbors(g, 1)))
vertex_size = rep(4, vcount(sub_graph))
vertex_size[1] = 8
vertex_color = rep("SkyBlue2", vcount(sub_graph))
vertex_color[1] = "red"
plot.igraph(sub_graph, vertex.size = vertex_size, vertex.label = NA,
            vertex.color = vertex_color, edge.arrow.size = 0)
ecount(sub_graph)
#2866
vcount(sub_graph)
#348
