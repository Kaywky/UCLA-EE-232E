rm(list = ls())
library("igraph")
setwd("~/Desktop/Proj1")
g = read.graph("facebook_combined.txt", directed = FALSE)
V(g)$name = V(g)

core_node = numeric(0)
for(i in 1:vcount(g))
{
  if(length(neighbors(g, i))>200)
  {
    core_node = c(core_node, i)
  }
}



# embeddedness_all = numeric(0)
# for (i in 1:length(core_node)) {
#   core = core_node[i]
#   sub_graph = induced_subgraph(g, c(core, neighbors(g, core)))
#   embeddedness = cocitation(sub_graph, 1)
#   embeddedness_all = c(embeddedness_all, embeddedness)
# }

# hist(embeddedness_all, breaks = 30)

dispersion_all = numeric(0)
for (i in 1:length(core_node)) 
{
  core_neighbors =neighbors(g,core_node[i])
  core_network = induced.subgraph(g,c(core_node[i],core_neighbors))
  dispersion = numeric(0)
  
  
  
  cat("i",i,"\n")
  for(k in 1: length(core_neighbors))
  {
    cat("k", k/length(core_neighbors), "\n")
    mutual_friends = intersect(neighbors(g,core_node[i]),neighbors(g,core_neighbors[k]))
    if(length(mutual_friends) < 2) next
    
    dispersion_subgraph = delete.vertices(core_network,c(which(V(core_network)$name==core_node[i]),which(V(core_network)$name==core_neighbors[k])))
    shortestpath = 0
    
    neighborsList = vector("list", length = length(mutual_friends))
    for (t in 1:length(mutual_friends)) {
      neighborsList[[t]] =  neighbors(dispersion_subgraph,which(V(dispersion_subgraph)$name==mutual_friends[t]))
    }
    for(t in 1:(length(mutual_friends)-1)){
      for(m in (t+1):length(mutual_friends)){
        neighborT = neighborsList[[t]]
        neighborM = neighborsList[[m]]
        if(length(intersect(neighborM, neighborT)) == 0 &&  !is.element(mutual_friends[m],neighborT))
          shortestpath = shortestpath + 1
      }
    }
    
    dispersion = c(dispersion, sum(shortestpath))
    
  }
  dispersion_all = c(dispersion_all,dispersion)
}