rm(list = ls())
library("igraph")
setwd("~/Desktop/Proj1")
g = read.graph("facebook_combined.txt", directed = FALSE)
V(g)$name = V(g)

flag = 0
core_node = numeric(0)
for(i in 1:vcount(g))
{
  if(length(neighbors(g, i))>200)
  {
    core_node = c(core_node, i)
    flag = flag+1
    if(flag == 3) 
      break
  }
}

for (i in 1:length(core_node)) {
  # choose the core node with id=1, which has more than 200 neighbors.
  core_neighbors =neighbors(g,core_node[i])
  core_network = induced.subgraph(g,c(core_node[i],core_neighbors))
  
  
  
  dispersion = numeric(0)
  for(k in 1: length(core_neighbors))
  {
    cat("k", k/length(core_neighbors), "\n")
    mutual_friends = intersect(neighbors(g,core_node[i]),neighbors(g,core_neighbors[k]))
    if(length(mutual_friends) < 2) {
      dispersion = c(dispersion, 0)
      next
    }
    
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
  
  embeddedness = cocitation(core_network, 1)
  newEmb = numeric(0)
  for (t in 2:length(embeddedness)) {
    newEmb = c(newEmb,embeddedness[[t]])
  }
  embeddedness = newEmb
  
  ed = dispersion/embeddedness
  ed[mapply(is.nan,ed)] = 0
  
  maxDisNode = which(V(core_network)$name==core_neighbors[which(dispersion == max(dispersion))])
  maxEmbNode = which(V(core_network)$name==core_neighbors[which(embeddedness == max(embeddedness))])
  maxEdNode = which(V(core_network)$name==core_neighbors[which(ed == max(ed))])
  
  
  # construct community structure
  personal_community = fastgreedy.community(core_network)
  sizes(personal_community)
  
  # set properties of the plotted network
  node_color = personal_community$membership+1
  node_size = rep(3,length(node_color))
  edge_color = rep("grey", length(E(core_network)))
  edge_weight = rep(0.5, length(E(core_network)))
  
  i_node = i
  d_node = maxDisNode
  e_node = maxEmbNode
  ed_node = maxEdNode
  
  edge_color[which(get.edgelist(core_network, name = FALSE)[,1] == d_node | 
                     get.edgelist(core_network, name = FALSE)[,2] == d_node)] = "red";
  edge_weight[which(get.edgelist(core_network, name = FALSE)[,1] == d_node |  
                      get.edgelist(core_network, name = FALSE)[,2] == d_node)] = 3;
  node_size[d_node] = 5
  node_color[d_node] = 7
  node_size[i_node] = 4
  node_color[i_node] = 0
  plot.igraph(core_network, vertex.size = node_size , vertex.label = NA , edge.width = edge_weight, edge.color =  edge_color, vertex.color = node_color, asp=9/16, layout = layout.fruchterman.reingold)
  
  node_color = personal_community$membership+1
  node_size = rep(3,length(node_color))
  edge_color = rep("grey", length(E(core_network)))
  edge_weight = rep(0.5, length(E(core_network)))
  edge_color[which(get.edgelist(core_network, name = FALSE)[,1] == e_node | 
                     get.edgelist(core_network, name = FALSE)[,2] == e_node)] = "red";
  edge_weight[which(get.edgelist(core_network, name = FALSE)[,1] == e_node |  
                      get.edgelist(core_network, name = FALSE)[,2] == e_node)] = 3;
  node_size[e_node] = 5
  node_color[e_node] = 7
  node_size[i_node] = 4
  node_color[i_node] = 0
  plot.igraph(core_network, vertex.size = node_size , vertex.label = NA , edge.width = edge_weight, edge.color =  edge_color, vertex.color = node_color, asp=9/16, layout = layout.fruchterman.reingold)
  
  
  node_color = personal_community$membership+1
  node_size = rep(3,length(node_color))
  edge_color = rep("grey", length(E(core_network)))
  edge_weight = rep(0.5, length(E(core_network)))
  edge_color[which(get.edgelist(core_network, name = FALSE)[,1] == ed_node | 
                     get.edgelist(core_network, name = FALSE)[,2] == ed_node)] = "red";
  edge_weight[which(get.edgelist(core_network, name = FALSE)[,1] == ed_node |  
                      get.edgelist(core_network, name = FALSE)[,2] == ed_node)] = 3;
  node_size[ed_node] = 5
  node_color[ed_node] = 7
  node_size[i_node] = 4
  node_color[i_node] = 0
  plot.igraph(core_network, vertex.size = node_size , vertex.label = NA , edge.width = edge_weight, edge.color =  edge_color, vertex.color = node_color, asp=9/16, layout = layout.fruchterman.reingold)
  
}


