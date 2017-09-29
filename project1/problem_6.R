library('igraph')
library('netrw')
#read file
# May be changed according to the directory of the file
fileDir = "C:\\Users\\SoniaYu\\Desktop\\facebook_combined.txt"

# Generate the graph from the file
g = read.graph(file = fileDir , format = "ncol", directed = T)
deg = degree(g)
core_degree = numeric(0)
core_index = numeric(0)

#find all the vertices with more than 200 neighbors
for(i in 1:length(deg))
{

  if(length(neighbors(g,i)) > 200)
  {
    core_degree = c(core_degree, length(neighbors(g,i)))
    core_index = c(core_index, i)
  }
}
#print(core_degree)
#print(core_index)

#features: 
average_degree_max = numeric(0)
average_degree_max_index = numeric(0)
clustering_coefficient_max = numeric(0)
clustering_coefficient_index_max = numeric(0)
density_max = numeric(0)
density_index_max = numeric(0)
community_max = numeric(0)
community_max_index = numeric(0)

average_degree_min = numeric(0)
average_degree_min_index = numeric(0)
clustering_coefficient_min = numeric(0)
clustering_coefficient_index_min = numeric(0)
density_min = numeric(0)
density_index_min = numeric(0)
community_min = numeric(0)
community_min_index = numeric(0)


#find all the communities with 10 or more members in the core's personal network
for(i in 1: length(core_index))
{
  vertice_i = core_index[i]
  coreneighbors_i = neighbors(g, vertice_i )
  coresubgraph_i = induced.subgraph(g, c(vertice_i, neighbors(g, vertice_i)))
  corecommunity_i = walktrap.community(coresubgraph_i)
  # 54 communities after calculating with the walktrap algorithm
  community_num_10up = numeric(0)
  for(j in 1: length(corecommunity_i))
  {
    #find every community with 10 or more nodes
    if(length(V(coresubgraph_i)[which(corecommunity_i$membership==j)]) > 10)
    {
      community_num_10up = c(community_num_10up, j)
    }
  }
  average_degree = numeric(0)
  global_clustering_coefficient = numeric(0)
  density = numeric(0)
  community_size = numeric(0)
  for(k in 1: length(community_num_10up))
  {
    #get the graph of the communities wiith 10 or more nodes
    community_graph = induced.subgraph(coresubgraph_i, V(coresubgraph_i)[which(corecommunity_i$membership == community_num_10up[k])])
    average_degree = c(average_degree, mean(degree(community_graph))/vcount(community_graph))
    global_clustering_coefficient = c(global_clustering_coefficient, transitivity(community_graph,type = "global"))
    density = c(density, graph.density(community_graph))
    community_size = c(community_size, vcount(community_graph)) 
  }
   
  #max values of features
  average_degree_max = c(average_degree_max,max(average_degree))
  clustering_coefficient_max = c(clustering_coefficient_max,max(global_clustering_coefficient))
  density_max = c(density_max,max(density))
  community_max = c(community_max, max(community_size))
  
  #min values of features
  average_degree_min = c(average_degree_min,min(average_degree))
  clustering_coefficient_min = c(clustering_coefficient_min,min(global_clustering_coefficient))
  density_min = c(density_min,min(density))
  community_min = c(community_min, min(community_size))
  
  #index of the max feature value
  average_degree_max_index = c(average_degree_max_index, community_num_10up[which.max(average_degree)])
  clustering_coefficient_index_max = c(clustering_coefficient_index_max, community_num_10up[which.max(global_clustering_coefficient)])
  density_index_max = c(density_index_max, community_num_10up[which.max(density)])
  community_max_index = c(community_max_index, community_num_10up[which.max(community_size)])
  
  #index of the min feature value
  average_degree_min_index = c(average_degree_min_index, community_num_10up[which.min(average_degree)])
  clustering_coefficient_index_min = c(clustering_coefficient_index_min, community_num_10up[which.min(global_clustering_coefficient)])
  density_index_min = c(density_index_min, community_num_10up[which.min(density)])
  community_min_index = c(community_min_index, community_num_10up[which.min(community_size)])
  

  
  cat("core_index: ",i,"\n")
  cat("[max] ")
  cat("average degree index: ",community_num_10up[which.max(average_degree)]," ")
  cat("cluster coefficient index: ",community_num_10up[which.max(global_clustering_coefficient)]," ")
  cat("density index: ",community_num_10up[which.max(density)]," ")
  cat("community size index: ",community_num_10up[which.max(community_size)],"\n")
  cat("[max] ")
  cat("average degree: ",max(average_degree)," ")
  cat("cluster coefficient: ", max(global_clustering_coefficient)," ")
  cat("density index: ",max(density)," ")
  cat("community size: ",max(community_size),"\n")
  cat("[min] ")
  cat("average degree index: ",community_num_10up[which.min(average_degree)]," ")
  cat("cluster coefficient index: ",community_num_10up[which.min(global_clustering_coefficient)]," ")
  cat("density index: ",community_num_10up[which.min(density)]," ")
  cat("community size index: ",community_num_10up[which.min(community_size)],"\n")
  cat("[min] ")
  cat("average degree: ",min(average_degree)," ")
  cat("cluster coefficient: ", min(global_clustering_coefficient)," ")
  cat("density index: ",min(density)," ")
  cat("community size: ",min(community_size),"\n")
  
}



