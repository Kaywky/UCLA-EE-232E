library("igraph")
require(graph)
require(eulerian)
library("TSP")

adj_matrix = read.csv("/Users/kay/Documents/course/ee232/hw_4/finance_data/adj_matrix.csv")
adj_matrix$X = NULL
adj_matrix = as.matrix(adj_matrix)
g = graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE, add.colnames = NULL, add.rownames = NA)
mst_g = mst(g, weights = E(g)$weight)

for(v1 in V(g))
{
  for(v2 in V(g))
  {
    for(v3 in V(g))
    {
      if(v1!=v2 && v1!=v3 && v2!=v3)
      {
        if(D[v1, v2] > D[v1, v3] + D[v2, v3])
        {
          print("the triangle inequality does not hold")
          break
        }
        else
          next
      }
    }
    break
  }
  break
}

mst_directed = as.directed(mst_g, 'mutual')
double_mst = as.undirected(mst_directed, 'each' )

el = get.edgelist(double_mst)
v3 = as.vector(E(double_mst)$weight)
dup = cbind(el, v3)

#write the duplicated mst graph to file
write.table(dup, file = "/Users/kay/Documents/course/ee232/hw_4/finance_data/dup_mst.txt",row.names=FALSE, col.names = FALSE)

#Read eulerian result from python 
tour_path = read.table("/Users/kay/Documents/course/ee232/hw_4/finance_data/tsp_name.txt")
tsp_path_name = as.vector(tour_path[,1])

node_name = V(g)$name
tsp_path_index = numeric()
for(i in 1:length(tsp_path_name)){
  match_index = which(tsp_path_name[i] == node_name)
  tsp_path_index = c(tsp_path_index, match_index)
}

tsp_weight = 0
for(i in 2:length(tsp_path_index)){
  tsp_weight = tsp_weight + adj_matrix[tsp_path_index[i],tsp_path_index[i-1]]
}

sum(E(mst_g)$weight)
#417.819
sum(E(double_mst)$weight)
#835.638
tsp_weight
#477.1329

#use a general tsp package
etsp = ETSP(adj_matrix)
tour = solve_TSP(etsp)
tour
#tour length: 768.3892 
plot(etsp, tour)
