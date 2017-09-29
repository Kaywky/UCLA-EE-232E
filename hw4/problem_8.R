library("igraph")

data = read.csv("/Users/SoniaYu/Documents/17Spring_GraphsandNetworkFlows/Homework4/finance_data/p1_correlaitons.csv")
stock_num = 494
data$X = NULL

old_hist = numeric()
new_hist = numeric()
for(m in 1:stock_num)
{
  for(n in 1:stock_num)
  {
    if(m<n)
      old_hist = c(old_hist, data[m, n])
    if(m!=n && data[m, n]>0.3)
    {
      data[m, n] = -1
    }
    if(m<n)
      new_hist = c(new_hist, data[m, n])
  }
}

hist(old_hist, xlab = "Correlation Rij", ylab = "frequency", col = "SkyBlue2")
hist(new_hist, xlab = "Correlation Rij", ylab = "frequency", col = "SkyBlue2")
adj_data = sqrt(2*(1-data))
adj_data = as.matrix(adj_data)
diag(adj_data) = 0
g = graph_from_adjacency_matrix(adj_data, mode = "upper", weighted = TRUE, add.colnames = NULL, add.rownames = NA)
mst_g = mst(g)
plot(mst_g, main = "Vine Cluster", vertex.label = NA, vertex.size = 1)

shortdis = distances(mst_g, v = V(mst_g), to = V(mst_g), mode = "all", algorithm = "dijkstra")
idx_max_shortdis = which.max(shortdis)
for (i in 1:494) {
  for (j in 1:494) {
    if (shortdis[i, j] == shortdis[idx_max_shortdis]) {
      print(i)
      print(V(mst_g)[i])
      print(j)
      print(V(mst_g)[j])
    }
  }
}
# 328 NI 342 O

