library("igraph")

data = read.csv("/Users/kay/Documents/course/ee232/hw_4/finance_data/p1_correlations.csv")
stock_num = 494
data$X = NULL

data = sqrt(2*(1-data))
diag(data) = 0
my_hist = numeric()
for(m in 1:(stock_num-1))
{
  for(n in m:stock_num)
  {
    if(m!=n)
    {
      my_hist = c(my_hist, data[m, n])
    }

  }
}

col_name = colnames(data)
rownames(data) = col_name
mydata = matrix(data)
save(mydata, file = "distance.Rdata")
data = as.matrix(data)

write.csv(data, "/Users/kay/Documents/course/ee232/hw_4/finance_data/adj_matrix.csv")
hist(my_hist, xlab = "the length of the link Dij", ylab = "frequency", col = "SkyBlue2")
g = graph_from_adjacency_matrix(data, mode = "upper", weighted = TRUE, add.colnames = NULL, add.rownames = NA)

