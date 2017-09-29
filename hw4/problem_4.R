library("igraph")

adj_matrix = read.csv("/Users/kay/Documents/course/ee232/hw_4/finance_data/adj_matrix.csv")
adj_matrix$X = NULL
adj_matrix = as.matrix(adj_matrix)
g = graph_from_adjacency_matrix(adj_matrix, mode = "upper", weighted = TRUE, add.colnames = NULL, add.rownames = NA)
mst_g = mst(g)
sec_valid = read.csv("/Users/kay/Documents/course/ee232/hw_4/finance_data/valid_sector.csv")

sec_name = c()
for(i in 1:nrow(sec_valid))
{
  sec_name = c(sec_name, toString(sec_valid$x[i]))
}
mst_g = set_vertex_attr(mst_g, name = "sector", value = sec_name)

eval_method = 0
eval_random = 0
for(i in 1:vcount(mst_g))
{
  v_neighbor = neighbors(mst_g, i, mode = "all")
  this_sec = sec_name[i]
  count = 0
  for(j in v_neighbor)
  {
    if(vertex_attr(mst_g, "sector", index = j)==this_sec)
      count = count + 1
  }
  eval_method = eval_method + (count/length(v_neighbor))
}

eval_method = eval_method/494

sector_list=list("Consumer Discretionary",
                 "Consumer Staples",
                 "Energy",
                 "Financials",
                 "Health Care",
                 "Industrials",
                 "Information Technology",
                 "Materials",
                 "Real Estate",
                 "Telecommunication Services",
                 "Utilities")
sum_list = numeric()
for(i in 1:length(sector_list))
{
  sum_list[i] = length(which(sec_name == sector_list[i]))
  #eval_random = eval_random + sum_list[i]*sum_list[i]/494
}

for(i in 1:vcount(mst_g))
{
  this_sec = sec_name[i]
  this_index = match(this_sec, sector_list)
  eval_random = eval_random + sum_list[this_index]/494
}
eval_random = eval_random/494

print(eval_method)
#0.8289301
print(eval_random)
#0.1141881

