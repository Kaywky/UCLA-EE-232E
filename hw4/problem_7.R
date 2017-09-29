library("igraph")

data = read.csv("/Users/kay/Documents/course/ee232/hw_4/finance_data/p1_correlations.csv")
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

sec_data = read.csv("/Users/kay/Documents/course/ee232/hw_4/finance_data/valid_sector.csv")
sector=list("Consumer Discretionary",
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
colors = rainbow(11, s=0.8, v=1)
names(colors)=sector
sec_data$col=colors[sec_data$x]

plot(mst_g, main = "MST graph", vertex.label = NA, vertex.size = 3,
     vertex.color = sec_data$col)
legend('topleft', cex = 0.75, y.intersp = 0.5, 
       text.width = 0.6, legend = sector, pch = 21, pt.bg = colors)
