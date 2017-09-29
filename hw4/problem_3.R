library("igraph")

adj_matrix = read.csv("/Users/kay/Documents/course/ee232/hw_4/finance_data/adj_matrix.csv")
adj_matrix$X = NULL
adj_matrix = as.matrix(adj_matrix)
g = graph_from_adjacency_matrix(adj_matrix, mode = "upper", weighted = TRUE, add.colnames = NULL, add.rownames = NA)
mst_g = mst(g)

plot(mst_g, main = "MST graph", vertex.label = NA, vertex.size = 3)

data = read.csv("/Users/kay/Documents/course/ee232/hw_4/finance_data/valid_sector.csv")
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
data$col=colors[data$x]

vcount(mst_g)
ecount(mst_g)

plot(mst_g, main = "MST graph", vertex.label = NA, vertex.size = 3,
     vertex.color = data$col)
legend('topleft', cex = 0.75, y.intersp = 0.5, 
       text.width = 0.6, legend = sector, pch = 21, pt.bg = colors)
