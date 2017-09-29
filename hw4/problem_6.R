library("igraph")

correlation_cal = function(stock_name1, stock_name2, index, data1)
{
  path2 = paste("/Users/kay/Documents/course/ee232/hw_4/finance_data/data/", stock_name2, ".csv", sep = "")
  data2 = read.csv(path2)
  if(nrow(data2)<765)
    return()
  price1 = data1$Close
  price2 = data2$Close
  date1 = data1$Date
  log1 = numeric()
  log2 = numeric()
  for(i in index)
  {
    log1 = c(log1, price1[i])
    log2 = c(log2, price2[i])
  }
  log1 = diff(log(log1))
  log2 = diff(log(log2))
  result = cor(log1, log2)
  mydata[stock_name1, stock_name2] <<- result
  mydata[stock_name2, stock_name1] <<- result
  gc()
}

data = read.csv("/Users/kay/Documents/course/ee232/hw_4/finance_data/Name_sector.csv")
stock_name = data$Symbol
valid_stock_name = c()
stock_num = nrow(data)
mydata = matrix(0, stock_num, stock_num)
index = numeric()

for(m in 1:stock_num)
{
  stock_name1 = stock_name[m]
  path1 = paste("/Users/kay/Documents/course/ee232/hw_4/finance_data/data/", stock_name1, ".csv", sep = "")
  data1 = read.csv(path1)
  if(m==1)
  {
    for(l in 1:nrow(data1))
    {
      if(wday(as.Date(toString(data1$Date[l])))==2)
      {
        index = c(index, l)
      }
    }
  }
  if(nrow(data1)<765)
    next
  valid_stock_name = c(valid_stock_name, toString(stock_name1))
  for(n in (m+1):stock_num)
  {
    stock_name2 = stock_name[n]
    correlation_cal(stock_name1, stock_name2, index, data1)
  }
  gc()
}

for(m in 1:nrow(mydata))
{
  if(is.na(mydata[m, 1]))
    break
  if(mydata[m, 1]==0 && mydata[m, 2]==0)
  {
    mydata = mydata[-m, ]
    mydata = mydata[, -m]
  }
}

rownames(mydata) = valid_stock_name
colnames(mydata) = valid_stock_name

adj_data = sqrt(2*(1-mydata))
diag(adj_data) = 0
my_hist = numeric()
for(m in 1:(nrow(mydata)-1))
{
  for(n in m:nrow(mydata))
  {
    if(m!=n)
    {
      my_hist = c(my_hist, adj_data[m, n])
    }
  }
}

write.csv(mydata, "/Users/kay/Documents/course/ee232/hw_4/finance_data/p6_correlations.csv")
write.csv(adj_data, "/Users/kay/Documents/course/ee232/hw_4/finance_data/p6_adj_matrix.csv")
hist(my_hist, xlab = "the length of the link Dij", ylab = "frequency", col = "SkyBlue2")
g = graph_from_adjacency_matrix(adj_data, mode = "upper", weighted = TRUE, add.colnames = NULL, add.rownames = NA)

mst_g = mst(g)
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

plot(mst_g, main = "MST graph", vertex.label = NA, vertex.size = 3,
     vertex.color = data$col)
legend('topleft', cex = 0.75, y.intersp = 0.5, 
       text.width = 0.6, legend = sector, pch = 21, pt.bg = colors)
