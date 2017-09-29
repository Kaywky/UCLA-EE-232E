correlation_cal = function(stock_name1, stock_name2, data, data1)
{
  path2 = paste("/Users/kay/Documents/course/ee232/hw_4/finance_data/data/", stock_name2, ".csv", sep = "")
  data2 = read.csv(path2)
  if(nrow(data2)<765)
    return()
  price1 = data1$Close
  price2 = data2$Close
  log1 = matrix(nrow = nrow(data1)-1, ncol = 1)
  log2 = matrix(nrow = nrow(data2)-1, ncol = 1)
  log1[, 1] = diff(log(price1))
  log2[, 1] = diff(log(price2))
  result = cor(log1, log2)
  mydata[stock_name1, stock_name2] <<- result
  mydata[stock_name2, stock_name1] <<- result
  gc()
}

data = read.csv("/Users/kay/Documents/course/ee232/hw_4/finance_data/Name_sector.csv")
stock_name = data$Symbol
sector_name = c()
valid_stock_name = c()
stock_num = nrow(data)
mydata = matrix(0, stock_num, stock_num)

for(m in 1:stock_num)
{
  stock_name1 = stock_name[m]
  path1 = paste("/Users/kay/Documents/course/ee232/hw_4/finance_data/data/", stock_name1, ".csv", sep = "")
  data1 = read.csv(path1)
  if(nrow(data1)<765)
    next
  this_sector = data$Sector[which(data$Symbol==stock_name1)]
  sector_name = c(sector_name, toString(this_sector))
  valid_stock_name = c(valid_stock_name, toString(stock_name1))
  for(n in (m+1):stock_num)
  {
    stock_name2 = stock_name[n]
    correlation_cal(stock_name1, stock_name2, data, data1)
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

write.csv(sector_name, "/Users/kay/Documents/course/ee232/hw_4/finance_data/valid_sector.csv")
write.csv(mydata, "/Users/kay/Documents/course/ee232/hw_4/finance_data/p1_correlations.csv")