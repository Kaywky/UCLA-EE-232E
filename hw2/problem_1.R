library ("igraph")
library ("netrw")

randomWalker = function(g, nodesNum, stepsNum, walkerNum)
{
  avg_dis = numeric()
  std_dev = numeric()
  deg = degree(g)
  for( i in 1:stepsNum)
  {
    short_path = numeric()
    rw = netrw(g, walker.num = walkerNum, damping = 1, T = i, output.walk.path = TRUE)
    rw_tmp = netrw(g, walker.num = walkerNum, damping = 1, T = stepsNum, output.walk.path = TRUE)
    
    for( j in 1:walkerNum)
    {
      short_path_tmp = shortest.paths(g, v = rw$walk.path[1,j], to = rw$walk.path[i,j])
      if(short_path_tmp == Inf)
        short_path_tmp = 0
      short_path = c(short_path, short_path_tmp)
      
      #for 1e
      if(nodesNum == 1000)
      {
        deg = c(deg, deg[rw_tmp$walk.path[stepsNum, j]])
      }
    }
    avg_dis = c(avg_dis, mean(short_path))
    std_dev = c(std_dev, sd(short_path))
  }
  
  plot(avg_dis, main = "average distance", xlab = "step", ylab = "average distance")
  plot(std_dev, main = "standard deviation", xlab = "step", ylab = "standard deviation")
  
  #for 1e
  if(nodesNum == 1000)
  {
    h = hist(deg, breaks = seq(-0.5, by = 1, length.out = max(deg) + 2), freq = F, main = "degree distribution of the nodes reached at the end")
    pl = data.frame(x = h$mids, y=h$density)
    plot(pl, main = "degree distribution of the nodes reached at the end", xlab = "degree", ylab = "density")
  }
  
  return 
}

#1a
p = 0.01
nodesNum = 1000
g1 = random.graph.game(nodesNum, p, directed = FALSE)
d = diameter(g1)
print(d)

#1b
randomWalker(g1, nodesNum, 100, 500)

#1e
plot(degree.distribution(g1), main = "degree distribution of graph", xlab = "degree", ylab = "density")

#1d
nodesNum = 100
g2 = random.graph.game(nodesNum, p, directed = FALSE)
d = diameter(g2)
print(d)
randomWalker(g2, nodesNum, 100, 500)

nodesNum = 10000
g3 = random.graph.game(nodesNum, p, directed = FALSE)
d = diameter(g3)
print(d)
randomWalker(g3, nodesNum, 100, 500)