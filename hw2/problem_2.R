library("igraph")
library("netrw")

randomWalker = function(g, stepsNum, walkerNum)
{
  avg_dis = numeric()
  std_dev = numeric()
  for( i in 1:stepsNum)
  {
    short_path = numeric()
    rw = netrw(g, walker.num = walkerNum, damping = 1, T = i, output.walk.path = TRUE)
    for( j in 1:walkerNum)
    {
      short_path_tmp = shortest.paths(g, v = rw$walk.path[1,j], to = rw$walk.path[i,j])
      if(short_path_tmp == Inf)
        short_path_tmp = 0
      short_path = c(short_path, short_path_tmp)
    }
    avg_dis = c(avg_dis, mean(short_path))
    std_dev = c(std_dev, sd(short_path))
  }
  
  plot(avg_dis, main = "average distance", xlab = "step", ylab = "average distance")
  plot(std_dev, main = "standard deviation", xlab = "step", ylab = "standard deviation")
}

randomWalker_toEnd = function(g, stepsNum, walkerNum)
{
  deg = degree(g)
  for(i in 1:stepsNum)
  {
    rw = netrw(g, walker.num = walkerNum, damping = 1, T = stepsNum, output.walk.path = TRUE)
    for(j in 1:walkerNum)
    {
      deg = c(deg, deg[rw$walk.path[stepsNum, j]])
    }
  }
  
  h = hist(deg, breaks = seq(-0.5, by = 1, length.out = max(deg) + 2), freq = F, main = "degree distribution of the nodes reached at the end")
  pl = data.frame(x = h$mids, y=h$density)
  plot(pl, main = "degree distribution of the nodes reached at the end", xlab = "degree", ylab = "density")
}

nodesNum = 1000
#2a
g1 = barabasi.game(nodesNum, directed = FALSE)
d = diameter(g1)
print(d)

#2b
randomWalker(g1, 100, 500)

#2d
nodesNum = 100
g2 = barabasi.game(nodesNum, directed = FALSE)
d = diameter(g2)
print(d)
randomWalker(g2, 100, 500)

nodesNum = 10000
g3 = barabasi.game(nodesNum, directed = FALSE)
d = diameter(g3)
print(d)
randomWalker(g3, 100, 500)

#2e
randomWalker_toEnd(g1, 100, 500)
plot(degree.distribution(g1), main = "degree distribution of graph", xlab = "degree", ylab = "density")

