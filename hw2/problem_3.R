library("igraph")
library("netrw")

randomWalker = function(nodesNum, if_directed, stepsNum, walkerNum, num_damp)
{
  g = random.graph.game(nodesNum, 0.01, directed = if_directed)
  avg_prob = matrix(0, 1, nodesNum)
  for(i in 1:nodesNum)
  {
    rw = netrw(g, walker.num = walkerNum, damping = num_damp,
               start.node = i, T = stepsNum, output.walk.path = TRUE,
               output.visit.prob = TRUE)
    avg_prob = avg_prob + rw$ave.visit.prob
  }
  
  avg_prob = avg_prob/nodesNum
  rw = netrw(g, walker.num = walkerNum, damping = num_damp,
             T = stepsNum, output.walk.path = TRUE,
             output.visit.prob = TRUE)
  prob = rw$ave.visit.prob
  deg = degree(g)
  if(if_directed)
  {
    correlation = cor(deg, prob)
    print(correlation)
    deg_in = degree(g, mode = "in")
    deg_out = degree(g, mode = "out")
    correlation_in = cor(deg_in, prob)
    correlation_out = cor(deg_out, prob)
    print(correlation_in)
    print(correlation_out)
  }
  else
  {
    correlation = cor(deg, prob)
    print(correlation)
  }
  
  plot(deg, colMeans(avg_prob), main = "probability with degree", xlab = "degree", ylab = "probability")
  return
}

#3a
randomWalker(1000, FALSE, 100, 100, 1)

#3b
randomWalker(1000, TRUE, 100, 100, 1)

#3c
randomWalker(1000, FALSE, 100, 100, 0.85)

