library("igraph")
library("netrw")

#using probability method to calculate PageRank
pagerank_prob = function(nodesNum, stepsNum, walkerNum, num_damp)
{
  g = random.graph.game(nodesNum, 0.01, directed = TRUE)
  pageRank_1 = matrix(0, 1, nodesNum)
  pageRank_2 = matrix(0, 1, nodesNum)
  
  #4a
  for(i in 1:nodesNum)
  {
    rw = netrw(g, walker.num = walkerNum, damping = num_damp,
               start.node = i, T = stepsNum, output.walk.path = TRUE,
               output.visit.prob = TRUE)
    pageRank_1 = pageRank_1 + rw$ave.visit.prob
  }
  pageRank_1 = pageRank_1/nodesNum
  plot(pageRank_1[1, ], main = "simulated PageRank", xlab = "node", ylab = "PageRank")
  
  #4b
  for(i in 1:nodesNum)
  {
    rw = netrw(g, walker.num = walkerNum, damping = num_damp,
               start.node = i, T = stepsNum, output.walk.path = TRUE,
               output.visit.prob = TRUE, teleport.prob = pageRank_1)
    pageRank_2 = pageRank_2 + rw$ave.visit.prob
  }
  pageRank_2 = pageRank_2/nodesNum
  plot(pageRank_2[1, ], main = "personalized PageRank", xlab = "node", ylab = "PageRank")
  
  plot(pageRank_1[1, ], pageRank_2[1, ], main = "PageRank 1 vs PageRank 2",
       xlab = "PageRank 1", ylab = "PageRank 2")

  return
}

#using default methods in igraph to calculate PageRank
pagerank_def = function(nodesNum, stepsNum, walkerNum, num_damp)
{
  #4a
  g = random.graph.game(nodesNum, 0.01, directed = TRUE)
  pageRank_1 = page.rank(g, damping = num_damp)
  plot(pageRank_1$vector, main = "simulated PageRank", xlab = "node", ylab = "PageRank")
  
  #4b
  pageRank_2 = personalized.pagerank(g, damping = num_damp, prob = pageRank_1$vector)
  plot(pageRank_2, main = "personalized PageRank", xlab = "node", ylab = "PageRank")
  
  plot(pageRank_1$vector, pageRank_2, main = "PageRank 1 vs PageRank 2",
       xlab = "PageRank 1", ylab = "PageRank 2")
  
  return
}

#pagerank_prob(1000, 100, 500, 0.85)
pagerank_def(1000, 100, 500, 0.85)