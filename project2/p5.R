setwd("~/Document/EE232/proj2/src")
rm(list = ls())
gc()
library('igraph')
el = read.csv("smallEdges.txt",header = FALSE)
el = as.matrix(el)
g = graph.edgelist(el[,1:2], directed = FALSE)
E(g)$weight = el[,3]
rm(el)
gc()
movie_community = fastgreedy.community(g)
for(i in 1:length(movie_community)){
  lapply(movie_community[i], write, "smallCommunities.txt", append=TRUE, ncolumns=500000)
}
