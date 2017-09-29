setwd("~/Document/EE232/proj2/src")
rm(list = ls())
gc()
library('igraph')
file = "movieRelation4.csv"
el=read.csv(file, header = FALSE) # read  the file
write.csv(el, file = "movieRelation5")
# el[,1]=as.character(el[,1]) #Because the vertex IDs in this dataset are numbers, we make sure igraph knows these should be treated as characters. Otherwise, it'll create problems (see page on data import)
# el[,2]=as.character(el[,2])
# el[,3] = strtoi(el[,3])
el=as.matrix(el) #igraph needs the edgelist to be in matrix format
g=graph.data.frame(el[,1:2], directed=FALSE) #We first greate a network from the first two columns, which has the list of vertices
#E(g)$weight=as.numeric(el[,3]) #We then add the edge weights to this network by assigning an edge attribute called 'weight'.

mov=read.csv("movieActNumber.csv", header = FALSE)
mov = as.matrix(mov)
# E(g)$weight = as.numeric(el[,3])/(strtoi(mov[el[,1],1]) + strtoi(mov[el[,2],1]) - as.numeric(el[,3]))
el[,3] = as.numeric(el[,3])/(strtoi(mov[el[,1],1]) + strtoi(mov[el[,2],1]) - as.numeric(el[,3]))

# E(g)$weight=edgeWeight
g=simplify(g)
rm(el)
gc()
g_community = cluster_fast_greedy(g)

