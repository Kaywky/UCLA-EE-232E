setwd("~/Document/EE232/proj2/src")
library('igraph')
file = "actGraph.txt"
el=read.csv(file, header = FALSE) # read  the file

# el[,1]=as.character(el[,1]) #Because the vertex IDs in this dataset are numbers, we make sure igraph knows these should be treated as characters. Otherwise, it'll create problems (see page on data import)
# el[,2]=as.character(el[,2])
el[,3] = strtoi(el[,3])
el=as.matrix(el) #igraph needs the edgelist to be in matrix format
g=graph.edgelist(el[,1:2]) #We first greate a network from the first two columns, which has the list of vertices
#E(g)$weight=as.numeric(el[,3]) #We then add the edge weights to this network by assigning an edge attribute called 'weight'.

act=read.csv("actorList.csv", header = FALSE)
act = as.matrix(act)
edgeWeight = as.numeric(el[,3])/(strtoi(act[el[,1],3]))
E(g)$weight=edgeWeight
g=simplify(g)
pr = page.rank(g)
pr_sort = sort(pr$vector,decreasing = TRUE, index.return = TRUE)
print(pr_sort$ix[1:10])
top10 = pr_sort$ix[1:10]
print(pr_sort$x[1:10])
act[top10,]
write.table(act[pr_sort$ix,], "actRank.txt", sep="\t")

