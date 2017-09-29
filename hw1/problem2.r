library(igraph)
#(a)

g <- barabasi.game(1000, directed=FALSE)
pdf("p2_1.pdf")
plot(degree_distribution(g), main = "Degree Distribution with 1000 nodes ", xlab="degree", ylab="density")
dev.off()
Diameter=Connectivity=numeric(0)
for (i in 1:100){
  g = barabasi.game(1000, directed=FALSE)
  Diameter = c(Diameter, diameter(g))
  Connectivity = c(Connectivity, is.connected(g))
}
Dia_avg = mean(Diameter)
Con_avg = mean(Connectivity)
print(paste("average diameter: ", Dia_avg))
print(paste("Connectivity: ", Con_avg))

#(b)
clu <- clusters(g)
gcc <- which.max(clu$csize)
nonGcc <- (1:vcount(g))[clu$membership != gcc]
structure <- cluster_fast_greedy(delete.vertices(g,nonGcc))
pdf("p2_2_.pdf")
#plot(structure, g, col = membership(structure),mark.groups = communities(structure), edge.color = c("black", "red")[crossing(structure,g) + 1],vertex.label=NA)
plot(structure, g, layout=layout.fruchterman.reingold(g, niter=10000), col = membership(structure),mark.groups = communities(structure), edge.color = c("black", "red")[crossing(structure,g) + 1],vertex.label=NA, vertex.size=5)
dev.off()
print(paste("length: ", length(structure)))
print(paste("modularity: ", modularity(structure)))

#(c)
g <- barabasi.game(10000, directed=FALSE)
pdf("p2_3.pdf")
plot(degree_distribution(g), main = "Degree Distribution with 10000 nodes", xlab="degree", ylab="density")
dev.off()
print(paste("diameter: ", diameter(g)))
clu <- clusters(g)
gcc <- which.max(clu$csize)
nonGcc <- (1:vcount(g))[clu$membership != gcc]
structure <- cluster_fast_greedy(delete.vertices(g,nonGcc))
pdf("p2_4_.pdf")
plot(structure, g, layout=layout.fruchterman.reingold(g, niter=10000), col = membership(structure),mark.groups = communities(structure), edge.color = c("black", "red")[crossing(structure,g) + 1],vertex.label=NA, vertex.size=5)
#plot(structure, g, col = membership(structure),mark.groups = communities(structure), edge.color = c("black", "red")[crossing(structure,g) + 1],vertex.label=NA)
dev.off()
print(paste("length: ", length(structure)))
print(paste("modularity: ", modularity(structure)))

#(d)
d <- numeric(0)
for(i in 1:1000){
	rand <- sample(1000, 1)
	nb <- sample(neighbors(g, rand),1)
	d = c(d, degree(g, nb))
}
pdf("p2_5.pdf")
plot(density(d), main = "Degree Distribution of nodes j", xlab="degree", ylab="density")
dev.off()


