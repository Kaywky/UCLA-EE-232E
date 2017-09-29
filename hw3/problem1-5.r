library("igraph")
g = read.graph("/Users/SoniaYu/Documents/17Spring_GraphsandNetworkFlows/Homework3/sorted_directed_net.txt", format = "ncol", directed = TRUE)

#(1)
Connectivity = is.connected(g)
print(paste("Connectivity:", Connectivity))
cl = clusters(g)
gcc_idx = which.max(cl$csize)
non_gcc_idx = (1:vcount(g))[cl$membership != gcc_idx]
gcc = delete.vertices(g, non_gcc_idx)

# #(2)
# dd1 = degree.distribution(gcc, mode="in")
# dd2 = degree.distribution(gcc, mode="out")
# pdf("p2_1.pdf")
# plot(seq(along=dd1)-1, dd1, main = "In-Degree Distribution of GCC", xlab="in degree", ylab="density")
# dev.off()
# pdf("p2_2.pdf")
# plot(seq(along=dd2)-1, dd2, main = "Out-Degree Distribution of GCC", xlab="out degree", ylab="density")
# dev.off()
# pdf("p2_3.pdf")
# hist(degree(gcc, mode="in"), breaks=100, main ="In-Degree Distribution of GCC", xlab="in degree", ylab="density", col="blue")
# dev.off()
# pdf("p2_4.pdf")
# hist(degree(gcc, mode="out"), breaks=100, main ="Out-Degree Distribution of GCC", xlab="out degree", ylab="density", col="blue")
# dev.off()

#(3)
#option 1
gcc_not_simple = as.undirected(gcc, mode = "each")
community_not_simple = label.propagation.community(gcc_not_simple, weights = E(gcc_not_simple)$weight)
print("option 1")
print(paste("sizes: ", sizes(community_not_simple)))
print(paste("modularity: ", modularity(community_not_simple)))
tiff("p3_1.tiff", width = 3200, height = 3200, units = "px", res = 800)
plot(community_not_simple, gcc_not_simple, layout=layout.fruchterman.reingold(gcc_not_simple, niter=10000), col = membership(community_not_simple),mark.groups = communities(community_not_simple), edge.color = c("black", "red")[crossing(community_not_simple,gcc_not_simple) + 1],vertex.label=NA, vertex.size=3)
dev.off()
#option 2
gcc_simple = as.undirected(gcc, mode = "collapse", edge.attr.comb = function(weight) sqrt(prod(weight)))
community_simple_fastgreedy = fastgreedy.community(gcc_simple, weights = E(gcc_simple)$weight)
print("option 2, fast greedy")
print(paste("sizes: ", sizes(community_simple_fastgreedy)))
print(paste("modularity: ", modularity(community_simple_fastgreedy)))
tiff("p3_2.tiff", width = 3200, height = 3200, units = "px", res = 800)
plot(community_simple_fastgreedy, gcc_simple, layout=layout.fruchterman.reingold(gcc_simple, niter=10000), col = membership(community_simple_fastgreedy),mark.groups = communities(community_simple_fastgreedy), edge.color = c("black", "red")[crossing(community_simple_fastgreedy,gcc_simple) + 1],vertex.label=NA, vertex.size=3)
dev.off()
community_simple_labelpropogation = label.propagation.community(gcc_simple, weights = E(gcc_simple)$weight)
print("option 2, label propagation")
print(paste("sizes: ", sizes(community_simple_labelpropogation)))
print(paste("modularity: ", modularity(community_simple_labelpropogation)))
tiff("p3_3.tiff", width = 3200, height = 3200, units = "px", res = 800)
plot(community_simple_labelpropogation, gcc_simple, layout=layout.fruchterman.reingold(gcc_simple, niter=10000), col = membership(community_simple_labelpropogation),mark.groups = communities(community_simple_labelpropogation), edge.color = c("black", "red")[crossing(community_simple_labelpropogation,gcc_simple) + 1],vertex.label=NA, vertex.size=3)
dev.off()

#(4)
non_gcc_simple_idx = V(gcc_simple)[which(community_simple_fastgreedy$membership != which.max(sizes(community_simple_fastgreedy)))]
sub_community = delete.vertices(gcc_simple, non_gcc_simple_idx)
sub_community_simple_fastgreedy = fastgreedy.community(sub_community, weights = E(sub_community)$weight)
print(paste("sizes: ", sizes(sub_community_simple_fastgreedy)))
print(paste("modularity: ", modularity(sub_community_simple_fastgreedy)))
tiff("p4_1.tiff", width = 3200, height = 3200, units = "px", res = 800)
plot(sub_community_simple_fastgreedy, sub_community, layout=layout.fruchterman.reingold(sub_community, niter=10000), col = membership(sub_community_simple_fastgreedy),mark.groups = communities(sub_community_simple_fastgreedy), edge.color = c("black", "red")[crossing(sub_community_simple_fastgreedy,sub_community) + 1],vertex.label=NA, vertex.size=3)
dev.off()

#(5)
# community_idx = which(sizes(community_simple_fastgreedy)>100)
# for(i in 1:length(community_idx)) {
# 	print(paste("\nsub-community index",community_idx[[i]],":\n"))
# 	non_community_idx = V(gcc_simple) [which(community_simple_fastgreedy$membership != community_idx[i])]
# 	sub_community_100 = delete.vertices(gcc_simple, non_community_idx)
# 	sub_community_100_fastgreedy = fastgreedy.community(sub_community_100, weights = E(sub_community_100)$weight)
# 	print(paste("sub-community structure using fastgreedy:\n"))
# 	print(paste("sizes: ", sizes(sub_community_100_fastgreedy)))
# 	print(paste("modularity: ", modularity(sub_community_100_fastgreedy)))
# }

community_idx = which(sizes(community_simple_labelpropogation)>100)
for(i in 1:length(community_idx)) {
	print(paste("\nsub-community index",community_idx[[i]],":\n"))
	non_community_idx = V(gcc_simple) [which(community_simple_labelpropogation$membership != community_idx[i])]
	sub_community_100 = delete.vertices(gcc_simple, non_community_idx)
	sub_community_100_labelpropagation = label.propagation.community(sub_community_100, weights = E(sub_community_100)$weight)
	print(paste("sub-community structure using label propagation:\n"))
	print(paste("sizes: ", sizes(sub_community_100_labelpropagation)))
	print(paste("modularity: ", modularity(sub_community_100_labelpropagation)))
}






