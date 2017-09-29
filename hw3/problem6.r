library('igraph')
library('netrw')
g = read.graph("C:\\Users\\SoniaYu\\Desktop\\sorted_directed_net.txt", format = "ncol", directed = TRUE)
cl = clusters(g)
gcc_idx = which.max(cl$csize)
non_gcc_idx = (1:vcount(g))[cl$membership != gcc_idx]
gcc = delete.vertices(g, non_gcc_idx)

gcc_simple = as.undirected(gcc, mode = "collapse", edge.attr.comb = function(weight) sqrt(prod(weight)))
#community_simple = fastgreedy.community(gcc_simple, weights = E(gcc_simple)$weight)
community_simple= label.propagation.community(gcc_simple, weights = E(gcc_simple)$weight)

multi_community = numeric(0)
k = 0
#threshold = 0.1
#threshold = 0.2
threshold = 0.3
for (i in 1:vcount(g)) {
	M = rep(0, length(community_simple))
	teleport_prob = rep(0, vcount(g))
	teleport_prob[i] = 1

	rw = netrw(g, walker.num = 1, start.node = i, damping = 0.85, output.visit.prob = TRUE, teleport.prob = teleport_prob)
	prob_sorted = sort(rw$ave.visit.prob, decreasing = TRUE, index.return = TRUE)

	for (j in 1:30) {
		m = rep(0, length(community_simple))
		m[community_simple$membership[which(V(gcc)==V(g)[prob_sorted$ix[j]])]]=1
		M = M + prob_sorted$x[j] * m		
	}
	if(length(which(M > threshold)) > 1) {
		node = c(i, M)
		multi_community = rbind(multi_community, node)
		k = k + 1
		print(paste(k,": Node", i, "belongs to", length(which(M > threshold)), "communities\n"))
	}


}

