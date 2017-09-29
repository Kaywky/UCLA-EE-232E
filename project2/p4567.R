library('igraph')

setwd("~/Document/EE232/proj2/src")

#problem 4
el=read.csv("smallEdges.txt", header = FALSE) # read  the file
el=as.matrix(el) # igraph needs the edgelist to be in matrix format
movies_network = graph.edgelist(el[,1:2], directed=FALSE)  # generate a network from the first two columns, which has the list of vertices
E(movies_network)$weight=el[,3]
cat("network generated")
movies_community = fastgreedy.community(movies_network)
cat("community found")

#problem 5

genre_file = file("newMovieList.csv", open = "r")
genre_lines = readLines(genre_file)
movies_genre = rep("null", length(genre_lines))
for(i in 1:length(genre_lines)){
  genre_list = strsplit(genre_lines[i], ",")
  movies_genre[i] = genre_list[[1]][3]
  cat(i, genre_list[[1]][3],"\n")
}
close(genre_file)
V(movies_network)$genre = movies_genre # add genre attribute to graph
cat("genre attribute added")

community_tag = numeric(0)
for (i in 1:length(movies_community)){
  community_genre = V(movies_network)[which(movies_community$membership) == i]$genre
  max_length = 0
  max_type = ""
  genre_type = unique(community_genre)
  for(genre in genre_type){
    if(length(which(community_genre == genre)) > max_length && length(which(community_genre == genre) > 0.2*length(community_genre))){
      max_length = length(community_genre[which(community_genre == genre)])
      max_type = genre
    }
  }
  cat(i, "\t", max_type, "\t", max_length, "\n")
  community_tag = c(community_tag, max_type)
}
cat("community tag generated")

#problem 7
rating_file = file("newMovieList.csv", open = "r")
rating_lines = readLines(rating_file)
movies_rating = rep(as.double(0), vcount(movies_network))
for(i in 1:vcount(movies_network)){
  rating_list = strsplit(rating_lines[i], ",")
  movies_rating[i] = as.double(rating_list[[1]][2])
  cat(i, rating_list[[1]][2],"\n")
}
close(rating_file)
V(movies_network)$rating = movies_rating # add rating attribute to graph
cat("rating attribute added")

#problem 6
nodeID = c(2734, 5007, 59559)
for (node in nodeID){
  node_index = as.numeric(node)
  node_neighbors = neighbours(movies_network, node_index)
  node_weights = numeric(0)
  for(node_neighbor in node_neighbors){
    node_weight = E(movies_network, P = c(node_index, node_neighbour))$weight
    node_weights = c(node_weights, node_weight)
  }
  sorted_weight = sort(node_weights, decreasing = T, index.return = T)
  near_neighbours = node_neighbors[sorted_weight$ix[1:5]]
  cat(node, "\n")
  cat("neighbours:", V(movies_network)[near_neighbours], "\n")
  cat("community:", movies_community$membership[near_neighbours],"\n")
  cat("genre:", V(movies_network)$genre[near_neighbours], "\n")
  # after adding rating attribute in problem 7
  cat("rating:", V(movies_network)$rating[near_neighbours],"\n")
  neighbours_rating = as.numeric(V(movies_network)$rating[near_neighbours])
  node_rating = mean(neighbours_rating)
  cat("node rating:", node_rating, "\n")
}


