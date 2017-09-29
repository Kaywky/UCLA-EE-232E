rm(list = ls())
library(igraph)

setwd("/Users/yunqiguo/Document/EE232/Proj1/gplus/")

edgesFiles = list.files(path=filesPath1,pattern="edges")
cat(edgesFiles)
g2Raw<-{}
g2<-{}
g2u<-{}
egoNodes={}
circlesRaw={{}}

count=1
commu1 <- {{}}
commu2 <- {{}}
for(j in 1:132)
{
  nodeId = sub("\\..*","",edgesFiles[j])
  circlesFile = paste(filesPath1,nodeId,".circles",sep="")
  fileConnection <- file(circlesFile, open="r")
  if(length(fileConnection)>0)
  {
    lines <- readLines(fileConnection)
    if(length(lines>0))
    {
      circles <- list()
      print(length(lines))
      for (i in 1:length(lines)) 
      {
        sp <- strsplit(lines[i],"\t")
        circles[[i]] <- sp[[1]][-1]
      }
      
      if(length(circles)>2)
      {
        print("Found one!")
        edgelistFile = paste(filesPath1,edgesFiles[j],sep="")
        
        g2Raw[[count]] = read.graph(edgelistFile,format="ncol",directed=TRUE)
        circlesRaw[[count]] <-circles
        
        
        nonEgoNodes = V(g2Raw[[count]])
        egoNodes[count]=nodeId
        g2[[count]] <- add.vertices(g2Raw[[count]],1,name=nodeId)
        egoNodeIndex <- which(V(g2[[count]])$name==nodeId) 
        edgeAppendList <- c()
        for (nodeIndex in 1:(vcount(g2[[count]])-1)) 
        {
          edgeAppendList <- c(edgeAppendList , c(vcount(g2[[count]]),nodeIndex))
        }
        g2[[count]] <- add.edges(g2[[count]],edgeAppendList)
        g2u[[count]]<- as.undirected(g2[[count]])
        commu1[[count]] <- walktrap.community(g2u[[count]], steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)
        commu2[[count]] <- infomap.community (g2u[[count]], e.weights = NULL, v.weights = NULL, nb.trials = 10, modularity = TRUE)
        
        count=count+1
        print(count)
      }
    } 
  }
  close(fileConnection)
}
##Sample Community Plot
gcomm_1<-walktrap.community(g2[[1]])
plot(gcomm_1,g2[[1]],vertex.label=NA,vertex.size=7,edge.arrow.size=0.2,main="Community Structure of ego node 1 by Walktrap")
gcomm_2<-infomap.community(g2[[1]])
plot(gcomm_2,g2[[1]],vertex.label=NA,vertex.size=7,edge.arrow.size=0.2,main="Community Structure of ego node 1 by Infomap")

plot(commu1[[1]],g2u[[1]],,vertex.label=NA,vertex.size=7,edge.arrow.size=0.2,main="Community Structure of ego node 1 by Walktrap")
plot(commu2[[1]],g2u[[1]],,vertex.label=NA,vertex.size=7,edge.arrow.size=0.2,main="Community Structure of ego node 1 by Infomap")


match_index <- list()
match_index1 <- list()
match_index2<-list()
array1 <- list()
array2 <- list()
z <- 1
circles<-circlesRaw
{
  match_index1 <- list()
  for(i in 1:57){
    if(length(commu1[[i]]) != 0){
      match_index1[[i]] <- list()
      for(c1 in 1:length(commu1[[i]])){
        match_index1[[i]][[c1]] <- list()
        array1 <- (commu1[[i]])$names[which(commu1[[i]]$membership==c1)]
        for(j in 1:length(circles[[i]])){
          array2 <- circles[[i]][[j]] 
          match_index1[[i]][[c1]][[j]] <- 2*length(intersect(array1,array2))/(length(array1)+length(array2))
          match_index1[[i]] <- 2*length(intersect(array1,array2))/(length(array1)+length(array2))
          z <- z + 1
          write(paste(c( ": communities ", i, "|", egoNodes[i], ".", c1, " ", j, "|", egoNodes[j], ".", " match_index - ", match_index1[[i]][[c1]][[j]])), file="C:/Users/Leo/Dropbox/232e hw/proj1/out2.txt", append=TRUE)
        }
      }
    }
  }
}