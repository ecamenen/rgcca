getNodes = function(opt, blocks) {

  values <- list( names(blocks), unlist(lapply(blocks, NCOL)), opt$tau, opt$ncomp )
  nodes <- as.data.frame(matrix(unlist(values), length(blocks), length(values)))
  colnames(nodes) = c("id", "P", "tau", "ncomp")

  nodes[, c(2, 3, 4)] <- apply(nodes[, c(2, 3, 4)], 2, as.numeric)

  return(nodes)
}

getEdges = function(connection, blocks) {

  J = NCOL(connection)

  edges = list()

  k = 0
  for (j in 1:J) {
    for (i in 1:J){
      if(i > k && connection[i, j] > 0)
        edges[[ length(edges) + 1 ]] = c( names(blocks)[j], names(blocks)[i], connection[i, j] )
    }
    k = k + 1
  }

  # ceiling(which(connection > 0) / nrow(connection) )
  # which(connection > 0) %% nrow(connection)

  edges <- as.data.frame(t(matrix(unlist(edges), 3, length(edges))))
  colnames(edges) = c("from", "to", "weight")
  edges[, 3] <- as.numeric(edges[, 3])

  return(edges)
}

nodes <- getNodes(opt, blocks)
edges <- getEdges(connection, blocks)

library("igraph")
net <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

plotNetwotk = function(net, blocks){

V(net)$color <-  colorGroup(as.factor(names(blocks)))
V(net)$size <- V(net)$tau * 50
V(net)$label.font <- 3
E(net)$width <- E(net)$weight

plot(net,
     edge.color = "gray80",
     vertex.frame.color="white",
     vertex.label.color="black")
}

plotNetwotk(net, blocks)


plotNetwotk2 = function(nodes, edges, blocks){
  nodes$shape  <- "square"
  nodes$shadow <- TRUE

  nodes$title  <- nodes$id
  nodes$label  <-  nodes$id
  nodes$size   <-nodes$tau * 50
  nodes$color.background <- colorGroup(as.factor(names(blocks)))

  nodes$borderWidth <- nodes$weight
  nodes$color.highlight.background <- "black"
  nodes$color.highlight.border <- "darkred"
  nodes$color.border <- "gray"

  edges$color <- "gray"
  edges$shadow <- TRUE

  visNetwork(nodes, edges)
}

plotNetwotk2(nodes, edges, blocks)

