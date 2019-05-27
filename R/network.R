getNodes = function(blocks, tau = NULL, rgcca = NULL) {

  if(attr(rgcca,"class") == "sgcca")
    par = "c1"
  else
    par = "tau"

  if(any(tau == "optimal")){
    if(!is.null(rgcca))
      tau = unlist(lapply(1:ncol(rgcca[[par]]), function(x) Reduce(paste, round(rgcca[[par]][, x],2))))
    else
      tau = rep(NA, length(blocks))
  }

  if(is.null(tau)){
    if(is.matrix(rgcca[[par]]))
      tau = unlist(lapply(1:ncol(rgcca[[par]]), function(x) Reduce(paste, round(rgcca[[par]][, x],2))))
    else
      tau = rgcca[[par]]
  }

  nrow = unlist(lapply(blocks, function(x) ifelse(is.null(attributes(x)$nrow), nrow(blocks[[1]]) , attributes(x)$nrow)))

  values <- list( names(blocks), unlist(lapply(blocks, NCOL)), nrow, tau )
  nodes <- as.data.frame(matrix(unlist(values), length(blocks), length(values)))
  colnames(nodes) = c("id", "P", "nrow", par)

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

  edges <- as.data.frame(t(matrix(unlist(edges), 3, length(edges))))
  colnames(edges) = c("from", "to", "weight")
  edges[, 3] <- as.numeric(edges[, 3])

  return(edges)
}

colorNodes = function(nodes){
  unlist( lapply(as.list(1 - nodes$P/max(nodes$P)),
                 function(x) rgb(colorRamp(c("coral3", "khaki2"))(x)/255) ) )
}

plotNetwork = function(nodes, edges, blocks){

  # Avoid random
  set.seed(1)
  V <- E <- NULL

  par = ifelse("c1" %in% names(nodes), "c1", "tau")

  net <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

  if(all(is.na(nodes[, par]))){
    nodes[, par] = rep("optimal", length(blocks))
    V(net)$tau = rep(1, length(blocks))
  }

  V(net)$color <- "khaki2"
  V(net)$label <- paste(nodes$id, "\nP =", nodes$P, paste0("\n", par, " ="), nodes[, par], "\nN =", nodes$nrow, sep=" ")
  V(net)$label.font <- 3
  V(net)$shape <- "square"
  E(net)$width <- E(net)$weight * 2

  plot(net,
       edge.color = "gray70",
       edge.lty = 2,
       vertex.frame.color = "gray50",
       vertex.label.color = "black",
       vertex.label.dist = 6,
       vertex.label.degree = 1.5,
       main = paste0("Number of common rows between blocks : ", nrow(blocks[[1]]) , "."))

}

plotNetwork2 = function(nodes, edges, blocks){

  # Avoid random
  set.seed(1)
  par = ifelse("c1" %in% names(nodes), "c1", "tau")

  if(all(is.na(nodes[, par])))
    nodes[, par] = rep("optimal", length(blocks))

  nodes$title  <- nodes$id
  nodes$label  <- paste(nodes$id, "\nP =", nodes$P, paste0("\n", par, " ="), nodes[, par], "\nN =", nodes$nrow, sep=" ")

  edges$width <- edges$weight * 2
  nodes$color.background <- rep("#eee685", length(blocks))

  visnet <- visNetwork(nodes, edges, main = paste0("Number of common rows between blocks : ", nrow(blocks[[1]]) , "."))

  visnet <- visNodes(visnet,
                     borderWidth = 2,
                     shape = "square",
                     shadow = TRUE,
                     color=list(border = "gray",
                                highlight = list(background = "black", border = "darkred"))
  )

  visEdges(visnet,
           smooth = FALSE,
           shadow = TRUE,
           dashes = TRUE,
           color = list(color = "gray", highlight = "darkred")
  )

}
