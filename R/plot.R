# Author: Etienne CAMENEN
# Date: 2018
# Contact: arthur.tenenhaus@l2s.centralesupelec.fr
# Key-words: omics, RGCCA, multi-block
# EDAM operation: analysis, correlation, visualisation
#
# Abstract: A user-friendly multi-blocks analysis (Regularized Generalized Canonical Correlation Analysis, RGCCA)
# with all default settings predefined. Produce four figures to help clinicians to identify fingerprint:
# the samples and the variables projected on the two first component of the multi-block analysis, the histograms
# of the most explicative variables and the explained variance for each blocks.

#Global settings
AXIS_TITLE_SIZE = 19
AXIS_TEXT_SIZE = 10
PCH_TEXT_SIZE = 3
AXIS_FONT = "italic"
SAMPLES_COL_DEFAULT = "brown3"

# Creates a circle
circleFun = function(center = c(0, 0), diameter = 2, npoints = 100) {

  r = diameter/2
  tt = seq(0, 2 * pi, length.out = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[2] + r * sin(tt)

  return(data.frame(x = xx, y = yy))
}

#' Print the variance of a component
#'
#' Prints the percent of explained variance for a component of a block (by default, the superblock or the last one) analysed by R/SGCCA
#'
#' @param rgcca A list giving the results of a R/SGCCA
#' @param n An integer giving the index of the analysis component
#' @param i An integer giving the index of a list of blocks
#' @seealso \code{\link[RGCCA]{rgcca}}, \code{\link[RGCCA]{sgcca}}
#' @examples
#' AVE = list(c(0.6, 0.5), c(0.7, 0.45))
#' rgcca.res = list(AVE = list(AVE_X = AVE))
#' # For the superblock (or the last block)
#' printAxis(rgcca.res, 1)
#' # "Axis 1 (70%)"
#' # For the first block
#' printAxis(rgcca.res, 2, 1)
#' # "Axis 2 (50%)"
#' @export printAxis
printAxis = function (rgcca, n, i = NULL, outer = FALSE){

  # by default, take the last block
  if ( is.null(i) )
    i = length(rgcca$AVE$AVE_X)

  if(isTRUE(outer))
    AVE = rgcca$AVE$AVE_outer
  else
    AVE = rgcca$AVE$AVE_X[[i]]

  nvar = varSelected(rgcca, i, n)
  if(class(rgcca) !="sgcca" | nvar == length(rgcca$a[[i]][, n]) )
    varText = ""
  else
    varText = paste(nvar, " variables, ", sep="")

  paste("Component ", n, " (", varText, round(AVE[n] * 100 , 1),"%)", sep = "")
}

varSelected = function(rgcca, i_block, comp)
  sum(rgcca$a[[i_block]][,comp] != 0)

#' Default font for plots
theme_perso = function() {

  theme(
    legend.text = element_text(size = 13),
    legend.title = element_text(face="bold.italic", size=16),
    plot.title = element_text(size = 25, face = "bold", hjust=0.5, margin = margin(0,0,20,0))
  )
}

colorGroup = function(group){
  palette = rep(c("#cd5b45", "#71ad65", "#3c78b4", "#ffc600",
                  "#b448af", "#9d9d9d", "#abcaef", "#4a6f43",  "#f0e500",
                  "#efb8f0", "black", "#d6d6d6" ), 10)
  palette[0 : length(levels(as.factor(group))) ]
}

#' Plot of samples space
#'
#' Plots samples on two components of a RGCCA
#'
#' @param rgcca A list giving the results of a R/SGCCA
#' @param resp A vector of characters corresponding either to a qualitative variable with levels or a continuous variable
#' @param comp_x An integer giving the index of the analysis component used for the x-axis
#' @param comp_y An integer giving the index of the analysis component used for the y-axis
#' @param i_block An integer giving the index of a list of blocks
#' @param text A bolean to represent the points with their row names (TRUE) or with circles (FALSE)
#' @param i_block_y An integer giving the index of a list of blocks (another one, different from the one used in i_block)
#' @examples
#' coord = lapply(1:3, function(x) matrix(runif(15 * 2, min = -1), 15, 2))
#' AVE_X = lapply(1:3, function(x) runif(2))
#' rgcca.res = list(Y = coord, AVE = list(AVE_X = AVE_X))
#' # Using a superblock
#' plotSamplesSpace(rgcca.res, rep(LETTERS[1:3], each = 5))
#' # Using the first block
#' plotSamplesSpace(rgcca.res, runif(15, min=-15, max = 15), 1, 2, 1)
#' @export plotSamplesSpace
plotSamplesSpace = function (rgcca, resp, comp_x = 1, comp_y = 2, i_block = NULL, text = TRUE, i_block_y = NULL){
  # resp : color the points with a vector

  if ( is.null(i_block) )
    i_block = length(rgcca$Y)

  if(is.null(i_block_y)){
    df = data.frame(rgcca$Y[[i_block]][, c(comp_x, comp_y)])
  }else
    df =  data.frame(rgcca$Y[[i_block]][, comp_x], rgcca$Y[[i_block_y]][, comp_y] )

  if(nrow(df) > 100)
    PCH_TEXT_SIZE = 2

  # if the resp is numeric
  if (  length(unique(as.matrix(resp))) > 1 ){

    if(!is.null(rownames(resp))){

      names = row.names(resp)
      resp = as.matrix(apply(as.matrix(resp), 1, as.character), row.names = row.names(resp))
      diff_column = setdiff(row.names(blocks[[i_block]]), row.names(resp))

      if(length(diff_column) > 1 ){
        resp[diff_column ] <- 'NA'
        names(resp)[names(resp)==""] <- names
      }else{
        names(resp) = names
      }

      resp = resp[row.names(blocks[[i_block]])]
    }

    if( ! unique(isCharacter(as.vector(resp))) && length(levels(as.factor(as.vector(resp)))) > 5 ){

      if(length(diff_column) > 1 ){
        resp = resp
        resp[diff_column ] <- NA
      }

      resp = as.numeric(resp)

      # add some transparency
      p = ggplot(df, aes(df[, 1], df[, 2], alpha = (resp - min(resp, na.rm = T)) / max(resp - min(resp, na.rm = T), na.rm = T))) +
      # get a color scale by quantile
            scale_alpha_continuous(
            name = "resp",
            breaks = seq(0, 1, .25),
            labels = round(quantile(as.matrix(resp), na.rm = T), 2)
        )

      print(resp)

    }else
      p = NULL
  }else
    p = NULL

  p = plotSpace(rgcca, df, "Samples", resp, "Response", comp_x, comp_y, i_block, p, text, i_block_y)

  # remove legend if missing
  if ( length(unique(resp)) == 1){
    p + theme(legend.position = "none")
  }else
    p
}

#' Get the blocs of each variables
#'
#' Get a vector of block names for each corresponding variable. The last block is considered as the superblock and ignored.
#'
#' @param rgcca A list giving the results of a R/SGCCA
#' @return A vector of character giving block names for each corresponding variable.
#' @seealso \code{\link[RGCCA]{rgcca}}, \code{\link[RGCCA]{sgcca}}
#' @examples
#' rgcca.res = list(a = rep(NA, 4))
#' names(rgcca.res$a) = LETTERS[1:4]
#' getBlocsVariables(rgcca.res)
#' # a, b, c
#' @export getBlocsVariables
getBlocsVariables = function(rgcca){

  rep( names(rgcca$a)[-length(rgcca$a)],
       sapply(rgcca$a[1:(length(rgcca$a)-1)], NROW))
}

#' Plot of variables space
#'
#' Correlation circle highlighting the contribution of each variables to the construction of the RGCCA components
#' @param rgcca A list giving the results of a R/SGCCA
#' @param blocks A list of matrix
#' @param comp_x An integer giving the index of the analysis component used for the x-axis
#' @param comp_y An integer giving the index of the analysis component used for the y-axis
#' @param superblock A boolean giving the presence (TRUE) / absence (FALSE) of a superblock
#' @param i_block An integer giving the index of a list of blocks
#' @param text A bolean to represent the points with their row names (TRUE) or with circles
#' @examples
#' setMatrix = function(nrow, ncol, iter = 3) lapply(1:iter, function(x) matrix(runif(nrow * ncol), nrow, ncol))
#' blocks = setMatrix(10, 5)
#' blocks[[4]] = Reduce(cbind, blocks)
#' for (i in 1:4)
#'     colnames(blocks[[i]]) = paste( LETTERS[i], as.character(1:NCOL(blocks[[i]])), sep="" )
#' coord = setMatrix(10, 2, 4)
#' a = setMatrix(5, 2)
#' a[[4]] = matrix(runif(15 * 2), 15, 2)
#' AVE_X = lapply(1:4, function(x) runif(2))
#' rgcca.res = list(Y = coord, a = a, AVE = list(AVE_X = AVE_X))
#' names(rgcca.res$a) = LETTERS[1:4]
#' # Using a superblock
#' plotVariablesSpace(rgcca.res, blocks, 1, 2, TRUE)
#' # Using the first block
#' plotVariablesSpace(rgcca.res, blocks, 1, 2, FALSE, 1)
#' @export plotVariablesSpace
plotVariablesSpace = function(rgcca, blocks, comp_x = 1, comp_y = 2, superblock = TRUE, i_block = NULL, text = TRUE){

  x = y = NULL

  if ( is.null(i_block) )
    i_block = length(blocks)

  df =  data.frame(
    #correlation matrix within a block for each variables and each component selected
    sapply ( c(comp_x, comp_y), function(x) cor( blocks[[i_block]], rgcca$Y[[i_block]][, x] ) ) ,
    row.names = colnames(blocks[[i_block]])
  )

  if(class(rgcca)=="sgcca"){
    selectedVar = rgcca$a[[i_block]][,comp_x] != 0 | rgcca$a[[i_block]][,comp_y] != 0
    df = df[selectedVar, ]
  }

  if(nrow(df) > 200){
    df = df [as.vector (unique( sapply(c(comp_x, comp_y),
                                       function(x) row.names(data.frame(df[order(abs(df[, x]), decreasing = TRUE),])[1:100,])))), ]
  }

  # if superblock is selected, color by blocks
  if ( superblock & ( i_block == length(blocks)) ){
    color = getBlocsVariables(rgcca)
    if(class(rgcca)=="sgcca")
      color = color[row.names(df[selectedVar, ])]
  }else{
    color = rep(1, NROW(df))
  }

  df = data.frame(df, color)

  p = plotSpace(rgcca, df, "Variables", color, "Blocks", comp_x, comp_y, i_block, text = text) +
    geom_path(aes(x, y), data = circleFun(), col = "grey", size = 1) +
    geom_path(aes(x, y), data = circleFun()/2, col = "grey", size = 1, lty = 2)

  # remove legend if not on superblock
  if ( !superblock || i_block != length(blocks) )
    p + theme(legend.position = "none")
  else
    p

}

#' Plot of components space
#'
#' Plots RGCCA components in a bi-dimensional space
#'
#' @param rgcca A list giving the results of a R/SGCCA
#' @param df A dataframe
#' @param title A character with the name of the space (either "Variables" or "Samples")
#' @param group A vector of character with levels used to color the points
#' @param name_group A character giving the type of groups (either "Blocs"  or "Response")
#' @param comp_x An integer giving the index of the analysis component used for the x-axis
#' @param comp_y An integer giving the index of the analysis component used for the y-axis
#' @param i_block An integer giving the index of a list of blocks
#' @param p A ggplot object
#' @param text A bolean to represent the points with their row names (TRUE) or with circles (FALSE)
#' @param i_block_y An integer giving the index of a list of blocks (another one, different from the one used in i_block)
#' @examples
#' df = as.data.frame(matrix(runif(20*2, min = -1), 20, 2))
#' AVE =  lapply(1:4, function(x) runif(2))
#' rgcca.res = list(AVE = list(AVE_X = AVE))
#' plotSpace(rgcca.res, df, "Samples", rep(c("a","b"), each=10), "Response")
#' @export plotSpace
plotSpace = function (rgcca, df, title, group, name_group, comp_x = 1, comp_y = 2, i_block = 1, p = NULL, text = TRUE, i_block_y = NULL, no_Overlap = TRUE){

  if(is.null(i_block_y))
    i_block_y = i_block

  if (!isTRUE(text)){
    func = quote(geom_point(size = PCH_TEXT_SIZE, aes(shape = as.factor(group))))
  }else{
    f = "geom_text"
    func = quote(get(f)(aes(label = rownames(df)), size = PCH_TEXT_SIZE))

    if(no_Overlap && nrow(df) <= 100){
      f = paste0(f, "_repel")
      func$force = 0.2
      func$max.iter = 500
    }
  }

  if(title == "Samples" && !is.null(p))
    func$colour = SAMPLES_COL_DEFAULT

  if (is.null(p)){
    p = ggplot(df, aes(df[,1], df[,2], colour = as.factor(group)))
  }

  p + eval(as.call(func)) +
    theme_classic() +
    geom_vline(xintercept = 0, col = "grey", linetype = "dashed", size = 1) +
    geom_hline(yintercept = 0, col = "grey", linetype = "dashed", size = 1) +
    labs ( title = paste(title, "space"),
           x = printAxis(rgcca, comp_x, i_block),
           y = printAxis(rgcca, comp_y, i_block_y),
           color = name_group,
           shape = name_group) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = NULL) +
    scale_color_manual(values = colorGroup(group), na.translate = TRUE) +
    theme_perso() +
    theme(
      axis.text = element_blank(),
      axis.title.y = element_text(face = AXIS_FONT, margin = margin(0,20,0,0), size = AXIS_TITLE_SIZE),
      axis.title.x = element_text(face = AXIS_FONT, margin = margin(20,0,0,0), size = AXIS_TITLE_SIZE)
    )
}

#' Histogram of a fingerprint
#'
#' Histogram of the higher outer weight vectors for a component of a block (by default, the superblock or the last one) analysed by R/SGCCA
#'
#' @param rgcca A list giving the results of a R/SGCCA
#' @param comp An integer giving the index of the analysis components
#' @param n_mark An integer giving the number of top potential biomarkers to select
#' @param superblock A boolean giving the presence (TRUE) / absence (FALSE) of a superblock
#' @param i_block An integer giving the index of a list of blocks
#' @seealso \code{\link[RGCCA]{rgcca}}, \code{\link[RGCCA]{sgcca}}
#' @examples
#' weights = lapply(1:3, function(x) matrix(runif(10*2), 10, 2))
#' weights[[4]] = Reduce(rbind, weights)
#' rgcca.res = list(a = weights)
#' names(rgcca.res$a) = LETTERS[1:4]
#' # With the 1rst component of the superblock
#' plotFingerprint(rgcca.res, 1, TRUE)
#' # With the 2nd component of the 1rst block by selecting the ten higher weights
#' plotFingerprint(rgcca.res, 2, FALSE, 10, 1)
#' @export plotFingerprint
plotFingerprint = function(rgcca, comp = 1, superblock = TRUE, n_mark = 100, i_block = NULL){

  color = NULL

  # if no specific block is selected, by default, the superblock is selected (or the last one)
  if ( is.null(i_block) )
    i_block = length(rgcca$a)

  # select the weights (var to add a column to work with comp = 1)
  df = data.frame(rgcca$a[[i_block]], var = row.names(rgcca$a[[i_block]]))

  # Get a qualitative variable with which block is associated with each variables
  if (  superblock & ( i_block == length(rgcca$a) ) )
    df = data.frame( df, color = getBlocsVariables(rgcca) )

  # sort in decreasing order
  df = data.frame(df[order(abs(df[, c(comp)]), decreasing = TRUE),], order = nrow(df):1)

  # selected variables in sgcca
  nvar_select = varSelected(rgcca, i_block, comp)
  if(n_mark > nvar_select)
    n_mark = nvar_select

  # max threshold for n
  if(NROW(df) >= n_mark) df = df[1:n_mark,]

  # if the superblock is selected, color the text of the y-axis according to their belonging to each blocks
  if (  superblock & ( i_block == length(rgcca$a) ) ){
    color2 = factor(df$color); levels(color2) = colorGroup(color2)
  }else{
    color2 = "black"
  }

  if (  superblock & i_block == length(rgcca$a) ){
    p = ggplot(df, aes(order, df[, comp], fill = as.factor(color)))
  }else{
    p = ggplot(df, aes(order, df[, comp], fill = abs(df[, comp])))
  }

  p = plotHistogram(p, df, "Variable weights", as.character(color2)) +
  labs(subtitle = printAxis(rgcca, comp, i_block))

  if(length(color2) != 1)
    p = p + scale_fill_manual(values = colorGroup(color2))

  if (  !superblock | i_block != length(rgcca$a) )
    p = p + theme(legend.position = "none")

  return(p)
}

#' Histogram of Average Variance Explained
#'
#' Histogram of the model quality (based on Average Variance Explained) for each blocks and sorted in decreasing order
#'
#' @param rgcca A list giving the results of a R/SGCCA
#' @param comp An integer giving the index of the analysis components
#' @seealso \code{\link[RGCCA]{rgcca}}, \code{\link[RGCCA]{sgcca}}
#' @examples
#' random_val = function() lapply(1:4, function(x) runif(1))
#' rgcca.res = list(AVE = list(AVE_X = random_val()), a = random_val())
#' names(rgcca.res$a) = LETTERS[1:4]
#' library("ggplot2")
#' plotAVE(rgcca.res, 1)
#' @export plotAVE
plotAVE = function(rgcca, comp = 1){

  ave = as.vector(Reduce(c, rgcca$AVE$AVE_X))
  blocks = unlist(lapply(1:length(names(rgcca$a)), function(x) rep(names(rgcca$a)[x], rgcca$ncomp[x])))
  ncomp = as.vector(Reduce(c, lapply(rgcca$AVE$AVE_X, names)))

  y_ave_cum = lapply(lapply(rgcca$AVE$AVE_X, function(x) round(cumsum(rev(x)), 2)), function(x) c(0, x))
  y_ave_cum = unlist(lapply(y_ave_cum, function(x) unlist(lapply(1:length(x), function(i) (x[i-1] + x[i]) / 2 ))))

  ave_label =  unlist(lapply(rgcca$AVE$AVE_X, function(x) round(rev(x), 2)))
  ave_label[ave_label < max(y_ave_cum)/10] =  ""


  df = data.frame(ave, blocks, ncomp)
  p = ggplot(data=df, aes(x=blocks, y=ave, fill = ncomp, label =  ave_label))

  p = plotHistogram(p, df, "Average Variance Explained") +
    labs(subtitle = printAxis(rgcca, comp, outer = TRUE)) +
    geom_text(aes(y = y_ave_cum), cex = 3) +
    scale_fill_manual(values=colorGroup(levels(df$ncomp)), labels = gsub("comp", " ", levels(df$ncomp))) +
    labs( fill = "Components" )

  return(p)
}

#' Histogram settings
#'
#' Default font for a vertical barplot.
#'
#' @param p A ggplot object.
#' @param df A dataframe with a column named "order"
#' @param title A character string giving a graphic title
#' @param color A vector of character giving the colors for the rows
#' @param low_col A character giving the color used for the lowest part of the gradient
#' @param high_col A character giving the color used for the highest part of the gradient
#' @examples
#' df = data.frame(x = runif(30), order = 30:1)
#' library("ggplot2")
#' p = ggplot(df, aes(order, x))
#' plotHistogram(p, df, "This is my title")
#' # Add colors per levels of a variable
#' df$color = rep(c(1,2,3), each=10)
#' p = ggplot(df, aes(order, x, fill = color))
#' plotHistogram(p, df, "Histogram", as.character(df$color))
#' @export plotHistogram
plotHistogram = function(p, df, title = "", color = "black", low_col = "khaki2", high_col = "coral3"){

  if ( nrow(df) <= 10 || title == "Average Variance Explained" ){
    WIDTH = NULL
  }else{
    WIDTH = 1
    AXIS_TEXT_SIZE = 8
  }

  p = p +
    geom_bar(stat = "identity", width = WIDTH) +
    geom_hline(yintercept = 0, col = "gray40", size = 1) +
    coord_flip()  +
    labs(
      title = title,
      x = "", y = "") +
    theme_classic() +
    theme_perso() +
    theme(
      axis.text.y = element_text(size = AXIS_TEXT_SIZE, face = AXIS_FONT, color = color),
      axis.text.x = element_text(size = AXIS_TEXT_SIZE, face = AXIS_FONT, color = "gray40"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic"))

  if(title != "Average Variance Explained"){
    p  = p +
      scale_x_continuous(breaks = df$order, labels = rownames(df)) +
      labs( fill = "Blocks")
    if(length(color) == 1){
      p = p + scale_fill_gradient(low = low_col, high = high_col)
    }
  }

  return(p)
}

corResponse = function(rgcca, blocks, response = NULL, comp = 1, i_block = 1){

  if(is.null(response))
    response = blocks[[ length(rgcca$a) ]]
  else{
    common_rows = intersect(row.names(blocks[[i_block]]), row.names(response))
    response = response[common_rows, ]
    options(warn = -1)
    # Disabling automatic factor conversion for some columns
    response = apply(response, 2, as.double)
    options(warn = 0)
  }

  cor.res = matrix(cor(rgcca$Y[[i_block]][, comp],
                       response,
                       use = "pairwise.complete.obs"),
                   dimnames = list(colnames(response, NULL)))
  res = data.frame(cor = cor.res[order(abs(cor.res[, 1]),
                                       decreasing = TRUE),],
                   order = length(cor.res):1)

  if(VERBOSE) print(res)

  p = ggplot(res, aes(order, cor, fill = abs(res[,1])))
  plotHistogram(p, res, "Correlation to the response") +
    labs(subtitle = printAxis(rgcca, comp, i_block))  +
    theme(legend.position = "none")
}

getCor = function(rgcca, blocks, comp_x = 1, comp_y = 2, i_block = NULL){

  if ( is.null(i_block) )
    i_block = length(blocks)

  return(  data.frame(
    #correlation matrix within a block for each variables and each component selected
    sapply ( c(comp_x, comp_y), function(x) cor( blocks[[i_block]], rgcca$Y[[i_block]][, x] ) ) ,
    row.names = colnames(blocks[[i_block]])
  ))
}
