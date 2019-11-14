#' Plot the two components of a RGCCA
#'
#' Plot the two components of a RGCCA
#'
#' @param rgcca A list giving the results of a R/SGCCA
#' @param resp A vector of characters corresponding either to a qualitative
#' variable with levels or a continuous variable
#' @param comp_x An integer giving the index of the analysis component used
#' for the x-axis
#' @param comp_y An integer giving the index of the analysis component used
#' for the y-axis
#' @param i_block An integer giving the index of a list of blocks
#' @param text A bolean to represent the points with their row names (TRUE)
#' or with circles (FALSE)
#' @param i_block_y An integer giving the index of a list of blocks (another
#' one, different from the one used in i_block)
#' @param reponse_name A character giving the legend title
#' @param no_Overlap A boolean to avoid overlap in plotted text
#' @param predicted A list containing as  2nd element a matrix of predicted components 
#' @examples
#' coord = lapply(seq_len(3),
#'    function(x) matrix(runif(15 * 2, min = -1), 15, 2))
#' AVE_X = lapply(seq_len(3), function(x) runif(2))
#' for (i in 1:length(coord))
#' row.names(coord[[i]]) = seq(15)
#' rgcca.res = list(Y = coord, AVE = list(AVE_X = AVE_X))
#' # Using a superblock
#' resp = as.matrix(rep(LETTERS[seq_len(3)], each = 5))
#' row.names(resp) = seq(15)
#' plotSamplesSpace(rgcca.res, resp)
#' # Using the first block
#' resp = as.matrix(runif(15, min=-15, max = 15))
#' row.names(resp) = seq(15)
#' plotSamplesSpace(rgcca.res, resp, 1, 2, 1)
#' @export
plotSamplesSpace <- function(
    rgcca,
    resp = rep(1, NROW(rgcca$Y[[1]])),
    comp_x = 1,
    comp_y = 2,
    i_block = length(rgcca$Y),
    text = TRUE,
    i_block_y = i_block,
    reponse_name = "Response",
    no_Overlap = FALSE,
    predicted = NULL,
    cex = 1,
    subtitle_cex = 16 * cex,
    pch_text_cex = 3 * cex,
    axis_title_cex = 19 * cex
    ) {

    if (is.null(i_block_y))
        i_block_y <- i_block

    df <- getComponents(
        rgcca = rgcca,
        resp = resp,
        comp_x = comp_x,
        comp_y = comp_y,
        i_block = i_block,
        i_block_y = i_block_y,
        predicted = predicted
    )

    if (nrow(df) > 100)
        pch_text_cex <- 2

    if (!is.null(predicted))
            p <- ggplot(df, aes(df[, 1], df[, 2], color = df$resp))

    else if (length(unique(as.matrix(df$resp))) > 5 && 
            !unique(isCharacter(as.vector(df$resp))) ) {

        p <- ggplot(df, aes(df[, 1], df[, 2], color = df$resp))

    }else
        p <- NULL


    p <- plotSpace(
            rgcca,
            df,
            "Sample",
            df$resp,
            reponse_name,
            comp_x,
            comp_y,
            i_block,
            p,
            text,
            i_block_y,
            no_Overlap = no_Overlap,
            cex = cex,
            subtitle_cex = subtitle_cex,
            pch_text_cex = pch_text_cex,
            axis_title_cex = axis_title_cex
        )

    # remove legend if missing
    if (length(unique(df$resp)) == 1)
        p + theme(legend.position = "none")
    else
        p
}
