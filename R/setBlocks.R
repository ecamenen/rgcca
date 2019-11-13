#' Create a list of matrix from loading files corresponding to blocks
#'
#' @param file A character giving the path of a file used as a response
#' @param names A character giving a list of names for the blocks
#' @param sep A character giving the column separator
#' @param header A bolean giving the presence or the absence of the header
#' @param rownames An integer corresponding to the column number of the row
#' names (NULL otherwise)
#' @return A list matrix corresponding to the blocks
#' @examples
#' \dontrun{
#' setBlocks (TRUE,
#'     "data/agriculture.tsv,data/industry.tsv,data/politic.tsv",
#'     "agric,ind,polit")
#' }
#' @export
setBlocks <- function(file,
    names = NULL,
    sep = "\t",
    header = TRUE,
    rownames = 1) {

    # Parse args containing files path
    isXls <- (length(grep("xlsx?", file)) == 1)
    # test if extension filename is xls
    if (!isXls)
    # if it is not, parse the name of file from the arg list
    blocksFilename <- parseList(file)
    else {
        # # if xls, check file exists
        # checkFile(file)
        # # load the xls
        # wb = loadWorkbook(file)
        # # load the blocks
        # blocksFilename = names(getSheets(wb))
    }

    # Parse optional names of blocks
    if (!is.null(names))
    # default name is filename, otherwise, the user could name the blocs
    blocksName <- parseList(names)

    # Load each dataset
    blocks <- list()
    for (i in seq_len(length(blocksFilename))) {
        if (!isXls) {
            # if not an xls, file exist test is done here
            fi <- blocksFilename[i]
            checkFile(fi)
        }

        #Get names of blocs
        if (!is.null(names))
        # names of blocks are those parsed from args
        fo <- getFileName(blocksName[i])
        else {
            if (!isXls)
            # if not xls, the name is the files without the extension .tsv
            fo <- getFileName(fi)
            else
            # for xls, the names are those of the sheets
            fo <- blocksFilename[i]
        }

        #load the data
        if (!isXls) {
            checkFileSize(fi)
            df <- loadData(fi, sep, rownames, header)
        }
        # }else{
        #   checkFileSize(file)
        #   df = loadExcel(file, blocksFilename[i], rownames, header)
        # }

        #if one-column file, it is a tabulation error
        if (NCOL(df) == 0)
        stop(paste(fo, "block file has an only-column. Check the separator."),
        exit_code = 102)

        dimnames <- list(row.names(df), colnames(df))
        df <- convertMatrixNumeric(df)

        df <- imputeMean(df)

        checkQuantitative(df, fo, header)
        df <- matrix(as.numeric(df), nrow(df), ncol(df), dimnames = dimnames)
        blocks[[fo]] <- df
    }

    nrow <- lapply(blocks, NROW)

    if (length(blocks) > 1)
    blocks <- keepCommonRow(blocks)

    blocks <- removeColumnSdNull(blocks)

    for (i in seq_len(length(blocks)))
    attributes(blocks[[i]])$nrow <- nrow[[i]]

    if (nrow(blocks[[1]]) > 0)
    return(blocks)
    else
    stop("There is no rows in common between the blocks.", exit_code = 108)
}