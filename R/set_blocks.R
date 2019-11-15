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
#' set_blocks (TRUE,
#'     "data/agriculture.tsv,data/industry.tsv,data/politic.tsv",
#'     "agric,ind,polit")
#' }
#' @export
set_blocks <- function(file,
    names = NULL,
    sep = "\t",
    header = TRUE,
    rownames = 1) {

    # Parse args containing files path
    isXls <- (length(grep("xlsx?", file)) == 1)
    # test if extension filename is xls
    if (!isXls)
    # if it is not, parse the name of file from the arg list
        block_filenames <- cut_list(file)
    else {
        # # if xls, check file exists
        # check_file(file)
        # # load the xls
        # wb = loadWorkbook(file)
        # # load the blocks
        # block_filenames = names(getSheets(wb))
    }

    # Parse optional names of blocks
    if (!is.null(names))
        # default name is filename, otherwise, the user could name the blocs
        block_names <- cut_list(names)

    # Load each dataset
    blocks <- list()
    for (i in seq(length(block_filenames))) {
        if (!isXls) {
            # if not an xls, file exist test is done here
            fi <- block_filenames[i]
            check_file(fi)
        }

        #Get names of blocs
        if (!is.null(names))
            # names of blocks are those parsed from args
            fo <- get_filename(block_names[i])
        else {
            if (!isXls)
                # if not xls, the name is the files without the extension .tsv
                fo <- get_filename(fi)
            else
                # for xls, the names are those of the sheets
                fo <- block_filenames[i]
        }

        #load the data
        if (!isXls) {
            check_size(fi)
            df <- load_data(fi, sep, rownames, header)
        }
        # }else{
        #   check_size(file)
        #   df = loadExcel(file, block_filenames[i], rownames, header)
        # }

        #if one-column file, it is a tabulation error
        if (ncol(df) == 0)
            stop(paste(fo, "block file has an only-column. Check the separator."),
            exit_code = 102)

        dimnames <- list(row.names(df), colnames(df))
        df <- to_numeric(df)

        df <- impute_mean(df)

        check_quantitative(df, fo, header)
        df <- matrix(as.numeric(df), nrow(df), ncol(df), dimnames = dimnames)
        blocks[[fo]] <- df
    }

    nrow <- lapply(blocks, nrow)

    if (length(blocks) > 1)
        blocks <- common_rows(blocks)

    blocks <- remove_null_sd(blocks)

    for (i in seq(length(blocks)))
        attributes(blocks[[i]])$nrow <- nrow[[i]]

    if (nrow(blocks[[1]]) > 0)
        return(blocks)
    else
        stop("There is no rows in common between the blocks.", exit_code = 108)
}
