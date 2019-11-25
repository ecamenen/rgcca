load_file <- function(
    file,
    file_text = file,
    sep = "\t",
    sheet = 1,
    rownames = 1,
    header = TRUE,
    response = FALSE) {

    isXls <- length(grep("xlsx?", file))
    
    if (!isXls)
        file <- file_text

    check_file(file)
    check_size_file(file)


    # TODO: add automatic separator setting

    if (!isXls)
        load_file_text(file, sep, rownames, header, response)
    # else load_file_excel(file, 1, rownames, header = header, response = response)

}
