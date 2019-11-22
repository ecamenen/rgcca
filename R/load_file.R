load_file <- function(
    file,
    file_text = file,
    sep = "\t",
    sheet = 1,
    rownames = 1,
    header = TRUE,
    response = FALSE) {

    lapply(c(file, file_text), check_size_file)

    isXls <- length(grep("xlsx?", file))

    # TODO: add automatic separator setting

    if (!isXls)
        load_file_text(file_text, sep, rownames, header, response)
    # else load_file_excel(file, 1, rownames, header = header, response = response)

}
