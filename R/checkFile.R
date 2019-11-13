checkFile <- function(f) {
    # Check the existence of a path f: A character giving the path of a file

    if (!file.exists(f))
    stop(paste0(f, " file does not exist."), exit_code = 101)

}