#' Source additional functions
#'
#' Sources additional functions for use within the project. These functions may
#' be user written and intended only for this specific projects, or they may be
#' functions that are yet to be added to the package.
#' @param path Character vector of length 1. The path to the directory that
#'     holds the additional functions. Defaults to "./misc/R/".
#' @export
SourceAdditionalFunctions <- function(path = "./misc/R/") {
    ## Error handling
    if (!is.character(path) | !IsLength1(path))
        stop ("path has to be a character vector of length 1")
    ## List files with additional functions
    function.files <- list.files(path = path,
                                 pattern = "^[A-Za-z0-9]*.R$")
    function.files.paths <- paste0(path, function.files)
    ## Source those files
    message.done <- "No additional functions were sourced"
    if (length(function.files) != 0) {
        invisible(lapply(function.files.paths, source))
        past.tense <- " were"
        if (length(function.files) == 1)
            past.tense <- " was"
        message.done <- paste0(paste0(function.files, collapse = ", "), past.tense, " sourced")
    }
    ## Report done
    message(message.done)
}
