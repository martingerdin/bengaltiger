#' Compile results
#'
#' Compiles the results saved to the results.Rds file and saves it to disk in
#' the format of your choice.
#' @param file.format Character vector of length 1. The file format in which to
#'     save the results. Must be one of "md", "pdf", or "docx". Defaults to
#'     "docx".
#' @param delete.results.file Logical vector of length 1. If TRUE the
#'     results.Rds file is deleted once its content has been compiled and
#'     saved. Defaults to TRUE.
#' @export
CompileResults <- function(file.format = "docx", delete.results.file = TRUE) {
    ## Load required packages
    library("rmarkdown")
    library("knitr")
    ## Error handling
    if (!(file.format %in% c("md", "pdf", "docx")))
        stop ("file.format has to be one of md, pdf or docx")
    if (!is.logical(delete.results.file) | !IsLength1(delete.results.file))
        stop ("delete.results.file has to be a logical vector of length 1")
    ## Read results
    results <- readRDS("results.Rds")
    ## Identify entries in results that are not character vectors or length 1
    IsInvalid <- function(result)
        !is.character(result) | !IsLength1(result)
    invalid.results <- sapply(results, IsInvalid)
    if (any(invalid.results))
        warning (paste0("There are elements in the results list that are not ",
                        "character vectors of length 1. These will not be ",
                        "included in the compiled document"))
    ## Delete invalid results from results list
    results <- results[!invalid.results]
    ## Paste results
    results.pasted <- paste0(paste0("## ", names(results), "\n", unlist(results)), collapse = "\n\n")
    ## Write to disk
    write(results.pasted, "results.Rmd")
    ## Format results file
    file.formats <- list("docx" = "word_document",
                         "pdf" = "pdf_document")
    if (file.format != "md")
        rmarkdown::render("results.Rmd", output_format = file.formats[[file.format]])
    ## Delete results.Rds
    if (delete.results.file)
        file.remove("results.Rds")
    ## Return
    message("Results compiled")
}
    
