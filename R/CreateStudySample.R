#' Create study sample
#'
#' Creates the study sample using a list of inclusion criteria. Note that the
#' selection process is stepwise in the order given by the inclusion criteria.
#' @param study.data Data frame. The study data. No defaults.
#' @param inclusion.criteria A list of functions. Each function should represent
#'     an inclusion criterion. No default.
#' @param complete.cases Logical vector of length 1. If TRUE only complete cases
#'     will be returned. If FALSE all cases are returned. Defaults to TRUE.
#' @param save.to.disk Logical vector of length 1. If TRUE a file named
#'     "exclusions" is saved to disk where the exclusions are
#'     described. Defaults to TRUE.
#' @param file.format Character vector of length 1. Has to be either "docx" or
#'     "rmd". The format in which the file detailing the exclusions is
#'     saved. Defaults to "docx".
#' @param override Logical vector of length 1. If TRUE the file "exclusions" is
#'     replaced if it exists. If FALSE the function aborts if the file
#'     exists. Defaults to TRUE.
#' @export
CreateStudySample <- function(study.data, inclusion.criteria,
                              complete.cases = TRUE, save.to.disk = TRUE,
                              file.format = "docx", override = TRUE) {
    ## Error handling
    if (!is.data.frame(study.data))
        stop("study.data has to be a data frame")
    if (!all(sapply(inclusion.criteria, is.function)))
        stop("All items in inclusion.criteria have to be functions")
    if (!is.logical(complete.cases) | !IsLength1(complete.cases))
        stop("complete.cases has to be a logical vector of length 1")
    if (!is.logical(save.to.disk) | !IsLength1(save.to.disk))
        stop("save.to.disk has to be a logical vector of length 1")
    if (!(file.format %in% c("docx", "rmd")))
        stop("file.format has to be one of docx or rmd")
    if (!is.logical(override) | !IsLength1(override))
        stop("override has to be a logical vector of length 1")
    ## Create full file name
    full.file.name <- paste0("exclusions", file.format)
    ## Use inclusion criteria to select sample from study data
    study.sample <- study.data
    for (i in seq_along(inclusion.criteria)) {
        criterion.function <- inclusion.criteria[i]
        exclusion.list <- criterion.function(study.sample)
        study.sample <- exclusion.list$subsample
        if (save.to.disk) {
            if (file.exists(full.file.name) & !override) 
                stop(paste0(full.file.name, " already exists. The function has stopped. If you still want to run the function please delete the file or run this function again setting override to TRUE"))
            write(exclusion.list$exclusion.text, "exclusions.rmd")
            if (file.format == "docx"){
                rmarkdown::render("exclusions.rmd", output_format = "docx")
                file.remove("exclusions.rmd")
            }
        }
    }
    return(study.sample)
}
