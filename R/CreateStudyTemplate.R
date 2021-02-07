#' Create study template
#'
#' Creates a .R file with a custom name where all study steps are documented and
#' run.
#' @param study.name Character vector of length 1. The name of the
#'     study. Defaults to "My bengaltiger study".
#' @param authors Character vector. The names of the study authors. Defaults to
#'     c("Firstname Lastname").
#' @param description Character vector of length 1. Short description of the
#'     study. Defaults to "This is a bengaltiger study.".
#' @param file.name Character vector of length 1. The file name. Defaults to
#'     "RunStudy".
#' @param path Character vector of length 1. The path where the study template
#'     is saved. Defaults to ".", i.e. the current working directory.
#' @param create.directory Logical vector of length 1. If TRUE the directory to
#'     which path is pointing is created using dir.create(path) if it does not
#'     already exist. Defaults to FALSE.
#' @param functions.to.include Character vector. The names of the functions to
#'     include in the template. Defaults to c("ImportsStudyData").
#' @param save.as.function Logical vector of length 1. If TRUE the study
#'     template is structured as a R function. Defaults to TRUE.
#' @param function.name Character vector of length 1. The name of the study
#'     function. Is used only if as.functions = TRUE. Defaults to file.name.
#' @param open Logical vector of length 1. If TRUE the study template file is
#'     opened using R's file.edit(). Defaults to TRUE.
#' @export
CreateStudyTemplate <- function(study.name = "My bengaltiger study",
                                authors = "Firstname Lastname",
                                description = "This is a bengaltiger study.",
                                file.name = "RunStudy", path = ".",
                                create.directory = FALSE,
                                functions.to.include = c("ImportStudyData"),
                                save.as.function = TRUE,
                                function.name = file.name, open = TRUE) {
    ## Error handling
    if (!is.character(study.name) | !IsLength1(study.name))
        stop("study.name has to be a character vector of length 1")
    if (!is.character(authors))
        stop("authors has to be a character vector")
    if (!is.character(file.name) | !IsLength1(file.name))
        stop("file.name has to be a character vector of length 1")
    if (!is.character(path) | !IsLength1(path))
        stop("path has to be a character vector of length 1")
    if (!dir.exists(path) & !create.directory)
        stop("path does not exist. Choose a different path or set create.directory to TRUE.")
    if (!is.logical(create.directory) | !IsLength1(create.directory))
        stop("create.directory has to be a logical vector of length 1")
    if (!is.character(functions.to.include))
        stop("functions.to.include has to be a character vector")
    if (!is.logical(save.as.function) | !IsLength1(save.as.function))
        stop("save.as.function has to be a logical vector of length 1")
    if (!is.character(function.name) | !IsLength1(function.name))
        stop("function.name has to be a character vector of length 1")
    if (!is.logical(open) | !IsLength1(open))
        stop("open has to be a logical vector of length 1")
    ## Create file header
    header <- paste0("## ", study.name, " main R code file \n",
                     "## \n",
                     "## ", description, " \n",
                     "## \n",
                     "## This file was created using bengaltiger version ", sub("`|´", "", packageVersion("bengaltiger")), "\n")
    ## Add function index
    function.index <- setNames(sapply(functions.to.include, function(nm) paste0("study.data <- ", nm,  "()")),
                               nm = functions.to.include)
    ## Create file body
    body <- paste0(unlist(lapply(functions.to.include, function(x) {
        return(function.index[[x]])
    })), collapse = " \n")
    if (save.as.function) {
        body <- paste0(function.name, " <- function() { \n",
                       body, " \n",
                       "}")
    }
    ## Combine header and body
    file.content <- paste0(header, body)
    ## Save file
    full.file.name <- paste0(file.name, ".R")
    if (!dir.exists(path))
        dir.create(path)
    full.path <- paste0(path, "/", full.file.name)
    write(file.content, full.path)
    ## Open file
    if (open)
        file.edit(full.path)
    ## End function
    message("Study template created")
}
