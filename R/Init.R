#' Initiate a bengaltiger study
#'
#' Initiates a bengeltiger study by creating a standard directory structure and
#' study template.
#' @param create.study.template Logical vector of length 1. If TRUE a study
#'     template will be created in the current working directory. Defaults to
#'     TRUE.
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
Init <- function(create.study.template = TRUE, study.name = "My bengaltiger study",
                 authors = "Firstname Lastname",
                 description = "This is a bengaltiger study.",
                 file.name = "RunStudy", path = ".", create.directory = FALSE,
                 functions.to.include = c("ImportStudyData"),
                 save.as.function = TRUE, function.name = file.name,
                 open = TRUE) {
    ## Error handling
    if (!is.logical(create.study.template) | !IsLength1(create.study.template))
        stop("create.study.template must be a logical vector of length 1")
    ## Create directories
    directories <- c("data", "misc", "docs") 
    for (directory in directories) {
        if (!dir.exists(directory))
            dir.create(directory)
    }
    ## Create study template. Error handling of arguments below is done in
    ## CreateStudyTemplate.
    if (create.study.template)
        CreateStudyTemplate(study.name = study.name, authors = authors,
                            description = description, file.name = file.name,
                            path = path, create.directory = create.directory,
                            functions.to.include = functions.to.include,
                            save.as.function = save.as.function,
                            function.name = function.name, open = open)
    ## End function
    message("bengaltiger initiated")
}
