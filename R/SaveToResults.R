#' Save to results
#'
#' Saves the desired output to a results file in the current working
#' directory. Output saved in this file can later be compiled as a document.
#' @param output.object Any object. The output to be saved. Most often you want
#'     this to be a character vector of length 1, why you will see a warning if
#'     it is something else. No default.
#' @param object.name Character vector of length 1. The name of the output
#'     object in the results object. No default.
#' @param overwrite Logical vector of length 1. If TRUE any entry in the results
#'     object with the same name as object.name is overwritten. Defaults to
#'     TRUE.
#' @export
SaveToResults <- function(output.object, object.name, overwrite = TRUE) {
    ## Error handling
    if ((!is.character(output.object) | !IsLength1(output.object)))
        warning("output.object is not a character vector of length 1. Are you sure this is correct?")
    if (!is.character(object.name) | !IsLength1(object.name))
        stop("object.name has to be a character vector of length 1")
    if (!is.logical(overwrite) | !IsLength1(overwrite))
        stop("overwrite has to be a logical vector of length 1")
    ## Check if the results file exists in working directory, create it if it
    ## does not exist
    results <- suppressWarnings(tryCatch(readRDS("results.Rds"),
                        error = function(e) {
                            if (e$message == "cannot open the connection")
                                return(list())
                        }))
    ## Check if a result with that name already exists in .results
    object.exists <- object.name %in% names(results)
    if (object.exists & !overwrite)
        stop(cat(paste0("The object you are trying to save appears to already \n",
                        "exist in the results object. If you want to overwrite \n",
                        "a previously saved results please run this again but \n",
                        "set the option overwrite to TRUE \n\n")))
    ## Add object to results 
    results[[object.name]] <- output.object
    ## Save results to disk
    saveRDS(results, "results.Rds")
    ## Message
    message("Result saved")
}
