#' Add AIS severity indicator
#'
#' Adds a variable to the data that indicates if a patient has an abbreviated
#' injury scale (AIS) severity above or equal to a certain cutoff in a certain
#' body region.
#' @param study.sample Data frame. The study sample. No default.
#' @param severity.variable.name Character vector of length 1. The name of the
#'     variable with AIS severity data. Defaults to "head_and_neck".
#' @param cutoff Numeric vector of length 1. The AIS severity cutoff. Has to be
#'     between 1 and 6. Defaults to 2.
#' @param levels Character vector of length 2. The levels to use to encode the
#'     resulting indicator variable. The first item in the vector should be the
#'     level to be used to indicate that the patient had an injury of the
#'     specified severity. Defaults to c("Yes", "No").
#' @param variable.name Character vector of length 1. The name of the resulting
#'     variable. Defaults to paste0(severity.variable.name, "geq", cutoff),
#'     where geq refers to the Latex command greater than or equal to.
#' @param add.as.factor Logical vector of length 1. If TRUE the indicator
#'     variable is added to the study sample as a factor. If FALSE it is added
#'     as character. Defaults to TRUE.
#' @param drop.used.variables Logical vector of length 1. If TRUE variables used
#'     to calculate the variable is dropped from the sample. Defaults to FALSE.
#' @export

AddAISSeverityIndicator <- function(study.sample,
                                    severity.variable.name = "head_and_neck",
                                    cutoff = 2,
                                    levels = c("Yes", "No"),
                                    variable.name = paste0(severity.variable.name, "geq", cutoff),
                                    add.as.factor = TRUE,
                                    drop.used.variables = FALSE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop ("study.sample has to be a data.frame")
    if (!is.character(severity.variable.name) | !IsLength1(severity.variable.name))
        stop ("severity.variable.name has to be a character vector of length 1")
    if (!is.numeric(cutoff) | !IsLength1(cutoff) | cutoff < 1 | cutoff > 6)
        stop ("cutoff has to be a numeric vector of length 1 between 1 and 6")
    if (!is.character(levels) | length(levels) != 2)
        stop("levels has to be a character vector of length 2")    
    if (!is.character(variable.name) | !IsLength1(variable.name))
        stop ("variable.name has to be a character vector of length 1")
    if (!is.logical(add.as.factor) | !IsLength1(add.as.factor))
        stop("add.as.factor has to be a logical vector of length 1")
    if (!is.logical(drop.used.variables) | !IsLength1(drop.used.variables))
        stop("drop.used.variables has to be a logical vector of length 1")    
    ## Get variable data
    variable.data <- as.character(study.sample[, severity.variable.name])
    ## Check if there are entries equal to the cutoff
    indicator.data <- as.factor(grepl(cutoff, variable.data))
    levels(indicator.data) <- levels[c(2, 1)]
    ## Set class
    if (!add.as.factor)
        indicator.data <- as.character(indicator.data)
    ## Add indicator variable to data
    study.sample[, variable.name] <- indicator.data
    ## Drop used variables
    if (drop.used.variables)
        study.sample <- study.sample[, -grep(severity.variable.name, colnames(study.sample))]
    ## Return study sample
    message(paste0(variable.name, " has been added to the data."))
    return(study.sample)
}
