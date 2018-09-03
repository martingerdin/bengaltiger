#' Merge road traffic injury categories
#'
#' Merges road traffic injury categories into one
#' @param study.sample Data frame. The study sample. No default.
#' @param variable.name Character vector of length 1. The name of the mechanism
#'     of injury variable. Defaults to "moi".
#' @export
MergeRoadTrafficInjuryCategories <- function(study.sample,
                                             variable.name = "moi") {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data.frame")
    if (!is.character(variable.name) | !IsLength1(variable.name))
        stop("variable.name has to be a character vector of length 1")
    ## Merge categories
    moi <- study.sample[, variable.name]
    should.be.factor <- FALSE
    if (is.factor(moi)) {
        moi <- as.character(moi)
        should.be.factor <- TRUE
    }
    moi[grep("Road traffic injury", moi)] <- "Road traffic injury"
    if (should.be.factor)
        moi <- as.factor(moi)
    ## Put merged moi back into data
    study.sample[, variable.name] <- moi
    ## Return study sample
    return(study.sample)
}
