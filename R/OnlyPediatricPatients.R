#' Only pediatric patients
#'
#' Keeps only the pediatric patients in the sample.
#' @param sample Data frame. The study sample. No default.
#' @param age.variable.name Character vector of length 1. The name of the age
#'     variable. Defaults to "age".
#' @param age.cutoff Numeric vector of length 1. The age cutoff. Defaults to 18,
#'     i.e. only observations with an age less than 18 are kept in the sample.
#' @param remove.missing Logical vector of length 1. If TRUE all observations
#'     with missing age, as detected by is.na, are removed from the
#'     sample. Defaults to TRUE.
#' @export
OnlyPediatricPatients <- function(sample, age.variable.name = "age", age.cutoff = 18, remove.missing = TRUE) {
    ## Error handling
    if (!is.data.frame(study.data))
        stop("sample has to be a data frame")
    if (!is.character(age.variable.name) | !IsLength1(age.variable.name))
        stop("age.variable.name has to be a character vector of length 1")
    if (!is.numeric(age.cutoff) | !IsLength1(age.cutoff))
        stop("age.cutoff has to be a numeric vector of length 1")
    if (!is.logical(remove.missing) | !IsLength1(remove.missing))
        stop("remove.missing has to be a logical vector of length 1")
    ## Make age numeric
    subsample <- sample
    subsample[, age.variable.name] <- as.numeric(subsample[, age.variable.name])
    ## Remove missing
    n.missing <- 0
    if (remove.missing) {
        subsample <- subsample[!is.na(subsample[, age.variable.name]), ]
        n.missing <- nrow(sample) - nrow(subsample)
    }
    ## Remove adults
    subsample <- subsample[subsample[, age.variable.name] < age.cutoff, ]
    n.excluded <- nrow(sample) - nrow(subsample) - n.missing
    ## Collate return list
    total.n.excluded <- n.excluded
    if (remove.missing)
        total.n.excluded <- total.n.excluded + n.missing
    exclusion.text <- paste0(total.n.excluded, " were adults.")
    if (remove.missing) {
        exclusion.text <- paste0(total.n.excluded, " excluded: \n",
                                 "- ", n.missing, " had missing age \n",
                                 "- ", n.excluded, " were adults (aged 18 years or above)")
    }
    return.list <- list(exclusion.text = exclusion.text,
                        subsample = subsample)
    return(return.list)
}
