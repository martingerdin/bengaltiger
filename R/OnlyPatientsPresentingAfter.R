#' Only patients presenting later than a specific delay
#'
#' Keeps only the patients whom presented later than a specific delay, where
#' delay refers to the time between injury and arrival to hospital.
#'
#' @param study.sample Data frame. The study sample. No default.
#' @param delay.variable.name Character vector of length 1. The name of the
#'     delay variable. Defaults to "delay".
#' @param delay.cutoff Numeric vector of length 1. The delay cutoff in
#'     hours. Defaults to 6, i.e. only observations with an delay equal to or
#'     more than 6 hours are kept in the sample.
#' @param remove.missing Logical vector of length 1. If TRUE all observations
#'     with missing delay, as detected by is.na, are removed from the
#'     sample. Defaults to TRUE.
#' @export
OnlyPatientsPresentingAfter <- function(study.sample, delay.variable.name = "delay", delay.cutoff = 6, remove.missing = TRUE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data frame")
    if (!is.character(delay.variable.name) | !IsLength1(delay.variable.name))
        stop("delay.variable.name has to be a character vector of length 1")
    if (!is.numeric(delay.cutoff) | !IsLength1(delay.cutoff))
        stop("delay.cutoff has to be a numeric vector of length 1")
    if (!is.logical(remove.missing) | !IsLength1(remove.missing))
        stop("remove.missing has to be a logical vector of length 1")
    ## Make delay numeric
    subsample <- study.sample
    subsample[, delay.variable.name] <- as.numeric(subsample[, delay.variable.name])
    ## Remove missing
    n.missing <- 0
    if (remove.missing) {
        subsample <- subsample[!is.na(subsample[, delay.variable.name]), ]
        n.missing <- nrow(study.sample) - nrow(subsample)
    }
    ## Remove those with too long delay
    subsample <- subsample[subsample[, delay.variable.name] >= delay.cutoff, ]
    n.excluded <- nrow(study.sample) - nrow(subsample) - n.missing
    ## Collate return list
    total.n.excluded <- n.excluded
    if (remove.missing)
        total.n.excluded <- total.n.excluded + n.missing
    exclusion.text <- paste0(total.n.excluded, " had a delay of less than ", delay.cutoff, " hours.")
    if (remove.missing) {
        exclusion.text <- paste0(total.n.excluded, " excluded: \n\n",
                                 "- ", n.missing, " had missing delay \n\n",
                                 "- ", n.excluded, " had a delay of less than ", delay.cutoff, " hours. \n\n")
    }
    return.list <- list(exclusion.text = exclusion.text,
                        subsample = subsample)
    return(return.list)
}
