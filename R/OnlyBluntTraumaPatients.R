#' Only patients with blunt trauma
#'
#' Keeps only patients with blunt trauma in the sample.
#' @param study.sample Data frame. The study sample. No default.
#' @param injury.type.variable.name Character vector of length 1. The name of
#'     the age variable. Defaults to "ti".
#' @param blunt.value Character or numeric vector of length 1. The value of the
#'     injury type variable that indicates that a patients had blunt trauma.
#'     Defaults to "Blunt".
#' @param remove.missing Logical vector of length 1. If TRUE all observations
#'     with missing injury type, as detected by is.na, are removed from the
#'     sample. Defaults to TRUE.
#' @export
OnlyBluntTraumaPatients <- function(study.sample, injury.type.variable.name = "ti",
                                    blunt.value = "Blunt", remove.missing = TRUE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data frame")
    if (!is.character(injury.type.variable.name) | !IsLength1(injury.type.variable.name))
        stop("injury.type.variable.name has to be a character vector of length 1")
    if ((!is.numeric(blunt.value) & !is.character(blunt.value)) | !IsLength1(blunt.value))
        stop("blunt.value has to be a character or numeric vector of length 1")
    if (!is.logical(remove.missing) | !IsLength1(remove.missing))
        stop("remove.missing has to be a logical vector of length 1")
    ## Create subsample
    subsample <- study.sample
    ## Remove missing
    n.missing <- 0
    if (remove.missing) {
        subsample <- subsample[!is.na(subsample[, injury.type.variable.name]), ]
        n.missing <- nrow(study.sample) - nrow(subsample)
    }
    ## Remove patients with penetrating trauma
    subsample <- subsample[subsample[, injury.type.variable.name] == blunt.value, ]
    n.excluded <- nrow(study.sample) - nrow(subsample) - n.missing
    ## Collate return list
    total.n.excluded <- n.excluded
    if (remove.missing)
        total.n.excluded <- total.n.excluded + n.missing
    exclusion.text <- paste0(total.n.excluded, " patients had penetrating trauma")
    if (remove.missing) {
        exclusion.text <- paste0(total.n.excluded, " excluded: \n\n",
                                 "- ", n.missing, " had missing injury type \n\n",
                                 "- ", n.excluded, " patients had penetrating trauma \n\n")
    }
    return.list <- list(exclusion.text = exclusion.text,
                        subsample = subsample)
    return(return.list)
}
