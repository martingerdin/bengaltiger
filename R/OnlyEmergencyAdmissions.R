#' Only emergency admissions
#'
#' Keeps only emergency admissions in the sample
#' @param study.sample Data frame. The study sample. No default.
#' @param admission.type.variable.name Character vector of length 1. The name of
#'     the admission type variable. Defaults to "ad_type".
#' @param emergency.value Character or numeric vector of length 1. The value of
#'     the admission type variable that indicates that an admission is an
#'     emergency admission. Defaults to "Emergency".
#' @param remove.missing Logical vector of length 1. If TRUE all observations
#'     with missing admission type, as detected by is.na, are removed from the
#'     sample. Defaults to TRUE.
#' @export
OnlyEmergencyAdmissions <- function(study.sample, admission.type.variable.name = "ad_type", emergency.value = "Emergency", remove.missing = TRUE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data frame")
    if (!is.character(admission.type.variable.name) | !IsLength1(admission.type.variable.name))
        stop("admission.type.variable.name has to be a character vector of length 1")
    if ((!is.numeric(emergency.value) & !is.character) | !IsLength1(emergency.value))
        stop("emergency.value has to be a character or numeric vector of length 1")
    if (!is.logical(remove.missing) | !IsLength1(remove.missing))
        stop("remove.missing has to be a logical vector of length 1")
    ## Create subsample
    subsample <- study.sample
    ## Remove missing
    n.missing <- 0
    if (remove.missing) {
        subsample <- subsample[!is.na(subsample[, admission.type.variable.name]), ]
        n.missing <- nrow(study.sample) - nrow(subsample)
    }
    ## Remove adults
    subsample <- subsample[subsample[, admission.type.variable.name] == emergency.value, ]
    n.excluded <- nrow(study.sample) - nrow(subsample) - n.missing
    ## Collate return list
    total.n.excluded <- n.excluded
    if (remove.missing)
        total.n.excluded <- total.n.excluded + n.missing
    exclusion.text <- paste0(total.n.excluded, " were elective admissions.")
    if (remove.missing) {
        exclusion.text <- paste0(total.n.excluded, " excluded: \n\n",
                                 "- ", n.missing, " had missing admission type \n\n",
                                 "- ", n.excluded, " were elective admissions \n\n")
    }
    return.list <- list(exclusion.text = exclusion.text,
                        subsample = subsample)
    return(return.list)
}
