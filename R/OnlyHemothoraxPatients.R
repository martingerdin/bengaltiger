#' Only patients with hemothorax
#'
#' Keeps only patients with hemothorax.
#' @param study.sample Data frame. The study sample. No default.
#' @param hemothorax.value Character or numeric vector of length 1. The patterns
#'     to be used to define that a patient had a hemothorax. Defaults to
#'     "s27.1".
#' @param remove.missing Logical vector of length 1. If TRUE all observations
#'     with data on hemothorax diagnosis, as detected by is.na, are removed from
#'     the sample. Defaults to TRUE.
#' @export
OnlyHemothoraxPatients <- function(study.sample, hemothorax.value = "s27.1",
                                   remove.missing = TRUE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data frame")
    if ((!is.numeric(hemothorax.value) & !is.character(hemothorax.value)) | !IsLength1(hemothorax.value))
        stop("hemothorax.value has to be a character or numeric vector of length 1")
    if (!is.logical(remove.missing) | !IsLength1(remove.missing))
        stop("remove.missing has to be a logical vector of length 1")
    ## Create subsample
    subsample <- study.sample
    ## Add hemothorax indicator variable
    subsample <- AddSpecificInjuryVariable(study.sample = subsample, injury.codes = hemothorax.value, variable.name = "hemothorax")
    ## Remove missing
    n.missing <- 0
    if (remove.missing) {
        subsample <- subsample[!is.na(subsample[, "hemothorax"]), ]
        n.missing <- nrow(study.sample) - nrow(subsample)
    }
    ## Remove patients without hemothorax
    subsample <- subsample[subsample[, "hemothorax"] == "Yes", ]
    n.excluded <- nrow(study.sample) - nrow(subsample) - n.missing
    ## Collate return list
    total.n.excluded <- n.excluded
    if (remove.missing)
        total.n.excluded <- total.n.excluded + n.missing
    exclusion.text <- paste0(total.n.excluded, " patients were not diagnosed with hemothorax")
    if (remove.missing) {
        exclusion.text <- paste0(total.n.excluded, " excluded: \n\n",
                                 "- ", n.missing, " had missing information on hemothorax diagnosis \n\n",
                                 "- ", n.excluded, " patients were not diagnosed with hemothorax \n\n")
    }
    return.list <- list(exclusion.text = exclusion.text,
                        subsample = subsample)
    return(return.list)
}
