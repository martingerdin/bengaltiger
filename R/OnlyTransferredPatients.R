#' Only transferred patients
#'
#' Keeps only patients who were transferred from another health facility in the
#' sample.
#' 
#' @param study.sample Data frame. The study sample. No default.
#' @param transfer.variable.name Character vector of length 1. The name of the
#'     transfer variable. Defaults to "tran".
#' @param transfer.value Character or numeric vector of length 1. The value of
#'     the transfer variable that indicates that a patients was
#'     transferred. Defaults to "Yes".
#' @param remove.missing Logical vector of length 1. If TRUE all observations
#'     with missing transfer, as detected by is.na, are removed from the
#'     sample. Defaults to TRUE.
#' @export
OnlyTransferredPatients <- function(study.sample, transfer.variable.name = "tran", transfer.value = "Yes", remove.missing = TRUE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data frame")
    if (!is.character(transfer.variable.name) | !IsLength1(transfer.variable.name))
        stop("transfer.variable.name has to be a character vector of length 1")
    if ((!is.numeric(transfer.value) & !is.character(transfer.value)) | !IsLength1(transfer.value))
        stop("transfer.value has to be a character or numeric vector of length 1")
    if (!is.logical(remove.missing) | !IsLength1(remove.missing))
        stop("remove.missing has to be a logical vector of length 1")
    ## Create subsample
    subsample <- study.sample
    ## Remove missing
    n.missing <- 0
    if (remove.missing) {
        subsample <- subsample[!is.na(subsample[, transfer.variable.name]), ]
        n.missing <- nrow(study.sample) - nrow(subsample)
    }
    ## Remove direct admissions
    subsample <- subsample[subsample[, transfer.variable.name] == transfer.value, ]
    n.excluded <- nrow(study.sample) - nrow(subsample) - n.missing
    ## Collate return list
    total.n.excluded <- n.excluded
    if (remove.missing)
        total.n.excluded <- total.n.excluded + n.missing
    exclusion.text <- paste0(total.n.excluded, " were direct admissions.")
    if (remove.missing) {
        exclusion.text <- paste0(total.n.excluded, " excluded: \n\n",
                                 "- ", n.missing, " had missing transfer status \n\n",
                                 "- ", n.excluded, " were direct admissions \n\n")
    }
    return.list <- list(exclusion.text = exclusion.text,
                        subsample = subsample)
    return(return.list)
}
