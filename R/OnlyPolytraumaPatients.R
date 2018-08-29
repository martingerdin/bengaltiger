#' Only polytrauma patients
#'
#' Keeps only the polytrauma patients. Polytrauma is here defined as at least
#' two injuries scored above a certain abbreviated injury scale (AIS) score
#' cutoff in at least two different body regions.
#' @param sample Data frame. The study sample. No default.
#' @param ais.variables Character vector. The names of the variables with AIS
#'     scores for each body region. Defaults to c("head_and_neck", "face",
#'     "chest", "extremities", "external").
#' @param ais.cutoff Integer vector of length 1 between 1 and 6. The cutoff
#'     above which an injury is counted for polytrauma. Defaults to 2.
#' @param remove.missing Logical vector of length 1. If TRUE all observations
#'     with missing AIS, as detected by is.na, are removed from the
#'     sample. Defaults to TRUE. 
#' @export
OnlyPolytraumaPatients <- function(sample,
                                   ais.variables = c("head_and_neck",
                                                     "face", "chest",
                                                     "extremities",
                                                     "external"),
                                   ais.cutoff = 2, remove.missing = TRUE) {
    ## Error handling
    if (!is.data.frame(study.data))
        stop("sample has to be a data frame")
    if (!is.character(ais.variables))
        stop("ais.variables has to be a character vector")
    if (!is.numeric(ais.cutoff) | !IsLength1(ais.cutoff))
        stop("ais.cutoff has to be an integer vector of length 1 between 1 and 6")
    if (!is.logical(remove.missing) | !IsLength1(remove.missing))
        stop("remove.missing has to be a logical vector of length 1")
    ## Remove missing
    n.missing <- 0
    subsample <- sample
    if (remove.missing) {
        subsample <- subsample[complete.cases(subsample[, ais.variables]), ]
        n.missing <- nrow(sample) - nrow(subsample)
    }
    ## Identify cases with AIS > ais.cutoff in ais.variables
    
    return.list <- list(exclusion.text = exclusion.text,
                        subsample = subsample)
    return(return.list)
}
