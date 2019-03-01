#' Add Triage Revised Trauma Score
#'
#' Add the Triage Revised Trauma Score to the sample
#' @param study.sample Data frame. The study sample. No default.
#' @param gcs.name Character vector of length 1. The name of the Glasgow coma
#'     scale variable. Defaults to "gcs_t_1".
#' @param sbp.name Character vector of length 1. The name of the systolic blood
#'     pressure variable. Defaults to "sbp_1".
#' @param rr.name Character vector of length 1. The name of the respiratory rate
#'     variable. Defaults to "rr_1".
#' @param variable.name Character vector of length 1. The name of the triage
#'     revised trauma score variable. Defaults to "trts".
#' @param drop.used.variables Logical vector of length 1. If TRUE the date and
#'     time variables used to calculate time between injury and arrival are
#'     dropped from the sample. Defaults to FALSE.
#' @export
AddTriageRevisedTraumaScore <- function(study.sample, gcs.name = "gcs_t_1",
                                        sbp.name = "sbp_1", rr.name = "rr_1",
                                        variable.name = "trts",
                                        drop.used.variables = FALSE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop ("study.sample has to be a data.frame")
    if (!is.character(gcs.name) | !IsLength1(gcs.name))
        stop ("gcs.name has to be a character vector of length 1")
    if (!is.character(sbp.name) | !IsLength1(sbp.name))
        stop ("sbp.name has to be a character vector of length 1")
    if (!is.character(rr.name) | !IsLength1(rr.name))
        stop ("rr.name has to be a character vector of length 1")
    if (!is.character(variable.name) | !IsLength1(variable.name))
        stop ("variable.name has to be a character vector of length 1")
    if (!is.logical(drop.used.variables) | !IsLength1(drop.used.variables))
        stop ("drop.used.variables has to be a logical vector of length 1")
    ## Get revised trauma score components
    rts.components <- GetRevisedTraumaScoreComponents(study.sample = study.sample,
                                                      gcs.name = gcs.name,
                                                      sbp.name = sbp.name,
                                                      rr.name = rr.name)
    ## Calculate score
    trts <- with(rts.components, gcs + sbp + rr)
    ## Add score to study sample
    study.sample[, variable.name] <- trts
    ## Drop used variables
    if (drop.used.variables)
        study.sample <- study.sample[, -grep(paste0(c(gcs.name, sbp.name, rr.name), collapse = "|"), colnames(study.sample))]
    ## Message that triage revised trauma score variable has been created
    message (paste0("The triage revised trauma score was added to the study sample as ", variable.name))
    ## Return study sample
    return(study.sample)
}    
