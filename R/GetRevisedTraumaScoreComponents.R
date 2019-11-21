#' Get revised trauma score components
#'
#' Gets revised trauma score components
#' @param study.sample Data frame. The study sample. No default.
#' @param gcs.name Character vector of length 1. The name of the Glasgow coma
#'     scale variable. Defaults to "gcs_t_1".
#' @param sbp.name Character vector of length 1. The name of the systolic blood
#'     pressure variable. Defaults to "sbp_1".
#' @param rr.name Character vector of length 1. The name of the respiratory rate
#'     variable. Defaults to "rr_1".
#' @param return.labels Logical vector of length 1. If TRUE the labels rather
#'     than the values are returned. Defaults to FALSE.
#' @export
GetRevisedTraumaScoreComponents <- function(
                                            study.sample,
                                            gcs.name = "gcs_t_1",
                                            sbp.name = "sbp_1",
                                            rr.name = "rr_1",
                                            return.labels = FALSE
                                            )
{
    ## Error handling
    if (!is.data.frame(study.sample))
        stop ("study.sample has to be a data.frame")
    if (!is.character(gcs.name) | !IsLength1(gcs.name))
        stop ("gcs.name has to be a character vector of length 1")
    if (!is.character(sbp.name) | !IsLength1(sbp.name))
        stop ("sbp.name has to be a character vector of length 1")
    if (!is.character(rr.name) | !IsLength1(rr.name))
        stop ("rr.name has to be a character vector of length 1")    
    ## Get raw variable data
    gcs.raw <- study.sample[, gcs.name]
    sbp.raw <- study.sample[, sbp.name]
    rr.raw <- study.sample[, rr.name]
    ## Recode Glasgow coma scale
    gcs <- ifelse(gcs.raw > 12, 4,
           ifelse(gcs.raw > 8, 3,
           ifelse(gcs.raw > 5, 2,
           ifelse(gcs.raw > 3, 1, 0))))
    ## Recode systolic blood pressure
    sbp <- ifelse(sbp.raw > 89, 4,
           ifelse(sbp.raw > 75, 3,
           ifelse(sbp.raw > 49, 2,
           ifelse(sbp.raw > 0, 1, 0))))
    ## Recode respiratory rate
    rr <- ifelse(rr.raw > 29, 3,
          ifelse(rr.raw > 9, 4,
          ifelse(rr.raw > 5, 2,
          ifelse(rr.raw > 0, 1, 0))))
    ## Add value labels
    if (return.labels) {
        gcs <- factor(gcs, levels = c(0, 1, 2, 3, 4), labels = c("3", "4-5", "6-8", "9-12", "13-15"))
        sbp <- factor(sbp, levels = c(0, 1, 2, 3, 4), labels = c("0", "1-49", "50-75", "76-89", ">89"))
        rr <- factor(rr, levels = c(0, 1, 2, 3, 4), labels = c("0", "1-5", "6-9", ">29", "10-29"))
    }
    ## Create return object
    return.object <- list(gcs = gcs,
                          sbp = sbp,
                          rr = rr)
    return(return.object)
}
 
