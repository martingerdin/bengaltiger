#' Only isolated traumatic brain injury patients
#'
#' Keeps only the isolated traumatic brain injury (TBI) patients. Isolated
#' traumatic brain injury is here defined as having any of the international
#' classification of disease (ICD) codes listed in the parameter icd.codes below
#' and no abbreviated injury scale (AIS) score >1 in any other body region.
#' @param study.sample Data frame. The study sample. No default.
#' @param icd.codes Character vector. The codes to be used to define a TBI. If
#'     any of the codes is present in any of the variables listen in
#'     icd.variables an observation will be classified as having a TBI. Defaults
#'     to c("s02.0", "s02.1", "s02.7", "s02.8", "s02.9", "s04.0", "s04.1",
#'     "s04.2", "s04.3", "s04.4", "s04.5", "s04.6", "s04.7", "s04.8", "s04.9",
#'     "s06.0", "s06.1", "s06.2", "s06.3", "s06.4", "s06.5", "s06.6", "s06.7",
#'     "s06.8", "s06.9", "s07.0", "s07.1", "s07.8", "s07.9", "s09.7", "s09.8",
#'     "s09.9", "t02.0", "t04.0", "t06.0").
#' @param icd.variables Character vector. The names of the variables with
#'     international classification of disease (ICD) codes. Defaults to
#'     c("e_1_icd", "e_2_icd", "e_3_icd", "e_4_icd", "e_5_icd", "e_6_icd",
#'     "e_7_icd", "e_8_icd", "e_9_icd", "e_10_icd", "e_11_icd", "e_12_icd",
#'     "xray_1_icd", "xray_2_icd", "xray_3_icd", "xray_4_icd", "xray_5_icd",
#'     "xray_6_icd", "xray_7_icd", "xray_8_icd", "xray_9_icd", "xray_10_icd",
#'     "xray_11_icd", "fast_1_icd", "fast_2_icd", "fast_3_icd", "fast_4_icd",
#'     "fast_5_icd", "fast_6_icd", "fast_7_icd", "fast_8_icd", "fast_9_icd",
#'     "fast_10_icd", "fast_11_icd", "ct_1_icd", "ct_2_icd", "ct_3_icd",
#'     "ct_4_icd", "ct_5_icd", "ct_6_icd", "ct_7_icd", "ct_8_icd", "ct_9_icd",
#'     "ct_10_icd", "ct_11_icd", "ct_12_icd", "ct_13_icd", "op_1_icd",
#'     "op_2_icd", "op_3_icd", "op_4_icd", "op_5_icd", "op_6_icd", "op_7_icd",
#'     "op_8_icd", "op_9_icd", "op_10_icd", "op_11_icd").
#' @param ais.variables Character vector. The names of the variables with AIS
#'     scores for each body region. Defaults to c("head_and_neck", "face",
#'     "chest", "extremities", "external").
#' @param ais.cutoff Integer vector of length 1 between 1 and 6. The cutoff
#'     above which an injury should not be included as isolated TBI. Defaults to
#'     1.
#' @param remove.missing Logical vector of length 1. If TRUE all observations
#'     with missing AIS, as detected by is.na, are removed from the
#'     sample. Defaults to FALSE.
#' @export
OnlyIsolatedTraumaticBrainInjuryPatients <- function(study.sample,
                                                     icd.codes = c("s02.0", "s02.1", "s02.7",
                                                                   "s02.8", "s02.9", "s04.0",
                                                                   "s04.1", "s04.2", "s04.3",
                                                                   "s04.4", "s04.5", "s04.6",
                                                                   "s04.7", "s04.8", "s04.9",
                                                                   "s06.0", "s06.1", "s06.2",
                                                                   "s06.3", "s06.4", "s06.5",
                                                                   "s06.6", "s06.7", "s06.8",
                                                                   "s06.9", "s07.0", "s07.1",
                                                                   "s07.8", "s07.9", "s09.7",
                                                                   "s09.8", "s09.9", "t02.0",
                                                                   "t04.0", "t06.0"),
                                                     icd.variables = c("e_1_icd", "e_2_icd",
                                                                       "e_3_icd", "e_4_icd",
                                                                       "e_5_icd", "e_6_icd",
                                                                       "e_7_icd", "e_8_icd",
                                                                       "e_9_icd", "e_10_icd",
                                                                       "e_11_icd", "e_12_icd",
                                                                       "xray_1_icd",
                                                                       "xray_2_icd",
                                                                       "xray_3_icd",
                                                                       "xray_4_icd",
                                                                       "xray_5_icd",
                                                                       "xray_6_icd",
                                                                       "xray_7_icd",
                                                                       "xray_8_icd",
                                                                       "xray_9_icd",
                                                                       "xray_10_icd",
                                                                       "xray_11_icd",
                                                                       "fast_1_icd",
                                                                       "fast_2_icd",
                                                                       "fast_3_icd",
                                                                       "fast_4_icd",
                                                                       "fast_5_icd",
                                                                       "fast_6_icd",
                                                                       "fast_7_icd",
                                                                       "fast_8_icd",
                                                                       "fast_9_icd",
                                                                       "fast_10_icd",
                                                                       "fast_11_icd", "ct_1_icd",
                                                                       "ct_2_icd", "ct_3_icd",
                                                                       "ct_4_icd", "ct_5_icd",
                                                                       "ct_6_icd", "ct_7_icd",
                                                                       "ct_8_icd", "ct_9_icd",
                                                                       "ct_10_icd", "ct_11_icd",
                                                                       "ct_12_icd", "ct_13_icd",
                                                                       "op_1_icd", "op_2_icd",
                                                                       "op_3_icd", "op_4_icd",
                                                                       "op_5_icd", "op_6_icd",
                                                                       "op_7_icd", "op_8_icd",
                                                                       "op_9_icd", "op_10_icd",
                                                                       "op_11_icd"),
                                                     ais.variables = c("face", "chest",
                                                                       "extremities",
                                                                       "external"),
                                                     ais.cutoff = 1, 
                                                     remove.missing = FALSE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data frame")
    if (!is.character(icd.codes))
        stop("icd.codes has to be a character vector")
    if (!is.character(icd.variables))
        stop("icd.variables has to be a character vector")
    if (!is.character(ais.variables))
        stop("ais.variables has to be a character vector")
    if (!is.numeric(ais.cutoff) | !IsLength1(ais.cutoff))
        stop("ais.cutoff has to be an integer vector of length 1 between 1 and 6")
    if (!is.logical(remove.missing) | !IsLength1(remove.missing))
        stop("remove.missing has to be a logical vector of length 1")
    ## Identify patients with TBI
    subsample <- study.sample
    subsample <- AddTraumaticBrainInjury(study.sample = subsample,
                                         icd.codes = icd.codes,
                                         icd.variables = icd.variables,
                                         drop.used.variables = FALSE)
    subsample <- subsample[subsample$tbi == "Yes", ]
    subsample$tbi <- NULL
    ## Remove missing
    n.missing <- 0
    if (remove.missing) {
        subsample <- subsample[complete.cases(subsample[, ais.variables]), ]
        n.missing <- nrow(sample) - nrow(subsample)
    }
    ## Identify patients with isolated TBI
    invalid.ais.values <- (ais.cutoff + 1):6
    invalid.ais.string <- paste0(invalid.ais.values, collapse = "|")
    ais.above.cutoff.data <- do.call(cbind, lapply(subsample[, ais.variables], function(column) {
        new.column <- grepl(invalid.ais.string, column)
        return(new.column)
    }))
    polytrauma <- rowSums(ais.above.cutoff.data) > 1
    subsample <- subsample[!polytrauma, ]
    n.excluded <- nrow(study.sample) - nrow(subsample) + n.missing
    ## Collate return list
    total.n.excluded <- n.excluded + n.missing
    exclusion.text <- paste0(total.n.excluded,
                             " did not have isolated traumatic brain injury.")
    if (remove.missing) {
        exclusion.text <- paste0(total.n.excluded, " excluded: \n\n",
                                 "- ", n.missing, " had missing AIS scores \n\n",
                                 "- ", n.excluded, " did not have isolated traumatic brain injury \n\n")
    }
    return.list <- list(exclusion.text = exclusion.text,
                        subsample = subsample)
    return(return.list)
}
