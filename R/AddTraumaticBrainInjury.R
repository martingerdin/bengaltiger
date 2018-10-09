#' Add traumatic brain injury
#'
#' Adds a new indicator variable to the study sample, defining what patients had
#' a traumatic brain injury (TBI).
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
#' @param levels Character vector of length 2. The levels to use to encode the
#'     resulting TBI variable. The first item in the vector should be the level
#'     to be used to represent a TBI. Defaults to c("Yes", "No").
#' @param variable.name Character vector of length 1. The name of the
#'     TBI. Defaults to "tbi".
#' @param add.as.factor Logical vector of length 1. If TRUE the TBI is added to
#'     the study sample as a factor. If FALSE it is added as character. Defaults
#'     to TRUE.
#' @param drop.used.variables Logical vector of length 1. If TRUE the ICD
#'     variables used to calculate TBI is dropped from the sample. Defaults to
#'     TRUE.
#' @export
AddTraumaticBrainInjury <- function(study.sample,
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
                                    levels = c("Yes", "No"),
                                    variable.name = "tbi",
                                    add.as.factor = TRUE,
                                    drop.used.variables = TRUE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data.frame")
    if (!is.character(icd.codes))
        stop("icd.codes has to be a character vector")
    if (!is.character(icd.variables))
        stop("icd.variables has to be a character vector")
    if (!is.character(levels) | length(levels) != 2)
        stop("levels has to be a character vector of length 2")
    if (!is.character(variable.name) | !IsLength1(variable.name))
        stop("variable.name has to be a character vector of length 1")
    if (!is.logical(add.as.factor) | !IsLength1(add.as.factor))
        stop("add.as.factor has to be a logical vector of length 1")
    if (!is.logical(drop.used.variables) | !IsLength1(drop.used.variables))
        stop("drop.used.variables has to be a logical vector of length 1")
    ## Identify patients with any of the codes listed in icd.codes in any of the
    ## icd.variables
    tbi.data.frame <- data.frame(lapply(study.sample[, icd.variables], function(column) {
        grepl(paste(icd.codes, collapse = "|"), column)
    }))
    tbi <- as.factor(rowSums(tbi.data.frame) > 0)
    ## Apply levels to the resulting variable
    levels(tbi) <- levels[c(2, 1)]
    ## Add to study sample
    if (!add.as.factor) tbi <- as.character(tbi)
    study.sample[, variable.name] <- tbi
    ## Drop used variables
    if (drop.used.variables)
        study.sample <- study.sample[, -grep(paste0(icd.variables, collapse = "|"), colnames(study.sample))]
    ## Return study sample
    return(study.sample)
}
