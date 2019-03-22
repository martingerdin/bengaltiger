#' Add specific injury variable
#'
#' Adds a new indicator variable to the study sample, defining what patients had
#' the specific injury or injury pattern provided.
#' @param study.sample Data frame. The study sample. No default.
#' @param injury.codes Character vector. The patterns to be used to define the
#'     specific injury or injury pattern. If any of the codes is present in any
#'     of the variables listen in injury.variables an observation will be
#'     classified as having the injury. No default.
#' @param variable.name Character vector of length 1. The name of the
#'     variable. No default.
#' @param injury.variables Character vector. The names of the variables with
#'     injudy data. Defaults to c("e_1_icd", "e_2_icd", "e_3_icd", "e_4_icd",
#'     "e_5_icd", "e_6_icd", "e_7_icd", "e_8_icd", "e_9_icd", "e_10_icd",
#'     "e_11_icd", "e_12_icd", "xray_1_icd", "xray_2_icd", "xray_3_icd",
#'     "xray_4_icd", "xray_5_icd", "xray_6_icd", "xray_7_icd", "xray_8_icd",
#'     "xray_9_icd", "xray_10_icd", "xray_11_icd", "fast_1_icd", "fast_2_icd",
#'     "fast_3_icd", "fast_4_icd", "fast_5_icd", "fast_6_icd", "fast_7_icd",
#'     "fast_8_icd", "fast_9_icd", "fast_10_icd", "fast_11_icd", "ct_1_icd",
#'     "ct_2_icd", "ct_3_icd", "ct_4_icd", "ct_5_icd", "ct_6_icd", "ct_7_icd",
#'     "ct_8_icd", "ct_9_icd", "ct_10_icd", "ct_11_icd", "ct_12_icd",
#'     "ct_13_icd", "op_1_icd", "op_2_icd", "op_3_icd", "op_4_icd", "op_5_icd",
#'     "op_6_icd", "op_7_icd", "op_8_icd", "op_9_icd", "op_10_icd",
#'     "op_11_icd").
#' @param levels Character vector of length 2. The levels to use to encode the
#'     resulting injury variable. The first item in the vector should be the
#'     level to be used to represent having the injury. Defaults to c("Yes",
#'     "No").
#' @param add.as.factor Logical vector of length 1. If TRUE the variable is
#'     added to the study sample as a factor. If FALSE it is added as
#'     character. Defaults to TRUE.
#' @param drop.used.variables Logical vector of length 1. If TRUE the injury
#'     variables used to calculate the injury variable are dropped from the
#'     sample. Defaults to FALSE.
#' @export
AddSpecificInjuryVariable <- function(study.sample,
                                      injury.codes,
                                      variable.name,
                                      injury.variables = c("e_1_icd", "e_2_icd",
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
                                      add.as.factor = TRUE,
                                      drop.used.variables = TRUE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data.frame")
    if (!is.character(injury.codes))
        stop("injury.codes has to be a character vector")
    if (!is.character(variable.name) | !IsLength1(variable.name))
        stop("variable.name has to be a character vector of length 1")    
    if (!is.character(injury.variables))
        stop("injury.variables has to be a character vector")
    if (!is.character(levels) | length(levels) != 2)
        stop("levels has to be a character vector of length 2")
    if (!is.logical(add.as.factor) | !IsLength1(add.as.factor))
        stop("add.as.factor has to be a logical vector of length 1")
    if (!is.logical(drop.used.variables) | !IsLength1(drop.used.variables))
        stop("drop.used.variables has to be a logical vector of length 1")
    ## Identify patients with any of the codes listed in injury.codes in any of the
    ## injury.variables
    injury.data.frame <- data.frame(lapply(study.sample[, injury.variables], function(column) {
        grepl(paste(injury.codes, collapse = "|"), column)
    }))
    injury <- as.factor(rowSums(injury.data.frame) > 0)
    ## Apply levels to the resulting variable
    levels(injury) <- levels[c(2, 1)]
    ## Add to study sample
    if (!add.as.factor) injury <- as.character(injury)
    study.sample[, variable.name] <- injury
    ## Drop used variables
    if (drop.used.variables)
        study.sample <- study.sample[, -grep(paste0(injury.variables, collapse = "|"), colnames(study.sample))]
    ## Return study sample
    return(study.sample)
}
