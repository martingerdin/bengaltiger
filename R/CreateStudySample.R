#' Create study sample
#'
#' Creates the study sample using a list of inclusion criteria. Note that the
#' selection process is stepwise in the order given by the inclusion criteria.
#' @param study.data Data frame. The study data. No defaults.
#' @param inclusion.criteria A list of functions. Each function should represent
#'     an inclusion criterion. No default.
#' @param complete.cases Logical vector of length 1. If TRUE only complete cases
#'     will be returned. If FALSE all cases are returned. Defaults to TRUE.
#' @param relevant.variables Character vector. The names of variables to keep in
#'     the study sample. Defaults to c("hos", "age", "sex", "tran", "doi",
#'     "toi", "doar", "toar", "dodd", "todd", "moi", "sbp_1", "hr_1", "rr_1",
#'     "gcs_t_1", "died", "head_and_neck", "face", "chest", "extremities",
#'     "external", "e_1_icd", "e_2_icd", "e_3_icd", "e_4_icd", "e_5_icd",
#'     "e_6_icd", "e_7_icd", "e_8_icd", "e_9_icd", "e_10_icd", "e_11_icd",
#'     "e_12_icd", "xray_1_icd", "xray_2_icd", "xray_3_icd", "xray_4_icd",
#'     "xray_5_icd", "xray_6_icd", "xray_7_icd", "xray_8_icd", "xray_9_icd",
#'     "xray_10_icd", "xray_11_icd", "fast_1_icd", "fast_2_icd", "fast_3_icd",
#'     "fast_4_icd", "fast_5_icd", "fast_6_icd", "fast_7_icd", "fast_8_icd",
#'     "fast_9_icd", "fast_10_icd", "fast_11_icd", "ct_1_icd", "ct_2_icd",
#'     "ct_3_icd", "ct_4_icd", "ct_5_icd", "ct_6_icd", "ct_7_icd", "ct_8_icd",
#'     "ct_9_icd", "ct_10_icd", "ct_11_icd", "ct_12_icd", "ct_13_icd",
#'     "op_1_icd", "op_2_icd", "op_3_icd", "op_4_icd", "op_5_icd", "op_6_icd",
#'     "op_7_icd", "op_8_icd", "op_9_icd", "op_10_icd", "op_11_icd").
#' @param add.to.relevant.variables Character vector. The names of variables to
#'     add to the default variables in relevant.variables. If NULL no variables
#'     are added. Defaults to NULL.
#' @param remove.from.relevant.variables Character vector. The names of
#'     variables to remove from the default variables in relevant.variables. If
#'     NULL no variables are removed. Defaults to NULL.
#' @param ignore.variables Character vector. The names of variables to ignore
#'     when complete cases are determined. The variables included in this vector
#'     must also be in relevant.variables. If NULL no variables are
#'     ignored. Defaults to c("head_and_neck", "face", "chest", "extremities",
#'     "external", "e_1_icd", "e_2_icd", "e_3_icd", "e_4_icd", "e_5_icd",
#'     "e_6_icd", "e_7_icd", "e_8_icd", "e_9_icd", "e_10_icd", "e_11_icd",
#'     "e_12_icd", "xray_1_icd", "xray_2_icd", "xray_3_icd", "xray_4_icd",
#'     "xray_5_icd", "xray_6_icd", "xray_7_icd", "xray_8_icd", "xray_9_icd",
#'     "xray_10_icd", "xray_11_icd", "fast_1_icd", "fast_2_icd", "fast_3_icd",
#'     "fast_4_icd", "fast_5_icd", "fast_6_icd", "fast_7_icd", "fast_8_icd",
#'     "fast_9_icd", "fast_10_icd", "fast_11_icd", "ct_1_icd", "ct_2_icd",
#'     "ct_3_icd", "ct_4_icd", "ct_5_icd", "ct_6_icd", "ct_7_icd", "ct_8_icd",
#'     "ct_9_icd", "ct_10_icd", "ct_11_icd", "ct_12_icd", "ct_13_icd",
#'     "op_1_icd", "op_2_icd", "op_3_icd", "op_4_icd", "op_5_icd", "op_6_icd",
#'     "op_7_icd", "op_8_icd", "op_9_icd", "op_10_icd", "op_11_icd").
#' @param save.to.disk Logical vector of length 1. If TRUE a file named
#'     "exclusions" is saved to disk where the exclusions are
#'     described. Defaults to TRUE.
#' @param file.format Character vector of length 1. Has to be either "docx" or
#'     "rmd". The format in which the file detailing the exclusions is
#'     saved. Defaults to "docx".
#' @param override Logical vector of length 1. If TRUE the file "exclusions" is
#'     replaced if it exists. If FALSE the function aborts if the file
#'     exists. Defaults to TRUE.
#' @export
CreateStudySample <- function(study.data, inclusion.criteria,
                              complete.cases = TRUE,
                              relevant.variables = c("hos", "age", "sex",
                                                     "tran", "doi", "toi",
                                                     "doar", "toar", "dodd",
                                                     "todd", "moi", "sbp_1",
                                                     "hr_1", "rr_1", "gcs_t_1",
                                                     "died", "head_and_neck",
                                                     "face", "chest",
                                                     "extremities", "external",
                                                     "e_1_icd", "e_2_icd",
                                                     "e_3_icd", "e_4_icd",
                                                     "e_5_icd", "e_6_icd",
                                                     "e_7_icd", "e_8_icd",
                                                     "e_9_icd", "e_10_icd",
                                                     "e_11_icd", "e_12_icd",
                                                     "xray_1_icd", "xray_2_icd",
                                                     "xray_3_icd", "xray_4_icd",
                                                     "xray_5_icd", "xray_6_icd",
                                                     "xray_7_icd", "xray_8_icd",
                                                     "xray_9_icd",
                                                     "xray_10_icd",
                                                     "xray_11_icd",
                                                     "fast_1_icd", "fast_2_icd",
                                                     "fast_3_icd", "fast_4_icd",
                                                     "fast_5_icd", "fast_6_icd",
                                                     "fast_7_icd", "fast_8_icd",
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
                              add.to.relevant.variables = NULL,
                              remove.from.relevant.variables = NULL,
                              ignore.variables = c("head_and_neck", "face",
                                                   "chest", "extremities",
                                                   "external", "e_1_icd",
                                                   "e_2_icd", "e_3_icd",
                                                   "e_4_icd", "e_5_icd",
                                                   "e_6_icd", "e_7_icd",
                                                   "e_8_icd", "e_9_icd",
                                                   "e_10_icd", "e_11_icd",
                                                   "e_12_icd", "xray_1_icd",
                                                   "xray_2_icd", "xray_3_icd",
                                                   "xray_4_icd", "xray_5_icd",
                                                   "xray_6_icd", "xray_7_icd",
                                                   "xray_8_icd", "xray_9_icd",
                                                   "xray_10_icd", "xray_11_icd",
                                                   "fast_1_icd", "fast_2_icd",
                                                   "fast_3_icd", "fast_4_icd",
                                                   "fast_5_icd", "fast_6_icd",
                                                   "fast_7_icd", "fast_8_icd",
                                                   "fast_9_icd", "fast_10_icd",
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
                              save.to.disk = TRUE,
                              file.format = "docx",
                              override = TRUE) {
    ## Error handling
    if (!is.data.frame(study.data))
        stop("study.data has to be a data frame")
    if (!all(sapply(inclusion.criteria, is.function)))
        stop("All items in inclusion.criteria have to be functions")
    if (!is.logical(complete.cases) | !IsLength1(complete.cases))
        stop("complete.cases has to be a logical vector of length 1")
    if (!is.character(relevant.variables))
        stop("relevant.variables has to be a character vector")
    if (!is.null & !is.character(add.to.relevant.variables))
        stop("add.to.relevant.variables has to be either NULL or a character vector")
    if (!is.null & !is.character(remove.from.relevant.variables))
        stop("remove.from.relevant.variables has to be either NULL or a character vector")
    if (!is.null & !is.character(ignore.variables))
        stop("ignore.variables has to be either NULL or a character vector")
    if (!is.logical(save.to.disk) | !IsLength1(save.to.disk))
        stop("save.to.disk has to be a logical vector of length 1")
    if (!(file.format %in% c("docx", "rmd")))
        stop("file.format has to be one of docx or rmd")
    if (!is.logical(override) | !IsLength1(override))
        stop("override has to be a logical vector of length 1")
    ## Create full file name
    full.file.name <- paste0("exclusions.", file.format)
    ## Use inclusion criteria to select sample from study data
    study.sample <- study.data
    for (i in seq_along(inclusion.criteria)) {
        ## Select the function to select a subset of patients from the inclusion
        ## criteria function list
        criterion.function <- inclusion.criteria[[i]]
        exclusion.list <- criterion.function(study.sample)
        study.sample <- exclusion.list$subsample
        ## Save exclusions to disk, but first check if the file already
        ## exists
        if (save.to.disk) {
            if (file.exists(full.file.name) & i == 1){
                if (!override)
                    stop(paste0(full.file.name, " already exists. The function has stopped. If you still want to run the function please delete the file or run this function again setting override to TRUE"))
                file.remove(full.file.name)
            }
            write(exclusion.list$exclusion.text, "exclusions.rmd", append = TRUE)
        }
    }
    ## Render exclusions file as docx
    if (save.to.disk & file.format == "docx"){
        rmarkdown::render("exclusions.rmd", output_format = "word_document")
        file.remove("exclusions.rmd")
    }
    ## Keep only relevant variables
    relevant.variables <- c(relevant.variables, add.to.relevant.variables)
    relevant.variables <- relevant.variables[!(relevant.variables %in% remove.from.relevant.variables)]
    study.sample <- study.sample[, relevant.variables]
    ## Keep only complete cases
    study.sample <- study.sample[complete.cases(study.sample[, !(colnames(study.sample) %in% ignore.variables)]), ]
    ## Return the new study sample
    return(study.sample)
}
