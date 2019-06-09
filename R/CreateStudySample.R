#' Create study sample
#'
#' Creates the study sample using a list of inclusion criteria. Note that the
#' selection process is stepwise in the order given by the inclusion criteria.
#' @param study.data Data frame. The study data. No defaults.
#' @param inclusion.criteria A list of functions. Each function should represent
#'     an inclusion criterion. No default.
#' @param complete.cases Logical vector of length 1. If TRUE only complete cases
#'     will be returned. If FALSE all cases are returned. Defaults to TRUE.
#' @param return.incomplete.cases Logical vector of length 1. Ignored if
#'     complete.cases is FALSE. If complete.cases is TRUE and this is TRUE a
#'     list is returned instead of a data.frame. The list has two entries. The
#'     first is a data.frame with only complete cases, called
#'     complete.sample. The second is a data.frame with only incomplete cases,
#'     called incomplete.sample. Defaults to FALSE.
#' @param relevant.variables Character vector. The names of variables to keep in
#'     the study sample. Defaults to c("hos", "sex", "tran", "doi", "toi",
#'     "doar", "toar", "dodd", "todd", "moi", "age", "sbp_1", "hr_1", "rr_1",
#'     "gcs_t_1", "iss", "died", "head_and_neck", "face", "chest",
#'     "extremities", "external", "e_1_icd", "e_2_icd", "e_3_icd", "e_4_icd",
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
#' @param save.to.results Logical vector of length 1. If TRUE the output is
#'     saved to a results file on disk. Defaults to TRUE.
#' @param results.name Character vector of length 1. The name of the analysis of
#'     missingness and exclusions saved to results. Defaults to
#'     "inclusions.and.exclusions".
#' @param save.to.disk Logical vector of length 1. If TRUE a file named
#'     "exclusions_and_missingness" is saved to disk where the exclusions and
#'     missingness are described. Defaults to FALSE.
#' @param file.format Character vector of length 1. Has to be either "docx" or
#'     "rmd". The format in which the file detailing the exclusions and
#'     missingness is saved. Defaults to "docx".
#' @param override Logical vector of length 1. If TRUE the file
#'     "exclusions_and_missingness" is replaced if it exists. If FALSE the
#'     function aborts if the file exists. Defaults to TRUE.
#' @export
CreateStudySample <- function(study.data, inclusion.criteria,
                              complete.cases = TRUE,
                              return.incomplete.cases = FALSE,
                              relevant.variables = c("hos", "sex", "tran",
                                                     "doi", "toi", "doar",
                                                     "toar", "dodd", "todd",
                                                     "moi", "age", "sbp_1",
                                                     "hr_1", "rr_1", "gcs_t_1",
                                                     "iss", "died",
                                                     "head_and_neck", "face",
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
                              save.to.results = TRUE,
                              results.name = "inclusions.and.exclusions",
                              save.to.disk = FALSE,
                              file.format = "docx",
                              override = TRUE) {
    ## Error handling
    if (!is.data.frame(study.data))
        stop("study.data has to be a data frame")
    if (!all(sapply(inclusion.criteria, is.function)))
        stop("All items in inclusion.criteria have to be functions")
    if (!is.logical(complete.cases) | !IsLength1(complete.cases))
        stop("complete.cases has to be a logical vector of length 1")
    if (!is.logical(return.incomplete.cases) | !IsLength1(return.incomplete.cases))
        stop("return.incomplete.cases has to be a logical vector of length 1")    
    if (!is.character(relevant.variables))
        stop("relevant.variables has to be a character vector")
    if (!is.null(add.to.relevant.variables) & !is.character(add.to.relevant.variables))
        stop("add.to.relevant.variables has to be either NULL or a character vector")
    if (!is.null(remove.from.relevant.variables) & !is.character(remove.from.relevant.variables))
        stop("remove.from.relevant.variables has to be either NULL or a character vector")
    if (!is.null(ignore.variables) & !is.character(ignore.variables))
        stop("ignore.variables has to be either NULL or a character vector")
    if (!is.logical(save.to.results) | !IsLength1(save.to.results))
        stop("save.to.results has to be a logical vector of length 1")
    if (!is.character(results.name) | !IsLength1(results.name))
        stop("results.name has to be a character vector of length 1")
    if (!is.logical(save.to.disk) | !IsLength1(save.to.disk))
        stop("save.to.disk has to be a logical vector of length 1")
    if (!(file.format %in% c("docx", "rmd")))
        stop("file.format has to be one of docx or rmd")
    if (!is.logical(override) | !IsLength1(override))
        stop("override has to be a logical vector of length 1")
    ## Create full file name
    file.name <- results.name
    full.file.name <- paste0(file.name, file.format)
    ## Use inclusion criteria to select sample from study data
    study.sample <- study.data
    ## Create inclusion and flowchart lists
    inclusion.list <- flowchart.list <- list()
    for (i in seq_along(inclusion.criteria)) {
        n.before.exclusion <- nrow(study.sample)
        ## Select the function to select a subset of patients from the inclusion
        ## criteria function list
        criterion.function <- inclusion.criteria[[i]]
        exclusion.list <- criterion.function(study.sample)
        study.sample <- exclusion.list$subsample
        ## Create inclusion text
        inclusion.text <- paste0("**Exclusions step ", i, "** \n\n",
                                 exclusion.list$exclusion.text, " \n\n",
                                 "*", nrow(study.sample),
                                 " patients remained.* \n\n")
        ## Define text for nodes after exclusion
        node.text <- list(paste0(nrow(study.sample),
                                 " patients remained"))
        ## If it is the first exclusion critera being applied then the
        ## number of patients in the cohort should be pasted above the text
        ## detailing the exclusions.
        if (i == 1) {
            inclusion.text <- paste0("**Before exclusions** \n\n",
                                     "There were ", n.before.exclusion,
                                     " patients in the cohort. \n\n",
                                     inclusion.text)
            flowchart.list <- c(list(paste0(n.before.exclusion,
                                            " patients in the cohort")),
                                flowchart.list)
        } else if (i == length(inclusion.criteria) & !complete.cases) {
            node.text <- list(paste0(nrow(study.sample), " patients were ",
                                     "included in the study sample"))
        }
        flowchart.list <- c(flowchart.list,
                            list(exclusion.list$exclusion.text),
                            node.text)
        ## Save number of patients remaining to flowchart list
        ## Add to inclusion list
        inclusion.list[[paste0("exclusion.", i)]] <- inclusion.text
        ## Save exclusions to disk, but first check if the file already
        ## exists
        if (save.to.disk) {
            if (file.exists(full.file.name) & i == 1){
                if (!override)
                    stop(paste0(full.file.name, " already exists. The function has stopped. If you still want to run the function please delete the file or run this function again setting override to TRUE"))
                file.remove(full.file.name)
            }
            write(inclusion.text, paste0(file.name, ".rmd"), append = TRUE)
        }
    }
    ## Keep only relevant variables
    relevant.variables <- c(relevant.variables, add.to.relevant.variables)
    relevant.variables <- relevant.variables[!(relevant.variables %in% remove.from.relevant.variables)]
    study.sample <- study.sample[, relevant.variables]
    ## Calculate missingness
    missingness.variables <- colnames(study.sample)[!(colnames(study.sample) %in% ignore.variables)]
    missingness.list <- lapply(missingness.variables, function(column.name) {
        column <- study.sample[, column.name]
        n.missing <- sum(is.na(column))
        p.missing <- round((n.missing/length(column)) * 100)
        string <- paste0(n.missing, " (", p.missing, "%) had missing in ", column.name)
        return(string)
    })
    missingness.string <- paste0(paste("-", unlist(missingness.list)), collapse = " \n\n")
    ## Keep only complete cases
    n.before.missing.excluded <- nrow(study.sample)
    complete.indices <- complete.cases(study.sample[, missingness.variables])
    complete.sample <- study.sample[complete.indices, ]
    incomplete.sample <- study.sample[!complete.indices, ]
    n.after.missing.excluded <- nrow(complete.sample)
    n.missing <- n.before.missing.excluded - n.after.missing.excluded
    p.missing <- round((n.missing/n.before.missing.excluded) * 100)
    missingness.handling.string <- paste0("A total of ", n.missing,
                                          " (", p.missing,
                                          "%) patients had missing ",
                                          "data in at least one variable")
    if (complete.cases) {
        study.sample <- complete.sample
        missingness.handling.string <- paste0("**Exclusions step ",
                                              length(inclusion.criteria) + 1,
                                              "** \n\n",
                                              missingness.handling.string,
                                              " and were therefore excluded")
        missingness.node.text <- flowchart.list[[length(flowchart.list)]] <- NULL
        ## Do not include the complete.cases exclusion node if data does not contain
        ## missing values
        if (n.missing != 0){
            flowchart.list[[length(flowchart.list) + 1]] <- node.text
            missingness.node.text <- list(paste0(n.missing,
                                                 " patients were excluded ",
                                                 "due to missing data"))  
        } 
        flowchart.list <- c(flowchart.list,
                            missingness.node.text,
                            list(paste0(nrow(study.sample), " patients were ",
                                        "included in the study sample")))
    } else {
        missingness.handling.string <- paste0("**Missingness** \n\n",
                                              missingness.handling.string)
    }
    missingness.string <- paste0("\n\n",
                                 missingness.handling.string, ": \n\n",
                                 missingness.string, " \n\n",
                                 "**Finally** \n\n",
                                 "The study sample included ",
                                 nrow(study.sample), " patients.")
    ## Add missingness string to inclusion list
    inclusion.list$missingness.string <- missingness.string
    ## Collapse inclusion list
    inclusions.and.exclusions <- paste0(unlist(inclusion.list), collapse = "\n")
    ## Save to results
    if (save.to.results){
        SaveToResults(inclusions.and.exclusions, results.name)
        ## Clean flowchart list before it is saved
        flowchart.list <- lapply(flowchart.list, function(element) {
            new.element <- sub("\\.$", "", element)
            return(new.element)
        })
        SaveToResults(flowchart.list, "flowchart.list")
    }
    ## Save to disk
    if (save.to.disk)
        write(missingness.string, paste0(file.name, ".rmd"), append = TRUE)
    ## Render exclusions and missingness file as docx
    if (save.to.disk & file.format == "docx"){
        rmarkdown::render(paste0(file.name, ".rmd"), output_format = "word_document")
        file.remove(paste0(file.name, ".rmd"))
    }
    ## Create return object
    return.object <- study.sample
    if (complete.cases & return.incomplete.cases)
        return.object <- list(complete.sample = study.sample,
                              incomplete.sample = incomplete.sample)
    return(return.object)
}
