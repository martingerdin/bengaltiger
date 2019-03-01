#' ICD variables
#'
#' Returns the names of ICD 10 variables in the TITCO dataset
#' @param subset Character vector. Must be one or more of "all", "external",
#'     "xray", "fast", "ct", "intraoperative". Defaults "all", which is equal
#'     to c("external", "xray", "fast", "ct", "intraoperative").
#' @examples
#' ICDVariables()
#' ICDVariables("external")
#' ICDVariables(c("xray", "ct"))
#' 
#' @export
ICDVariables <- function(subset = "all") {
    ## Error handling
    if (!is.character(subset))
        stop ("subset has to be a character vector")
    if (!(subset %in% c("all", "external", "xray", "fast", "ct", "intraoperative")))
        stop ("subset has to be one or more of all, external, xray, fast, ct, intraoperative")
    ## Define subsets
    subsets <- list(external = c("e_1_icd", "e_2_icd", "e_3_icd", "e_4_icd",
                                  "e_5_icd", "e_6_icd", "e_7_icd", "e_8_icd",
                                  "e_9_icd", "e_10_icd", "e_11_icd", "e_12_icd"),
                     xray = c("xray_1_icd", "xray_2_icd", "xray_3_icd",
                              "xray_4_icd", "xray_5_icd", "xray_6_icd",
                              "xray_7_icd", "xray_8_icd", "xray_9_icd",
                              "xray_10_icd", "xray_11_icd"),
                     fast = c("fast_1_icd", "fast_2_icd", "fast_3_icd",
                              "fast_4_icd", "fast_5_icd", "fast_6_icd",
                              "fast_7_icd", "fast_8_icd", "fast_9_icd",
                              "fast_10_icd", "fast_11_icd"),
                     ct = c("ct_1_icd", "ct_2_icd", "ct_3_icd", "ct_4_icd",
                            "ct_5_icd", "ct_6_icd", "ct_7_icd", "ct_8_icd",
                            "ct_9_icd", "ct_10_icd", "ct_11_icd", "ct_12_icd",
                            "ct_13_icd"),
                     intraoperative = c("op_1_icd", "op_2_icd", "op_3_icd",
                                        "op_4_icd", "op_5_icd", "op_6_icd",
                                        "op_7_icd", "op_8_icd", "op_9_icd",
                                        "op_10_icd", "op_11_icd"))
    ## Create return set
    if ("all" %in% subset) {
        return.set <- setNames(unlist(subsets), NULL)
    } else {
        return.set <- setNames(unlist(subsets[subset]), NULL)
    }
    ## Return return.set
    return(return.set)
}
