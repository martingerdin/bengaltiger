#' Create logistic regression table
#'
#' Creates a table for reporting logistic regression results.
#' @param model.list List. The model objects on which to base the
#'     table. No default.
#' @param odds.ratio Logical vector of length 1. If TRUE odds ratios are
#'     reported in the table instead of coefficients. Defaults to TRUE.
#' @param confidence.interval Numeric vector of length 1. Has to be greater than
#'     0 and less than 1. The width of the confidence interval. Defaults to
#'     0.95.
#' @param include.intercept Logical vector of length 1. If TRUE the model
#'     intercept is reported. Defaults to FALSE.
#' @param include.p.value Logical vector of length 1. If TRUE the p-value is
#'     reported. Defaults to FALSE.
#' @param digits Numerical vector of length 1. Has to be an integer greater than
#'     0. The number of digits when reporting results. Defaults to 2.
#' @param save.table Logical vector of length 1. If TRUE the table is saved to
#'     the results file. Defaults to TRUE.
#' @param table.name Character vector of length 1 or NULL. The name of the table
#'     when saved. Only used if save.table is TRUE, in which case table.name
#'     cannot be NULL. Defaults to NULL.
#' @param verbose Logical vector of length 1. If TRUE progress is printed as the
#'     function runs. Useful for debugging. Defaults to FALSE.
#' @export
CreateLogisticRegressionTable <- function(model.list, odds.ratio = TRUE,
                                          confidence.interval = 0.95,
                                          include.intercept = FALSE,
                                          include.p.value = FALSE,
                                          digits = 2, save.table = TRUE,
                                          table.name = NULL,
                                          verbose = FALSE) {
    ## Load required packages
    library(knitr)
    ## Error handling
    if (!is.list(model.list))
        stop("model.list has to be a list")
    if (!is.logical(odds.ratio) | !IsLength1(odds.ratio))
        stop("odds.ratio has to be a logical vector of length 1")
    if (!is.numeric(confidence.interval) | !IsLength1(confidence.interval)| confidence.interval <= 0 | confidence.interval >= 1)
        stop("confidence.interval has to be a numeric vector of length 1. It has to be greater than 0 or less than 1.")
    if (!is.logical(include.p.value) | !IsLength1(include.p.value))
        stop("include.p.value has to be a logical vector of length 1")
    if (!is.logical(include.intercept) | !IsLength1(include.intercept))
        stop("include.intercept has to be a logical vector of length 1")
    if (!is.numeric(digits) | digits < 0 | !IsLength1(digits))
        stop("digits has to be an integer greater than 0")
    if ((!is.character(table.name) | !IsLength1(table.name)) & !is.null(table.name))
        stop("table.name has to be a character vector of length 1 or NULL")
    ## Create table component list
    table.components.list <- lapply(model.list, CreateLogisticRegressionSubTable,
                                    odds.ratio = odds.ratio,
                                    confidence.interval = confidence.interval,
                                    include.intercept = include.intercept,
                                    include.p.value = include.p.value,
                                    digits = digits,
                                    verbose = verbose)
    ## Create adjusted table
    table.draft <- table.components.list[[1]]
    ## Create unadjusted table and merge with adjusted
    if (length(table.components.list) > 1) {
        colnames(table.draft)[-1] <- paste0("Adjusted ", tolower(colnames(table.draft)[-1]))
        table.components.list[[1]] <- NULL
        unadjusted.table.draft <- do.call(rbind, table.components.list)
        colnames(unadjusted.table.draft)[-1] <- paste0("Unadjusted ", tolower(colnames(unadjusted.table.draft)[-1]))
        table.draft <- merge(as.data.frame(unadjusted.table.draft), as.data.frame(table.draft), by = "Covariate")
        colnames(table.draft) <- gsub("95% ci", "95% CI", colnames(table.draft))
    }
    ## Keep only adjusted intercept
    if (include.intercept & any(grepl("Unadjusted", colnames(table.draft)))) {
        intercept.rows <- grep("(Intercept)", table.draft$Covariate)
        table.draft[intercept.rows[1], 2] <- NA
        table.draft <- table.draft[-intercept.rows[2:length(intercept.rows)], ]
    }
    ## Format table
    formatted.table <- paste0(kable(table.draft), collapse = "\n")
    ## Save table
    if (save.table) {
        if (is.null(table.name))
            stop("table.name has to be a character vector if save.table is TRUE")
        SaveToResults(formatted.table, table.name)
    }
    ## Return table
    return(cat(formatted.table))
}
