#' Create logistic regression table
#'
#' Creates a table for reporting logistic regression results.
#' @param model.object List of class glm. The model object on which to base the
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
CreateLogisticRegressionTable <- function(model.object, odds.ratio = TRUE,
                                          confidence.interval = 0.95,
                                          include.intercept = FALSE,
                                          include.p.value = FALSE,
                                          digits = 2, save.table = TRUE,
                                          table.name = NULL,
                                          verbose = FALSE) {
    ## Load required packages
    library(knitr)
    ## Error handling
    if (!is.list(model.object) | !("glm" %in% class(model.object)))
        stop("model.object has to be of class glm")
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
    table.components <- list()
    ## Extract parameters from model object
    table.components$coefficients <- coef(model.object)
    ## Generate confidence intervals
    table.components$confidence.intervals <- confint(model.object, level = confidence.interval)
    ## Make coefficients into odds ratios
    table.components <- lapply(table.components, exp)
    ## Extract p-values
    if (include.p.value)
        table.components$p.value <- summary(model.object)$coefficients[, "Pr(>|z|)"]
    ## Format figures
    if (verbose)
        print(paste0("digits = ", digits))
    fmt <- paste0("%.", digits, "f")
    reverse.sprintf <- function(x, fmt) sprintf(fmt, x)
    table.components <- lapply(table.components, reverse.sprintf, fmt = fmt)
    ## Merge coefficients and confidence intervals
    table.components$confidence.intervals <- matrix(table.components$confidence.intervals, ncol = 2)
    table.components$merged.parameters <-  with(table.components,
                                               paste0(coefficients,
                                                      " (",
                                                      confidence.intervals[, 1],
                                                      "-",
                                                      confidence.intervals[, 2],
                                                      ")"))
    table.components$coefficients <- NULL
    table.components$confidence.intervals <- NULL
    ## Create table draft
    table.draft <- do.call(cbind, table.components)
    ## Add names to table
    colnames(table.draft)[1] <- "Coefficient"
    if (odds.ratio)
        colnames(table.draft)[1] <- "Odds ratio"
    colnames(table.draft)[1] <- paste0(colnames(table.draft)[1], " (95% CI)")
    if (include.p.value)
        colnames(table.draft)[2] <- "P-value"
    table.draft <- cbind(names(coef(model.object)), table.draft)
    colnames(table.draft)[1] <- "Covariate"
    ## Remove intercept
    if (!include.intercept)
        table.draft <- table.draft[-grep("(Intercept)", table.draft[, 1]), ]
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
