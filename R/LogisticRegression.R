#' Logistic regression
#'
#' Runs a logistic regression.
#' @param study.sample Data frame. The study sample. No default.
#' @param outcome.name Character vector of length 1. The name of the outcome
#'     variable. No default.
#' @param covariate.names Character vector. The names of the covariates. No
#'     default.
#' @param stop.if.missing Logical of length 1. If TRUE the execution stops if
#'     there is missing data in the outcome or covariates. Defaults to TRUE.
#' @param create.table Logical of length 1. If TRUE a table with the regression
#'     results is created and returned. The apperance of this table can be
#'     modified using table.options. If FALSE the model object is
#'     returned. Defaults to TRUE.
#' @param table.options List. Can only include "odds.ratio",
#'     "confidence.interval", "include.intercept", "include.p.value", and
#'     "digits". Valid values of "odds.ratio" are TRUE or FALSE. If FALSE the
#'     coefficients are included in the table instead of the odds
#'     ratios. Defaults to TRUE. "confidence.interval" has to be a value between
#'     0 and 1 and governs the width of the confidence interval. Defaults to
#'     0.95, i.e. a 95% confidence interval is presented. Valid values of
#'     "include.intercept" are TRUE or FALSE. If TRUE the model intercept is
#'     reported. Defaults to FALSE. Valid values of "include.p.value" are TRUE
#'     or FALSE. If TRUE the p-value is included in the table. Defaults to
#'     FALSE. Valid values of "digits" are any integer greater than 0.
#' @export
LogisticRegression <- function(study.sample, outcome.name, covariate.names,
                               stop.if.missing = TRUE, create.table = TRUE,
                               table.options = list(odds.ratio = TRUE,
                                                    confidence.interval = 0.95,
                                                    include.p.value = FALSE)) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.data has to be a data frame")
    if (!is.character(outcome.name) | !IsLength1(outcome.name))
        stop("outcome.name has to be a character vector of length 1")
    if (!is.character(covariate.names))
        stop("covariate.names has to be a character vector")
    if (!is.logical(stop.if.missing) | !IsLength1(stop.if.missing))
        stop("stop.if.missing has to be a logical vector of length 1")
    if (!is.logical(create.table) | !IsLength1(create.table))
        stop("create.table has to be a logical vector of length 1")
    if (!is.list(table.options))
        stop("table.options has to be a list")
    if (!any(names(table.options) %in% c("odds.ratio", "confidence.interval", "include.intercept", "include.p.value")))
        stop("table.options can only include odds.ratio, confidence.interval, include.intercept, and include.p.value")
    ## Get model outcome
    model.outcome <- study.sample[, outcome.name]
    ## Create model data
    model.data <- study.sample[, covariate.names]
    ## Check for missing
    if (any(is.na(cbind(model.outcome, model.data))) & stop.if.missing)
       stop(cat(paste0("There is missing data in your outcome or covariates and \n",
                    "stop.if.missing is set to TRUE. If you still want to run \n",
                    "the analysis with missing data and let R use its default \n",
                    "setting to remove this data please set stop.if.missing to \n",
                    "FALSE and run again. \n\n")))
    ## Build model
    model <- glm(model.outcome ~ ., data = model.data, family = binomial)
    return.object <- model
    ## Create table        
    if (create.table) {
        table.options$model <- model
        model.table <- do.call(CreateLogisticRegressionTable, table.options)
        return.object <- model.table
    }
    ## Return
    return(return.object)
}
