#' Estimate traumatic brain injury proportion
#'
#' Estimates the proportion of patients with traumatic brain injury.
#' @param study.sample Data frame. The study sample. No default.
#' @param variable.name Character vector of length 1. The name of the traumatic
#'     brain injury variable. Defaults to "tbi".
#' @param tbi.level Character vector of length 1. The level of the traumatic
#'     brain injury variable that indicates a traumatic brain injury. Defaults
#'     to "Yes".
#' @param digits Numeric vector of length 1. Must be a positive integer. The
#'     number of digits to use when rounding the proportion, and if applicable,
#'     the lower and upper bounds of the confidence interval. Defaults to 3.
#' @param bootstrap.confidence.interval Logical vector of length 1. If TRUE a
#'     confidence interval is estimated using an emperical bootstrap. Deafults
#'     to TRUE.
#' @param random.seed Numeric vector of length 1. Has to be an integer. The seed
#'     to use for random number generation. Only used if
#'     bootstrap.conficence.interval is TRUE. Defaults to NULL.
#' @param number.of.bootstrap.samples Numeric vector of length 1. Has to be a
#'     positive integer. The number of bootstrap samples to use. Only used it
#'     bootstrap.confidence.interval is TRUE. Defaults to 1000.
#' @param save.to.disk Logical vector of length 1. If TRUE the result is saved
#'     to disk in "results.rmd". Defaults to TRUE.
#' @param return.result Logical vector of length 1. If TRUE the result is
#'     returned to the parent environment. Default to FALSE.
#' @export
EstimateTraumaticBrainInjuryProportion <- function(study.sample,
                                                   variable.name = "tbi",
                                                   tbi.level = "Yes",
                                                   digits = 3,
                                                   bootstrap.confidence.interval = TRUE,
                                                   random.seed = NULL,
                                                   number.of.bootstrap.samples = 1000,
                                                   save.to.disk = TRUE,
                                                   return.result = FALSE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data.frame")
    if (!is.character(variable.name) | !IsLength1(variable.name))
        stop("variable.name has to be a character vector of length 1")
    if (!is.character(tbi.level) | !IsLength1(tbi.level))
        stop("tbi.level has to be a character vector of length 1")
    if (!is.numeric(digits) | !IsLength1(digits) | digits < 0 | as.integer(digits) != digits)
        stop("digits has to be a positive integer")
    if (!is.logical(bootstrap.confidence.interval) | !IsLength1(bootstrap.confidence.interval))
        stop("bootstrap.confidence.interval has to be a logical vector of length 1")
    if (!is.null(random.seed))
        if (!is.numeric(random.seed) | !IsLength1(random.seed) | as.integer(random.seed) != random.seed)
            stop("random.seed has to be an integer")
    if (!is.numeric(number.of.bootstrap.samples) | !IsLength1(number.of.bootstrap.samples) | number.of.bootstrap.samples < 0 | as.integer(number.of.bootstrap.samples) != number.of.bootstrap.samples)
        stop("number.of.bootstrap.samples has to be a positive integer")
    if (!is.logical(save.to.disk) | !IsLength1(save.to.disk))
        stop("save.to.disk has to be a logical vector of length 1")
    if (!is.logical(return.result) | !IsLength1(return.result))
        stop("return.result has to be a logical vector of length 1")
    ## Calculate proportion of traumatic brain injury point estimate
    tbi <- study.sample[, variable.name]
    estimates <- list(point.estimate = mean(tbi == tbi.level))
    ## Estimate bootstrap confidence interval
    if (bootstrap.confidence.interval) {
        if (is.null(random.seed))
            stop("Please provide a random seed to estimate a bootstrap confidence interval")
        set.seed(random.seed)
        ## Get row indices to use to generate bootstrap samples
        row.indices <- lapply(1:number.of.bootstrap.samples, function(i) {
            sample(1:length(tbi), length(tbi), replace = TRUE)
        })
        ## Generate bootstrap samples
        bootstrap.samples <- lapply(row.indices, function(i) {
            tbi[i]
        })
        ## Estimate bootstrap estimates
        bootstrap.estimates <- unlist(lapply(bootstrap.samples, function(tbi.sample) {
            mean(tbi.sample == tbi.level)
        }))
        ## Use the emperical bootstrap method
        deviations <- estimates$point.estimate - bootstrap.estimates
        quantiles <- quantile(deviations, probs = c(0.975, 0.025))
        bounds <- estimates$point.estimate - quantiles
        estimates$lower.bound <- min(bounds)
        estimates$upper.bound <- max(bounds)
    }
    ## Round estimates
    estimates <- lapply(estimates, round, digits = digits)
    ## Save to disk
    if (save.to.disk){
        content <- paste0("The proportion of patients with TBI was ", estimates$point.estimate)
        if (bootstrap.confidence.interval)
            content <- paste0(content, " (95% CI ", estimates$lower.bound, "-", estimates$upper.bound, ")")
        content <- paste0(content, ".")
        write(content, "results.rmd")
    }
    ## Return proportion
    if (return.result)
        return(estimates)
}
