#' Estimate in hospital mortality
#'
#' Estimates the proportion of patients who died in hospital with a bootstrap
#' confidence interval if requested.
#' @param study.sample Data frame. The study sample. No default.
#' @param variable.name Character vector of length 1. The name of the in
#'     hospital mortality variable. Defaults to "m24h".
#' @param died.level Character vector of length 1. The level of the in hospital
#'     mortality variable that indicates in hospital mortality. Defaults to
#'     "Yes".
#' @param digits Numeric vector of length 1. Must be a positive integer. The
#'     number of digits to use when rounding the proportion, and if applicable,
#'     the lower and upper bounds of the confidence interval. Defaults to 3.
#' @param bootstrap.confidence.interval Logical vector of length 1. If TRUE a
#'     confidence interval is estimated using an emperical bootstrap. Deafults
#'     to TRUE.
#' @param bootstrap.samples.exist Logical vector of length 1. If TRUE bootstrap
#'     samples are assumed to have been created using CreateBootstrapSamples,
#'     and are therefore read from the file bootstrap.samples.Rds. Defaults to
#'     FALSE.
#' @param random.seed.already.set Logical vector of length 1. If TRUE
#'     random.seed does not need to be set within this function as it indicates
#'     that this has been done (which is good practice) earlier in the
#'     code. Defaults to FALSE.
#' @param random.seed Numeric vector of length 1. Has to be an integer. The seed
#'     to use for random number generation. Only used if
#'     bootstrap.conficence.interval is TRUE and random.seed.already.set is
#'     FALSE. Defaults to NULL.
#' @param number.of.bootstrap.samples Numeric vector of length 1. Has to be a
#'     positive integer. The number of bootstrap samples to use. Only used it
#'     bootstrap.confidence.interval is TRUE. Defaults to 1000.
#' @param save.to.results Logical vector of length 1. If TRUE the table object
#'     is saved to a results file on disk using SaveToResults. Defaults to TRUE.
#' @param print.result Logical vector of length 1. If TRUE the result is
#'     printed so that you see what is saved to results. Defaults to TRUE.
#' @param return.result Logical vector of length 1. If TRUE the result is
#'     returned to the parent environment. Default to FALSE.
#' @export
EstimateInHospitalMortality <- function(study.sample,
                                        variable.name = "m24h",
                                        died.level = "Yes",
                                        digits = 3,
                                        bootstrap.confidence.interval = TRUE,
                                        bootstrap.samples.exist = FALSE,
                                        random.seed.already.set = FALSE,
                                        random.seed = NULL,
                                        number.of.bootstrap.samples = 1000,
                                        save.to.results = TRUE,
                                        print.result = TRUE,
                                        return.result = FALSE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data.frame")
    if (!is.character(variable.name) | !IsLength1(variable.name))
        stop("variable.name has to be a character vector of length 1")
    if (!is.character(died.level) | !IsLength1(died.level))
        stop("died.level has to be a character vector of length 1")
    if (!is.numeric(digits) | !IsLength1(digits) | digits < 0 | as.integer(digits) != digits)
        stop("digits has to be a positive integer")
    if (!is.logical(bootstrap.confidence.interval) | !IsLength1(bootstrap.confidence.interval))
        stop("bootstrap.confidence.interval has to be a logical vector of length 1")
    if (!is.logical(bootstrap.samples.exist) | !IsLength1(bootstrap.samples.exist))
        stop("bootstrap.samples.exist has to be a logical vector of length 1")    
    if (!is.logical(random.seed.already.set) | !IsLength1(random.seed.already.set))
        stop("random.seed.already.set has to be a logical vector of length 1")
    if (!is.null(random.seed))
        if (!is.numeric(random.seed) | !IsLength1(random.seed) | as.integer(random.seed) != random.seed)
            stop("random.seed has to be an integer")
    if (!is.numeric(number.of.bootstrap.samples) | !IsLength1(number.of.bootstrap.samples) | number.of.bootstrap.samples < 0 | as.integer(number.of.bootstrap.samples) != number.of.bootstrap.samples)
        stop("number.of.bootstrap.samples has to be a positive integer")
    if (!is.logical(save.to.results) | !IsLength1(save.to.results))
        stop("save.to.results has to be a logical vector of length 1")
    if (!is.logical(print.result) | !IsLength1(print.result))
        stop("print.result has to be a logical vector of length 1")    
    if (!is.logical(return.result) | !IsLength1(return.result))
        stop("return.result has to be a logical vector of length 1")
    ## Calculate proportion of in hospital mortality point estimate
    in.hospital.mortality <- study.sample[, variable.name]
    estimates <- list(point.estimate = mean(in.hospital.mortality == died.level))
    ## Estimate bootstrap confidence interval
    if (bootstrap.confidence.interval) {
        if (is.null(random.seed) & !random.seed.already.set & !bootstrap.samples.exist)
            stop("Please provide a random seed to estimate a bootstrap confidence interval")
        if (!is.null(random.seed) & random.seed.already.set)
            stop ("If a random seed has already been set you should not provide a new one. Run this function again but remove your random.seed argument or set random.seed.already.set to FALSE if that is really the case.")
        if (!is.null(random.seed))
            set.seed(random.seed)
        ## Get bootstrap samples
        if (bootstrap.samples.exist) {
            bootstrap.samples <- readRDS("bootstrap.samples.Rds")
            bootstrap.samples <- lapply(bootstrap.samples, function(sample) {
                return(sample[, variable.name])
            })
        } else {
            ## Get row indices to use to generate bootstrap samples
            row.indices <- lapply(1:number.of.bootstrap.samples, function(i) {
                sample(1:length(in.hospital.mortality), length(in.hospital.mortality), replace = TRUE)
            })
            ## Generate bootstrap samples
            bootstrap.samples <- lapply(row.indices, function(i) {
                in.hospital.mortality[i]
            })
        }
        ## Estimate bootstrap estimates
        bootstrap.estimates <- unlist(lapply(bootstrap.samples, function(in.hospital.mortality.sample) {
            mean(in.hospital.mortality.sample == died.level)
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
    ## Prepare result strings for known in-hospital mortality variable names
    prepared.strings <- list(m24h = "The 24-hour in hospital mortality was ",
                             m30d = "The 30-day in hospital mortality was ")

    ## Format result
    result.string <- "The in hospital mortality was "
    if (variable.name %in% names(prepared.strings))
        result.string <- prepared.strings[[variable.name]]
    content <- paste0(result.string, estimates$point.estimate)
    if (bootstrap.confidence.interval)
        content <- paste0(content, " (95% CI ", estimates$lower.bound, "-", estimates$upper.bound, ")")
    content <- paste0(content, ".")        
    ## Save to results
    if (save.to.results)
        SaveToResults(content, paste0(variable.name, ".in.hospital.mortality.estimate"))        
    ## Print result
    if (print.result)
        cat(content, "\n")
    ## Return result
    if (return.result)
        return(list(result = content,
                    estimates = estimates))
}
