#' Create bootstrap samples
#'
#' Creates bootstrap samples and save them to disk, to use in functions that
#' rely on bootstraping to estimate uncertainty intervals or to come up with
#' other estimators, for example a linear shrinkage factor.
#' @param study.sample Data frame. The study sample. No default.
#' @param random.seed.already.set Logical vector of length 1. If TRUE
#'     random.seed does not need to be set within this function as it indicates
#'     that this has been done (which is good practice) earlier in the
#'     code. Defaults to FALSE.
#' @param random.seed Numeric vector of length 1. Has to be an integer. The seed
#'     to use for random number generation. Only used if random.seed.already.set
#'     is FALSE. Defaults to NULL.
#' @param number.of.bootstrap.samples Numeric vector of length 1. Has to be a
#'     positive integer. The number of bootstrap samples to create. Only used it
#'     bootstrap.confidence.interval is TRUE. Defaults to 1000.
#' @export
CreateBootstrapSamples <- function(study.sample, random.seed.already.set = FALSE,
                                   random.seed = NULL,
                                   number.of.bootstrap.samples = 1000) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data.frame")
    if (!is.logical(random.seed.already.set) | !IsLength1(random.seed.already.set))
        stop("random.seed.already.set has to be a logical vector of length 1")
    if (!is.null(random.seed))
        if (!is.numeric(random.seed) | !IsLength1(random.seed) | as.integer(random.seed) != random.seed)
            stop("random.seed has to be an integer")
    if (!is.numeric(number.of.bootstrap.samples) | !IsLength1(number.of.bootstrap.samples) | number.of.bootstrap.samples < 0 | as.integer(number.of.bootstrap.samples) != number.of.bootstrap.samples)
        stop("number.of.bootstrap.samples has to be a positive integer")    
    ## Get row indices to use to generate bootstrap samples
    number.of.rows <- nrow(study.sample)
    row.indices <- lapply(1:number.of.bootstrap.samples, function(i) {
        sample(1:number.of.rows, number.of.rows, replace = TRUE)
    })
    ## Generate bootstrap samples
    bootstrap.samples <- lapply(row.indices, function(i) {
        return(study.sample[i,])
    })
    ## Save bootstrap samples to disk
    saveRDS(bootstrap.samples, "bootstrap.samples.Rds")
    ## Let the user know that the samples are saved
    message("Bootstrap samples created and saved to disk")
}
    
