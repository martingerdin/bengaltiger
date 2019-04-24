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
#' @param strata Character vector of length 1 or NULL. The name of the variable
#'     on which to stratify the bootstrap, i.e. do the bootstrap separately in
#'     each level of the strata variable and then combine the results. If NULL
#'     the bootstrap is not stratified. Defaults to NULL.
#' @param create.as.list Logical vector of length 1. If TRUE the bootstrap
#'     samples are returned or saved as a list. If FALSE the samples are
#'     returned as a data.frame. Defaults to TRUE.
#' @param return.samples Logical vector of length 1. If TRUE, the function
#'     returns bootstrap.samples to parent enviroment. Defaults to FALSE.
#' @param save.to.disk Logical vector of length 1. If TRUE, bootstrap.samples
#'     are saved to disk in RDS format. Defaults to TRUE.
#' @export
CreateBootstrapSamples <- function(study.sample, random.seed.already.set = FALSE,
                                   random.seed = NULL,
                                   number.of.bootstrap.samples = 1000,
                                   strata = NULL,
                                   create.as.list = TRUE,
                                   return.samples = FALSE,
                                   save.to.disk = TRUE) {
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
    if ((!is.character(strata) | !IsLength1(strata)) & !is.null(strata))
        stop ("strata has to be a character vector of length 1 or NULL")
    if (!is.logical(create.as.list) | !IsLength1(create.as.list))
        stop("create.as.list has to be a logical vector of length 1")    
    if (!is.logical(return.samples) | !IsLength1(return.samples))
        stop("return.samples has to be a logical vector of length 1")
    if (!is.logical(save.to.disk) | !IsLength1(save.to.disk))
        stop("save.to.disk has to be a logical vector of length 1")
    ## Stratify
    split.samples <- study.sample
    if (!is.null(strata)) {
        split.factor <- as.character(study.sample[, strata])
        split.factor[is.na(split.factor)] <- "::MISSING::"
        study.sample$split.factor <- as.factor(split.factor)
        split.samples <- split(study.sample, f = split.factor)
    }
    if (length(split.samples) == 1 | is.data.frame(split.samples)) 
        split.samples <- list(split.samples)
    ## Generate bootstrap samples
    bootstrap.samples <- lapply(split.samples, function(split.sample) {
        number.of.rows <- nrow(split.sample)
        stratified.bootstrap.samples <- lapply(1:number.of.bootstrap.samples, function(i) {
            row.index <- sample(1:number.of.rows, number.of.rows, replace = TRUE)
            stratified.bootstrap.sample <- split.sample[row.index, ]
            stratified.bootstrap.sample$.boot.id <- i
            return(stratified.bootstrap.sample)
        })
        ## Combine bootstrap samples
        bootstrap.sample <- do.call(rbind, stratified.bootstrap.samples)
        ## Return bootstrap samples
        return(bootstrap.sample)
    })
    ## Combine bootstrap samples
    bootstrap.samples <- do.call(rbind, bootstrap.samples)
    ## Order bootstraps based on .boot.id
    bootstrap.samples <- bootstrap.samples[order(bootstrap.samples$.boot.id), ]
    ## Remove split factor from combined dataset
    bootstrap.samples$split.factor <- NULL
    ## Make into a list and name bootstrap samples
    if (create.as.list) {
        bootstrap.samples <- split(bootstrap.samples, f = as.factor(bootstrap.samples$.boot.id))
        names(bootstrap.samples) <- paste0("bootstrap.sample.", 1:length(bootstrap.samples))    
    }
    ## Save bootstrap samples to disk
    if (save.to.disk) {
       saveRDS(bootstrap.samples, "bootstrap.samples.Rds")
       ## Let the user know that the samples are saved
       message("Bootstrap samples created and saved to disk")
    }
    ## Return bootstrap samples
    if (return.samples) 
       return(bootstrap.samples)
}
