#' Only adolescents and young adults
#'
#' Keeps only the adolescents and young adults.
#' @param study.sample Data frame. The study sample. No default.
#' @param age.variable.name Character vector of length 1. The name of the age
#'     variable. Defaults to "age".
#' @param lower.age.cutoff Numeric vector of length 1. The lower age
#'     cutoff. Defaults to 10, i.e. only observations with an age of at least
#'     ten and at most upper.age.cutoff are kept in the sample.
#' @param upper.age.cutoff Numeric vector of length 1. The upper age
#'     cutoff. Defaults to 24, i.e. only observations with an age of at least
#'     lower.age.cutoff and at most 24 are kept in the sample.
#' @param remove.missing Logical vector of length 1. If TRUE all observations
#'     with missing age, as detected by is.na, are removed from the
#'     sample. Defaults to TRUE.
#' @export
OnlyAdolescentsAndYoungAdults <- function(study.sample, age.variable.name = "age",
                                          lower.age.cutoff = 10, upper.age.cutoff = 24,
                                          remove.missing = TRUE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data frame")
    if (!is.character(age.variable.name) | !IsLength1(age.variable.name))
        stop("age.variable.name has to be a character vector of length 1")
    if (!is.numeric(lower.age.cutoff) | !IsLength1(lower.age.cutoff))
        stop("lower.age.cutoff has to be a numeric vector of length 1")
    if (!is.numeric(upper.age.cutoff) | !IsLength1(upper.age.cutoff))
        stop("upper.age.cutoff has to be a numeric vector of length 1")    
    if (!is.logical(remove.missing) | !IsLength1(remove.missing))
        stop("remove.missing has to be a logical vector of length 1")
    ## Make age numeric
    subsample <- study.sample
    subsample[, age.variable.name] <- as.numeric(subsample[, age.variable.name])
    ## Remove missing
    n.missing <- 0
    if (remove.missing) {
        subsample <- subsample[!is.na(subsample[, age.variable.name]), ]
        n.missing <- nrow(study.sample) - nrow(subsample)
    }
    ## Remove adults
    subsample <- subsample[subsample[, age.variable.name] >= lower.age.cutoff & subsample[, age.variable.name] <= upper.age.cutoff, ]
    n.excluded <- nrow(study.sample) - nrow(subsample) - n.missing
    ## Collate return list
    total.n.excluded <- n.excluded
    if (remove.missing)
        total.n.excluded <- total.n.excluded + n.missing
    exclusion.text <- paste0(total.n.excluded, " were young children (age < ", lower.age.cutoff, "years) or adults (age > ", upper.age.cutoff, " years).")
    if (remove.missing) {
        exclusion.text <- paste0(total.n.excluded, " excluded: \n\n",
                                 "- ", n.missing, " had missing age \n\n",
                                 "- ", n.excluded, " were young children (age < ", lower.age.cutoff, "years) or adults (age > ", upper.age.cutoff, " years) \n\n")
    }
    return.list <- list(exclusion.text = exclusion.text,
                        subsample = subsample)
    return(return.list)
}
