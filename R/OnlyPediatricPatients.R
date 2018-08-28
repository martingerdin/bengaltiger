OnlyPediatricPatients <- function(sample, age.variable.name = "age", age.cutoff = 18, remove.missing = TRUE) {
    ## Error handling
    if (!is.data.frame(study.data))
        stop("sample has to be a data frame")
    if (!is.character(age.variable.name) | !IsLength1(age.variable.name))
        stop("age.variable.name has to be a character vector of length 1")
    if (!is.numeric(age.cutoff) | !IsLength1(age.cutoff))
        stop("age.cutoff has to be a numeric vector of length 1")
    if (!is.logical(remove.missing) | !IsLength1(remove.missing))
        stop("remove.missing has to be a logical vector of length 1")
    ## Make age numeric
    subsample <- sample
    subsample[, age.variable.name] <- as.numeric(subsample[, age.variable.name])
    ## Remove missing
    n.missing <- 0
    if (remove.missing) {
        subsample <- subsample[!is.na(subsample[, age.variable.name]), ]
        n.missing <- nrow(sample) - nrow(subsample)
    }
    ## Remove adults
    subsample <- subsample[subsample[, age.variable.name] < age.cutoff, ]
    n.excluded <- nrow(sample) - nrow(subsample) - n.missing
    ## Collate return list
    return.list <- list(exclusion.text = paste0(n.missing + n.excluded, " excluded: \n",
                                                "- ", n.missing, " had missing age \n",
                                                "- ", n.excluded, " were adults"),
                        subsample = subsample)
    return(return.list)
}
