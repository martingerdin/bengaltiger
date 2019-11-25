#' SplitDataset
#'
#' Splits the dataset into development, updating, and validation samples.
#' @param study.sample A data.frame. The study sample. No default.
#' @param events A numeric vector of the same length as the number of splits or
#'     NULL. Each item should indicate the number of events to be included in
#'     the resulting sample. If NULL split.proportions is used instead. Defaults
#'     to NULL.
#' @param event.variable.name A character vector of length 1 or NULL. The name
#'     of the variable that defines an event. Defaults to NULL.
#' @param event.level A character or numeric vector of length 1 or NULL. The
#'     level of event.variable that defines an event. Default to NULL.
#' @param split.proportions A numeric vector of the same length as the number of
#'     splits or NULL. Each item should indicate the proportion of the dataset
#'     that should be included in the resulting sample. If NULL events is used
#'     instead. Defaults to NULL.
#' @param temporal.split Not yet implemented.
#' @param remove.missing A logical vector of length 1. If TRUE observations with
#'     missing event data, as detected by is.na(), are removed from the sample
#'     and a warning is issued. If FALSE execution stops if there is missing
#'     event data.  Defaults to FALSE.
#' @param random.seed A numeric vector of length 1 or NULL. The random seed to
#'     be used when creating splits. Remember to set the seed outside this
#'     function if you are running other tasks that perform random
#'     operations. Defaults to NULL.
#' @param sample.names A character vector of the same length as events or
#'     split.proportions, depending on which is used, or NULL. If NULL the
#'     samples will be called "training" and "test" if two samples are to
#'     created, or "training", "validation", and "test" if three samples are to
#'     be created. Defaults to NULL.
#' @param return.data.frame A A logical vector of length 1. If TRUE a single
#'     data.frame is returned. This data.frame includes a new column called
#'     .sample which indicates what sample observations belong to. Defaults to
#'     FALSE.
#' @return A named list with three data frames or a single data frame with an
#'     added column indicating what sample observations belong to.
#' @export
SplitDataset <- function(study.sample, events = NULL,
                         event.variable.name = NULL, event.level = NULL,
                         split.proportions = NULL, temporal.split = NULL,
                         remove.missing = FALSE, random.seed = NULL,
                         sample.names = NULL,
                         return.data.frame = FALSE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop ("study.sample has to be a data.frame")
    if (!is.null(events) & (!is.numeric(events) | length(events) > 3))
        stop ("events has to be a numeric vector or NULL and can not have a length > 3")
    if (!is.null(event.variable.name) & (!is.character(event.variable.name) | !IsLength1(event.variable.name)))
        stop ("event.variable.name has to be a character vector of length 1 or NULL")
    if (!is.null(event.level) & (!is.character(event.level) & !is.numeric(event.level) | !IsLength1(event.level)))
        stop ("event.level has to be a character or numeric vector of length 1 or NULL")    
    if (!is.null(split.proportions) & (!is.numeric(split.proportions) | length(split.proportions) > 3 | sum(split.proportions) > 1))
        stop ("split.proportions has to be a numeric vector, can not have a length > 3, and can not sum to more than 1")
    if (is.null(events) & is.null(split.proportions))
        stop ("You need to set either events or split.proportions, both can not be NULL")
    if (!is.null(events) & !is.null(split.proportions))
        stop ("You cannot set both events and split.proportions, one has to be NULL")
    if (!is.null(temporal.split))
        stop ("temporal.split is not yet implemented")
    if (!is.logical(remove.missing) | !IsLength1(remove.missing))
        stop ("remove.missing has to be a logical vector of length 1")
    if (!any(sapply(list(events, event.variable.name, event.level), is.null)) & any(sapply(list(events, event.variable.name, event.level), is.null)))
        stop ("You need to set all of events, event.variable.name and event.level")
    if (!is.null(random.seed) & (!is.numeric(random.seed) | !IsLength1(random.seed)))
        stop ("random.seed has to be a numeric vector of length 1 or NULL")
    if (!is.null(random.seed))
        warning ("You have set random.seed. If you are running random operations other than this function then you should probably use set.seed elsewhere.")
    if (!is.null(sample.names) & (!is.character(sample.names) | (!is.null(events) & length(sample.names) != length(events)) | (!is.null(split.proportions) & length(sample.names) != length(split.proportions))))
        stop ("sample.names has to be a character vector of the same length as events or split.proportions, depending on which is used, or NULL")
    if (length(unique(sample.names)) != length(sample.names))
        stop ("All sample names has to be unique")
    if (any(is.na(sample.names)))
        stop ("sample.names cannot include NA")
    if (!is.logical(return.data.frame) | !IsLength1(return.data.frame))
        stop ("return.data.frame has to be a logical vector of length 1")    
    ## Set seed
    if (!is.null(random.seed))
        set.seed(random.seed)
    ## Name vectors
    selector <- split.proportions
    if (!is.null(events))
        selector <- events
    if (is.null(names(selector))) {
        if (length(selector) == 2)
            names(selector) <- c("training", "test")
        if (length(selector) == 3)
            names(selector) <- c("training", "validation", "test")
    }
    ## Order observations randomly
    if (is.null(temporal.split))
        sort.variable <- sample(1:nrow(study.sample), nrow(study.sample))
    study.sample <- study.sample[order(sort.variable), ]
    ## For some reason cbind.fill calls the new column object. Make a new object
    ## that contains the values of that column, if it exists
    object.column <- study.sample$object
    ## Split using events
    if (!is.null(events)) {
        ## Create event object
        event.variable <- study.sample[, event.variable.name] == event.level
        if (!remove.missing & any(is.na(event.variable)))
            stop (gsub('(.{1,90})(\\s|$)', '\\1\n',
                       paste0("There is missing data in the event variable. ",
                              "If you still want to run this function either ",
                              "make sure that there is no missing data in the ",
                              "study sample, or run this function with ",
                              "remove.missing = TRUE. That can be dangerous ",
                              "though so make sure you know what you are doing.")))
        ## Remove observations with missing data
        if (remove.missing) {
            n.na <- sum(is.na(event.variable))
            if (n.na > 0)
                warning (paste0(n.na, " observations had missing event data and were therefore removed."))
            study.sample <- study.sample[!is.na(event.variable), ]
        }
        event.variable <- event.variable[!is.na(event.variable)]
        ## Calculate proportion events and number of non-events
        proportion.events <- mean(event.variable)
        non.events <- ceiling(selector/proportion.events)
        names(non.events) <- names(selector)
        ## Stop if the total number of events and non-events exceed the number
        ## of observations in the study sample
        if (sum(selector, non.events) > nrow(study.sample))
            stop ("There is not enough observations in the study sample to allow this split.")
        ## Split data into events and non-events
        split.data.list <- split(study.sample, event.variable)
        ## Select observations to be included in each split
        sample.index.list <- lapply(list(selector, non.events), GetSampleIndex)
        split.data.list <- lapply(seq_along(split.data.list), function(i) {
            split.data <- split.data.list[[i]]
            index <- sample.index.list[[i]]
            new.split.data <- rowr::cbind.fill(split.data, index, fill = NA)
            return(new.split.data)
        })
        samples <- do.call(rbind, split.data.list)
    }
    ## Split using split.proportions
    if (!is.null(split.proportions)) {
        n.samples <- round(selector * nrow(study.sample))
        n.total <- sum(n.samples)
        ## Make sure the total number of observations in the samples equals the
        ## number of observations in the study.sample
        if (n.total != nrow(study.sample))
            n.samples[length(n.samples)] <- n.samples[length(n.samples)] - (n.total - nrow(study.sample))
        names(n.samples) <- names(selector)
        sample.index <- GetSampleIndex(n.samples)
        samples <- study.sample
        samples$object <- sample.index
    }
    ## Remove observations that do no belong to any sample
    n.to.remove <- sum(is.na(samples$object))
    if (n.to.remove > 0)
        message (paste0(n.to.remove), " observations were removed when samples were created")
    samples <- samples[!is.na(samples$object), ]
    ## Rename the object column to .sample
    if (any(names(samples) == ".sample"))
        stop ("There is already a column in the data called .sample. Please rename that column.")
    samples[, ".sample"] <- samples$object
    ## Put back the original contents of object to the column called object
    if (!is.null(object.column))
        samples$object <- object.column
    ## Create the return object
    return.object <- split(samples, samples[, ".sample"])
    if (return.data.frame)
        return.object <- samples
    return(return.object)
}

#' GetSampleIndex
#'
#' Internal function to get an index of observations to include in each sample
#' @param n.obs A named numeric vector. The number of observations that should
#'     be included in each sample. No default.
GetSampleIndex <- function(n.obs) {
    ## Error handling
    if (!is.numeric(n.obs))
        stop ("n.obs has to be a numeric vector")
    if (is.null(names(n.obs)))
        stop ("n.obs has to be named")
    ## Create sample index
    sample.index <- unlist(lapply(seq_along(n.obs), function(i) {
        name <- names(n.obs)[i]
        n <- n.obs[i]
        index.vector <- rep(name, n)
        return(index.vector)
    }))
    ## Return sample index    
    return(sample.index)
}
