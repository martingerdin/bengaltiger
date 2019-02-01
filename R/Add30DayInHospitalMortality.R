#' Add 30-day in hospital mortality
#'
#' Adds the variable 30-day in hospital mortality to the study sample
#' @param study.sample Data frame. The study sample. No default.
#' @param from.date Character or POSIXt vector of length 1. The name of the
#'     variable with the date from which to start counting. Defaults to "doar".
#' @param from.time Character or POSIXt vector of length 1. The name of the
#'     variable with the time from which to start counting. Defaults to "toar".
#' @param to.date Character or POSIXt vector of length 1. The name of the
#'     variable with the date from which to start counting. Defaults to "dodd".
#' @param to.time Character or POSIXt vector of length 1. The name of the
#'     variable with the time from which to start counting. Defaults to "todd".
#' @param date.format Character vector of length 1. The date format. Defaults to
#'     "\%Y-\%m-\%d".
#' @param time.format Character vector of length 1. The date format. Defaults to
#'     "\%H:\%M".
#' @param died.variable Character vector of length 1. The name of the variable
#'     indicating whether the patient was dead at the end of follow up. Defaults
#'     to "died".
#' @param died.value Character vector of length 1. The value or level of the
#'     died variable that means that the patients died. Defaults to "Yes".
#' @param levels Character vector of length 2. The levels to use to encode the
#'     resulting 30-day in hospital mortality variable. The first item in the
#'     vector should be the level to be used to represent a death. Defaults to
#'     c("Yes", "No").
#' @param variable.name Character vector of length 1. The name of the 30-day in
#'     hospital mortality variable. Defaults to "m30d".
#' @param add.as.factor Logical vector of length 1. If TRUE the 30-day in
#'     hospital mortality variables is added to the study sample as a
#'     factor. If FALSE it is added as character. Defaults to TRUE.
#' @param drop.used.variables Logical vector of length 1. If TRUE the date and
#'     time variables used to calculate 30-day in hospital mortality is dropped
#'     from the sample. Defaults to TRUE.
#' @export
Add30DayInHospitalMortality <- function(study.sample, from.date = "doar",
                                        from.time = "toar", to.date = "dodd",
                                        to.time = "todd",
                                        date.format = "%Y-%m-%d",
                                        time.format = "%H:%M",
                                        died.variable = "died",
                                        died.value = "Yes",
                                        levels = c("Yes", "No"),
                                        variable.name = "m30d",
                                        add.as.factor = TRUE,
                                        drop.used.variables = TRUE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data.frame")
    if ((!is.character(from.date) & !is.numeric.POSIXt(from.date)) | !IsLength1(from.date))
        stop("from.date has to be a character or POSIXt vector of length 1")
    if ((!is.character(from.time) & !is.numeric.POSIXt(from.time)) | !IsLength1(from.time))
        stop("from.time has to be a character or POSIXt vector of length 1")
    if ((!is.character(to.date) & !is.numeric.POSIXt(to.date)) | !IsLength1(to.date))
        stop("to.date has to be a character or POSIXt vector of length 1")
    if ((!is.character(to.time) & !is.numeric.POSIXt(to.time)) | !IsLength1(to.time))
        stop("to.time has to be a character or POSIXt vector of length 1")
    if (!is.character(date.format) | !IsLength1(date.format))
        stop("date.format has to be a character vector of length 1")
    if (!is.character(time.format) | !IsLength1(time.format))
        stop("time.format has to be a character vector of length 1")
    if (!is.character(died.variable) | !IsLength1(died.variable))
        stop("died.variable has to be a character vector of length 1")
    if (!is.character(died.value) | !IsLength1(died.value))
        stop("died.value has to be a character vector of length 1")
    if (!is.character(levels) | length(levels) != 2)
        stop("levels has to be a character vector of length 2")
    if (!is.character(variable.name) | !IsLength1(variable.name))
        stop("variable.name has to be a character vector of length 1")
    if (!is.logical(add.as.factor) | !IsLength1(add.as.factor))
        stop("add.as.factor has to be a logical vector of length 1")
    if (!is.logical(drop.used.variables) | !IsLength1(drop.used.variables))
        stop("drop.used.variables has to be a logical vector of length 1")
    ## Merge date and time variables
    format <- paste(date.format, time.format)
    from <- as.POSIXct(strptime(paste0(study.sample[, from.date],
                                       study.sample[, from.time]),
                                format = format))
    to <- as.POSIXct(strptime(paste0(study.sample[, to.date],
                                     study.sample[, to.time]),
                              format = format))
    ## Calculate time between from and to
    time.between <- difftime(to, from, units = "days")
    ## Define 30 day in hospital mortality
    m30d <- as.factor(time.between <= 30 & study.sample[, died.variable] == died.value)
    levels(m30d) <- levels[c(2, 1)]
    ## Set class
    if (!add.as.factor)
        m30d <- as.character(m30d)
    ## Add to study sample
    study.sample[, variable.name] <- m30d
    ## Drop date and time variables from study sample
    if (drop.used.variables)
        study.sample <- study.sample[, -grep(paste0(c(from.date, from.time, to.date, to.time), collapse = "|"), colnames(study.sample))]
    ## Return sample
    return(study.sample)
}
