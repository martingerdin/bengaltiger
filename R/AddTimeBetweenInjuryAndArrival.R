#' Add time between injury and arrival
#'
#' Adds the variable time between injury and arrival to participating centre.
#' @param study.sample Data frame. The study sample. No default.
#' @param from.date Character or POSIXt vector of length 1. The name of the
#'     variable with the date from which to start counting. Defaults to "doi".
#' @param from.time Character or POSIXt vector of length 1. The name of the
#'     variable with the time from which to start counting. Defaults to "toi".
#' @param to.date Character or POSIXt vector of length 1. The name of the
#'     variable with the date from which to start counting. Defaults to "doar".
#' @param to.time Character or POSIXt vector of length 1. The name of the
#'     variable with the time from which to start counting. Defaults to "toar".
#' @param date.format Character vector of length 1. The date format. Defaults to
#'     "\%Y-\%m-\%d".
#' @param time.format Character vector of length 1. The date format. Defaults to
#'     "\%H:\%M".
#' @param units Character vector of length 1. Should be one of "days", "hours",
#'     "minutes", or "seconds". Defaults to "hours".
#' @param variable.name Character vector of length 1. The name of the time
#'     between injury and arrival variable. Defaults to "tbia".
#' @param drop.used.variables Logical vector of length 1. If TRUE the date and
#'     time variables used to calculate time between injury and arrival are
#'     dropped from the sample. Defaults to TRUE.
#' @export
AddTimeBetweenInjuryAndArrival <- function(study.sample, from.date = "doi",
                                           from.time = "toi", to.date = "doar",
                                           to.time = "toar",
                                           date.format = "%Y-%m-%d",
                                           time.format = "%H:%M",
                                           units = "hours",
                                           variable.name = "tbia",
                                           drop.used.variables = TRUE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop ("study.sample has to be a data.frame")
    if ((!is.character(from.date) & !is.numeric.POSIXt(from.date)) | !IsLength1(from.date))
        stop ("from.date has to be a character or POSIXt vector of length 1")
    if ((!is.character(from.time) & !is.numeric.POSIXt(from.time)) | !IsLength1(from.time))
        stop ("from.time has to be a character or POSIXt vector of length 1")
    if ((!is.character(to.date) & !is.numeric.POSIXt(to.date)) | !IsLength1(to.date))
        stop ("to.date has to be a character or POSIXt vector of length 1")
    if ((!is.character(to.time) & !is.numeric.POSIXt(to.time)) | !IsLength1(to.time))
        stop ("to.time has to be a character or POSIXt vector of length 1")
    if (!is.character(date.format) | !IsLength1(date.format))
        stop ("date.format has to be a character vector of length 1")
    if (!is.character(time.format) | !IsLength1(time.format))
        stop ("time.format has to be a character vector of length 1")
    if (!is.character(units) | !IsLength1(units))
        stop ("units has to be a character vector of length 1")
    if (!(units %in% c("hours", "days", "minutes", "seconds")))
        stop ("units has to be one of hours, days, minutes or seconds")
    if (!is.character(variable.name) | !IsLength1(variable.name))
        stop ("variable.name has to be a character vector of length 1")
    if (!is.logical(drop.used.variables) | !IsLength1(drop.used.variables))
        stop ("drop.used.variables has to be a logical vector of length 1")
    ## Merge date and time variables
    format <- paste(date.format, time.format)
    from <- as.POSIXct(strptime(paste0(study.sample[, from.date],
                                       study.sample[, from.time]),
                                format = format))
    to <- as.POSIXct(strptime(paste0(study.sample[, to.date],
                                     study.sample[, to.time]),
                              format = format))
    ## Calculate time between from and to
    tbia <- as.numeric(difftime(to, from, units = units))
    ## Add to study sample
    study.sample[, variable.name] <- tbia
    ## Drop date and time variables from study sample
    if (drop.used.variables)
        study.sample <- study.sample[, -grep(paste0(c(from.date, from.time, to.date, to.time), collapse = "|"), colnames(study.sample))]
    ## Return sample
    return(study.sample)
}
