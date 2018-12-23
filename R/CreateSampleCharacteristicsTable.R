#' Create sample characteristics table
#'
#' Creates the sample characteristics table. Wrapper of TableOne.
#' @param study.sample Data frame. The study sample. No default.
#' @param data.dictionary Not currently used.
#' @param group Character vector of length 1. The grouping variable. If NULL the
#'     table is not grouped. Defaults to NULL.
#' @param variables Character vector. The names of variables to include in the
#'     table. If NULL all variables in data.dictionary is included. Defaults to
#'     NULL.
#' @param exclude.variables Character vector. The names of variables to exclude
#'     from the table. If NULL no variables are excluded. Defaults to NULL.
#' @param include.overall Logical vector of length 1. If TRUE an overall column
#'     is included in the tables. Used only if group is not NULL. Defaults to
#'     TRUE.
#' @param include.missing Not currently used. Logical vector of length 1. If
#'     TRUE a column with the number (%) of missing values in each variable is
#'     included. Defaults to TRUE.
#' @param digits Numeric vector of length 1 greater than or equal to 0. Number
#'     of digits to use when rounding table entries. Defaults to 1.
#' @param save.to.results Logical vector of length 1. If TRUE the table object
#'     is saved to a results file on disk using SaveToResults. Defaults to TRUE.
#' @param save.to.disk Logical vector of length 1. If TRUE the table object is
#'     saved to disk. Defaults to FALSE.
#' @param file.format Character vector of length 1. The format in which to save
#'     the table to disk. Has to be one of c("pdf", "rmd", "docx"). Defaults to
#'     "docx".
#' @export
CreateSampleCharacteristicsTable <- function(study.sample,
                                             data.dictionary = NULL,
                                             group = NULL,
                                             variables = NULL,
                                             exclude.variables = NULL,
                                             include.overall = TRUE,
                                             include.missing = TRUE,
                                             digits = 1,
                                             save.to.results = TRUE,
                                             save.to.disk = FALSE,
                                             file.format = "docx") {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data.frame")
    if (!is.null(data.dictionary))
        stop("data.dictionary has to be NULL")
    if ((!is.character(group) & !is.null(group)) | !IsLength1(group))
        stop("group has to be a character vector of length 1 or NULL")
    if (!is.character(variables) & !is.null(variables))
        stop("variables has to be a character vector or NULL")
    if (!is.character(exclude.variables) & !is.null(exclude.variables))
        stop("exclude.variables has to be a character vector or NULL")
    if (!is.logical(include.overall) | !IsLength1(include.overall))
        stop("include.overall has to be a character vector of length 1")
    if (!is.logical(include.missing) | !IsLength1(include.missing))
        stop("include.missing has to be a character vector of length 1")
    if (!is.numeric(digits) | !IsLength1(digits) | digits < 0)
        stop("digits has to be a numeric vector of length 1")
    if (!is.logical(save.to.results) | !IsLength1(save.to.results))
        stop("save.to.results has to be a character vector of length 1")    
    if (!is.logical(save.to.disk) | !IsLength1(save.to.disk))
        stop("save.to.disk has to be a character vector of length 1")
    if (!(file.format %in% c("docx", "rmd", "pdf")) | !IsLength1(file.format))
        stop("file.format has to be one of docx, rmd, or pdf")
    ## Define variables
    if (is.null(variables)) variables <- colnames(study.sample)
    if (!is.null(exclude.variables)) variables <- variables[!(variables %in% exclude.variables)]
    if (!is.null(group)) 
        if (!(group %in% variables))
            stop("group has to be one of the variables to be in the table")
    ## Define table data
    table.data <- study.sample[, variables]
    ## Make a list that will hold the individual tables
    table.list <- list()
    ## Create the grouped table if there should be one
    if (!is.null(group)) {
        ## Remove the group variable from the list of variables to be put in the
        ## table
        variables <- variables[!(variables %in% group)]
        ## Create the grouped table
        table.list$grouped.table <- CreateTableOne(vars = variables,
                                                   strata = group,
                                                   data = table.data,
                                                   test = FALSE) 
    }
    ## Create the overall table if there should be one
    if (is.null(group) | include.overall) table.list$overall.table <- CreateTableOne(vars = variables, data = table.data)
    ## Define variables to be treated as non-normally distributed, i.e. so that
    ## they are reported using medians and IQR
    nonormal.variables <- sapply(table.data, is.numeric)
    ## Format the tables in table.list
    formatted.tables <- lapply(table.list, print,
                               nonnormal = variables[nonormal.variables],
                               noSpaces = TRUE,
                               catDigits = digits,
                               contDigits = digits,
                               showAllLevels = TRUE,
                               printToggle = FALSE)
    ## Combine the formatted tables into one
    table <- do.call(cbind, formatted.tables)
    ## Remove duplicate level columns
    level.indices <- grep("level", colnames(table)) # Find the indices of columns named level
    if (length(level.indices) > 1) table <- table[, -level.indices[2]] # Remove the second level column
    ## Rename level column
    colnames(table)[1] <- "Level"
    ## Modify the first table row with n to also include percentages
    if (!is.null(group)) {
        ni <- grep("^n$", rownames(table)) # Get index of row with n
        nnum <- as.numeric(table[ni, ]) # Make numeric
        ps <- round(nnum/nrow(table.data) * 100, digits = digits) # Estimate percentages
        fmt <- paste0("%.", digits, "f") # Generate format based on number of digits
        nn <- paste0(nnum, " (", sprintf(fmt, ps), ")") # Format numbers with percentages
        table[ni, ] <- nn # Put back in table
        rownames(table)[ni] <- "n (%)" # Modify name of n row
        table["n (%)", "Level"] <- ""
    }
    ## The code below is currently not implemented
    ##
    ## ## Replace variable names with labels
    ## nrns <- orns <- rownames(table) # Get current rownames
    ## abbr <- list() # Genderate vector to hold abbreviations
    ## for (x in variables) {
    ##     vdd <- data_dictionary[[x]] # Get variable specific data dictionary
    ##     l <- vdd$al # Get abbreviated as label
    ##     if (l == "") l <- vdd$l else abbr[[x]] <- paste0(vdd$al, ", ", vdd$l) # If there is no abbreviated label get full label, else store full label to use in explanatory note
    ##     i <- grep(paste0("^", x), rownames(table)) # Get position of old rowname
    ##     nrns[i] <- sub(paste0("^", x), l, rownames(table)[i]) # Put new rowname there
    ## }
    ## table <- cbind(nrns, table) # Add rownames as column
    ## colnames(table)[1] <- "Characteristic" # Name that column
    ## ## Add missing values column
    ## if (include_missing_column) {
    ##     missing_column <- rep("", nrow(table))
    ##     for (variable in variables) {
    ##         missing_variable <- grep(paste0(variable, "_missing"), colnames(full_table.data), value = TRUE)
    ##         missing_entry <- "0 (0)"
    ##         if (length(missing_variable) != 0) {
    ##             missing_data <- full_table.data[, missing_variable]
    ##             n_missing <- sum(missing_data == 0)
    ##             p_missing <- round(100 - mean(missing_data) * 100, digits = digits)
    ##             missing_entry <- paste0(n_missing, " (", p_missing, ")")
    ##         }
    ##         index <- grep(paste0(variable, " "), rownames(table))
    ##         missing_column[index] <- missing_entry
    ##     }
    ##     missing_cols <- full_table.data[, grep("_missing", colnames(full_table.data))]
    ##     total_n_missing <- nrow(full_table.data) - nrow(missing_cols[-unique(which(missing_cols == 0, arr.ind = TRUE)[, 1]), ])
    ##     total_p_missing <- round(total_n_missing/nrow(full_table.data) * 100, digits = digits)
    ##     missing_column[1] <- paste0(total_n_missing, " (", total_p_missing, ")*")
    ##     missing_column <- matrix(missing_column, ncol = 1)
    ##     colnames(missing_column) <- "Missing values, n (%)"
    ##     rownames(missing_column) <- NULL
    ##     rownames(table) <- NULL
    ##     table <- cbind(table, missing_column)
    ## }
    ## rownames(table) <- NULL # Remove rownames
    ## ## Save raw table object
    ## tables <- list(raw = table)
    ## rownames(tables$raw) <- orns # Add old rownames back, for easy access
    ## ## Make abbreviations and explanations text
    ## abbrv <- paste0("Abbreviations and explanations: ", paste0(sort(unlist(abbr)), collapse = "; ")) # Make abbreviation string
    ## ## Format the table using xtable
    ## formatted_table <- print.xtable(xtable(table,
    ##                                        caption = "\\bf Characteristics of the samples analysed in this study",
    ##                                        label = "tab:sample-characteristics"),
    ##                                 type = "latex",
    ##                                 table.placement = "!ht",
    ##                                 include.rownames = FALSE,
    ##                                 include.colnames = TRUE,
    ##                                 caption.placement = "top",
    ##                                 print.results = FALSE)
    ## star_caption <- abbr
    ## if (include_missing_column) star_caption <- paste0("*The total number (\\%) of observations with missing data. ", abbrv)
    ## formatted_table <- add.star.caption(formatted_table, star_caption) # add caption*
    ## Put formatted table in tables
    ## tables$formatted <- formatted_table
    ## Save formatted table to results file
    if (save.to.results) {
        formatted.table <- paste0(kable(table, caption = "Sample characteristics", format = "markdown"), collapse = "\n")
        SaveToResults(formatted.table, "sample.characteristics.table")
    }
    ## Save formatted table to disk 
    if (save.to.disk) {
        ## Create R markdown code
        table.file <- paste0("```{r echo = FALSE, results = 'asis'} \n",
                             "kable(table, caption = 'Sample characteristics') \n",
                             "```")
        ## Write to disk
        write(table.file, "sample_characteristics_table.rmd")
        ## Render to desired format
        if (file.format != "rmd") {
            output.format.list <- list(docx = "word_document",
                                       pdf = "pdf_document",
                                       html = "html_document")
            rmarkdown::render("sample_characteristics_table.rmd",
                              output_format = output.format.list[[file.format]])
            file.remove("sample_characteristics_table.rmd")
        }
    }
    ## Return table
    return(table)
}
