#' AIS variables
#'
#' Returns the names of all AIS variables in the TITCO dataset
#' @examples
#' AISVariables()
#' 
#' @export
AISVariables <- function() {
    ## Create return set
    return.set <- c("external", "head_and_neck", "face", "chest", "extremities")
    ## Return return.set
    return(return.set)
}
