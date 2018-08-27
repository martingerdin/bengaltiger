#' Import study data from mysql. To be updated with global variables from configurationfile.
#'
#' Imports the study data from a mysql server
#' @import RMySQL
#' @import DBI
#' @param mysql.server.name Character vector of length 1. The hostname or IP of the mysqlserver, for localuse use localhost.
#' @param mysql.server.port Integer. The port of mysql-server, default is 3306.
#' @param mysql.database Character vector of length 1. The name of the database, TITCO for titco access
#' @param mysql.username Character vector of length 1. The username for the database, use titco or ntdb.
#' @param mysql.password Character vector of length 1. Password for db access.
#' @export
ImportTitcoStudyData <- function(mysql.server.name = "127.0.0.1", mysql.server.port = 3307,  mysql.database = "TITCO", mysql.username = "titco", mysql.password = "mangrovetitco", mysql.titco.table = "titco") {
  ## Error handling
  if (is.null(mysql.password)) stop("You have to supply a password for the database.")
  mydb <- dbConnect(MySQL(), user=mysql.username, password=mysql.password, dbname=mysql.database, host=mysql.server.name, port=mysql.server.port)
  ## Select all data from database
  study.data <- dbGetQuery(mydb, sprintf("select * from %s", mysql.titco.table))
  return(study.data)
}
