#' Import titco study data from mysql server
#'
#' Imports the titco study data from a mysql server, if no arguments are set, defaults to mangrove SSH tunnel-configuration.
#' @import RMySQL
#' @import DBI
#' @param mysql.server.name Character vector of length 1. The hostname or IP of the mysql-server, defaults to 127.0.0.1.
#' @param mysql.server.port Integer. The port of mysql-server, defaults is 3307.
#' @param mysql.database Character vector of length 1. The name of the database, defaults to TITCO
#' @param mysql.username Character vector of length 1. The username for the database, defaults to titco.
#' @param mysql.password Character vector of length 1. Password for db access, defaults to mangrovetitco
#' @param mysql.titco.table Character vector of length 1. The mysql table name, defaults to titco. To use the smaller sample data set, set this to titco_sample
#' @export
ImportTitcoStudyData <- function(mysql.server.name = "127.0.0.1", mysql.server.port = 3307,  mysql.database = "TITCO", mysql.username = "titco", mysql.password = "mangrovetitco", mysql.titco.table = "titco") {
  mydb <- dbConnect(MySQL(), user=mysql.username, password=mysql.password, dbname=mysql.database, host=mysql.server.name, port=mysql.server.port)
  ## Select all data from database
  study.data <- dbGetQuery(mydb, sprintf("select * from %s", mysql.titco.table))
  return(study.data)
}
