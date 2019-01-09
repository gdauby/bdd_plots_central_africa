
#' Launch shiny app for taxa
#'
#' Allow exploring the database by taxa
#'
#' @return Launch in web browser the app
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @export
query_taxa_app <- function() {
  shiny::runApp(appDir = "./query_individuals_data/",  launch.browser = TRUE)
}

#' Load the database
#'
#' Load the database and ask for password
#'
#' @return The database is loaded
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
.call.mydb <- function() {

  if(!exists("mydb")) {
    pass <- rstudioapi::askForPassword("Please enter your password")

    mydb < DBI::dbConnect(RPostgres::Postgres(),dbname = 'plots_transects',
                      host = 'localhost', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                      port = 5432, # or any other port specified by your DBA
                      user = 'postgres',
                      password = pass)

    assign("mydb", mydb, envir = .GlobalEnv)

    # return(mydb)
  }
}


### provide list of countries where plots occur
country_list <- function() {
  if(!exists("mydb")) .call.mydb() # mydb <-

  nn <-
    tbl(mydb, "data_liste_plots") %>%
    group_by(country) %>%
    count() %>%
    mutate(n=as.integer(n)) %>%
    collect()

  # dbDisconnect(mydb)

  return(nn)
}


