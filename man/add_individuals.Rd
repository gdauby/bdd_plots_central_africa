% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_manip_db.R
\name{add_individuals}
\alias{add_individuals}
\title{Add new individuals data}
\usage{
add_individuals(
  new_data,
  col_names_select,
  col_names_corresp,
  id_col,
  features_field = NULL,
  launch_adding_data = FALSE
)
}
\arguments{
\item{new_data}{tibble new data to be import}

\item{col_names_select}{string}

\item{col_names_corresp}{string}

\item{id_col}{integer indicate which name of col_names_select is the id for matching plot in metadata}

\item{features_field}{vector string of field names in new_data containing the features associated with individual or stem data}

\item{launch_adding_data}{logical FALSE whether adding should be done or not}
}
\value{
No return value individuals updated
}
\description{
Add new individuals data
}
\author{
Gilles Dauby, \email{gilles.dauby@ird.fr}
}
