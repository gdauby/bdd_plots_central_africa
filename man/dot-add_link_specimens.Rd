% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_manip_db.R
\name{.add_link_specimens}
\alias{.add_link_specimens}
\title{Add link between specimen and individual}
\usage{
.add_link_specimens(
  new_data,
  col_names_select = NULL,
  col_names_corresp = c("id_specimen", "id_n", "type"),
  launch_adding_data = FALSE
)
}
\arguments{
\item{new_data}{tibble fuzzy person name to look for}

\item{col_names_select}{a vector of string that select columns of new_data to consider, it must be 3 columns}

\item{col_names_corresp}{a vector of string of same length of col_names_select, should not be changed}

\item{launch_adding_data}{logical, if TRUE links are added, by default it is FALSE for security}
}
\value{
A tibble of all subplots
}
\description{
Generate link between individual and specimens
}
\author{
Gilles Dauby, \email{gilles.dauby@ird.fr}
}
