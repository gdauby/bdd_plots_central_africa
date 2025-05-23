% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxa_traits_function.R
\name{add_sp_traits_measures}
\alias{add_sp_traits_measures}
\title{Add an observation in trait measurement table at species level}
\usage{
add_sp_traits_measures(
  new_data,
  col_names_select = NULL,
  col_names_corresp = NULL,
  traits_field,
  collector = NULL,
  idtax = NULL,
  features_field = NULL,
  add_data = FALSE,
  ask_before_update = TRUE
)
}
\arguments{
\item{new_data}{tibble}

\item{col_names_select}{string vector}

\item{col_names_corresp}{string vector}

\item{collector}{string column name which contain the collector name}

\item{idtax}{string column name which contain the individual tag for linking}

\item{add_data}{logical whether or not data should be added - by default FALSE}

\item{plot_name_field}{string column name which contain the plot_name for linking}

\item{id_plot_name}{string column name which contain the ID of plot_name}

\item{id_tag_plot}{string column name which contain the ID of individuals table}
}
\value{
list of tibbles that should be/have been added
}
\description{
Add a trait measure in trait measurement table
}
\author{
Gilles Dauby, \email{gilles.dauby@ird.fr}
}
