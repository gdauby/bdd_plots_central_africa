% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_manip_db.R
\name{query_plots}
\alias{query_plots}
\title{List, extract and map selected plots and associated individuals}
\usage{
query_plots(
  plot_name = NULL,
  tag = NULL,
  country = NULL,
  locality_name = NULL,
  method = NULL,
  extract_individuals = FALSE,
  map = FALSE,
  id_individual = NULL,
  id_plot = NULL,
  id_tax = NULL,
  id_specimen = NULL,
  show_multiple_census = FALSE,
  show_all_coordinates = FALSE,
  remove_ids = TRUE,
  collapse_multiple_val = FALSE,
  extract_traits = TRUE,
  extract_individual_features = TRUE,
  traits_to_genera = FALSE,
  wd_fam_level = FALSE,
  include_liana = FALSE,
  extract_subplot_features = TRUE,
  concatenate_stem = FALSE,
  remove_obs_with_issue = TRUE
)
}
\arguments{
\item{plot_name}{String. Fuzzy match on plot name.}

\item{tag}{Numeric. Exact tag number of the plot.}

\item{country}{String. Fuzzy match on country name.}

\item{locality_name}{String. Fuzzy match on locality name.}

\item{method}{String. Fuzzy match on inventory method.}

\item{extract_individuals}{Logical. If TRUE, returns individual-level data instead of plot list.}

\item{map}{Logical. If TRUE, produces an interactive map of selected plots.}

\item{id_individual}{Numeric. ID of a specific individual to extract (see id_n column of database)}

\item{id_plot}{Numeric. ID of a specific plot to extract.}

\item{id_specimen}{Numeric. ID of a herbarium specimen linked to individuals.}

\item{show_multiple_census}{Logical. Whether to include multiple censuses. Default is FALSE.}

\item{remove_ids}{Logical. If TRUE, removes ID columns from the output. Default is TRUE.}

\item{collapse_multiple_val}{Logical. If TRUE, collapses multiple trait values into a single character string (dash-separated).}

\item{extract_traits}{Logical. Whether to extract species-level traits.}

\item{extract_individual_features}{Logical. Whether to extract individual-level traits or features.}

\item{traits_to_genera}{Logical. If TRUE, extrapolates species-level traits to genus level.}

\item{wd_fam_level}{Logical. If TRUE, uses wood density at family level (currently not fully available).}

\item{include_liana}{Logical. Whether to include liana individuals. Default is FALSE.}

\item{concatenate_stem}{Logical. If TRUE, concatenates multi-stem individuals into a single row.}

\item{remove_obs_with_issue}{Logical. Whether to exclude individuals with issues. Default is TRUE.}
}
\value{
A tibble of plots or individuals if extract_individuals is TRUE
}
\description{
This function queries a PostgreSQL inventory database to return a list of forest plots or individuals, 
with options to include associated traits and metadata, and to generate maps.
}
\examples{
\dontrun{
  get_plot_data(country = "Gabon", extract_individuals = TRUE, map = TRUE)
}

}
\author{
Gilles Dauby, \email{gilles.dauby@ird.fr}
}
