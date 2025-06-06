% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subsplots_features_function.R
\name{add_subplottype}
\alias{add_subplottype}
\title{Add a type in subplot table}
\usage{
add_subplottype(
  new_type = NULL,
  new_valuetype = NULL,
  new_maxallowedvalue = NULL,
  new_minallowedvalue = NULL,
  new_typedescription = NULL,
  new_factorlevels = NULL,
  new_expectedunit = NULL,
  new_comments = NULL
)
}
\arguments{
\item{new_type}{string value with new type descritors - try to avoid space}

\item{new_valuetype}{string one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character'}

\item{new_maxallowedvalue}{numeric if valuetype is numeric, indicate the maximum allowed value}

\item{new_minallowedvalue}{numeric if valuetype is numeric, indicate the minimum allowed value}

\item{new_typedescription}{string full description of trait}

\item{new_factorlevels}{string a vector of all possible value if valuetype is categorical or ordinal}

\item{new_expectedunit}{string expected unit (unitless if none)}

\item{new_comments}{string any comments}
}
\value{
nothing
}
\description{
Add feature and associated descriptors in subplot list table
}
\author{
Gilles Dauby, \email{gilles.dauby@ird.fr}
}
