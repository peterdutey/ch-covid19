% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute.R
\name{total_cases_in_home}
\alias{total_cases_in_home}
\title{Compute the total number of cases in the home}
\usage{
total_cases_in_home(occupancy, new_cases, order_by)
}
\arguments{
\item{occupancy}{a numeric vector of total number of residents present
in the home for every date}

\item{new_cases}{a numeric vector of new cases recorded in the home at
every date}

\item{order_by}{a vector of dates or day numbers}
}
\value{
a numeric vector of total new and previous cases currently in
the home
}
\description{
Compute the total number of cases in the home
}
