% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plantsFINALforPackage.R
\name{setup.plants}
\alias{setup.plants}
\title{Simulate a Plant Ecosystem}
\usage{
setup.plants(reproduce, survive, compete.matrix, names = NULL)
}
\arguments{
\item{reproduce}{A vector of length n storing reproduction probabilities for n plant species. Must be values between 0 and 1.}

\item{survive}{A vector of length n storing survival probabilities for n plant species. Must be values between 0 and 1.}

\item{compete.matrix}{An nxn matrix storing the probability the plant in row i will outcompete the plant in column j, given n plant species. Must be values between 0 and 1.}

\item{names}{A vector of length n storing the names of the n plants in the ecosystem. If no names are given, they will be automatically assigned.}
}
\value{
A list containing all survival, reproduction, and survival values for the user's plant ecosystem.
}
\description{
Setup.plants: makes a list of objects needed to run the plant ecosystem function.
}
