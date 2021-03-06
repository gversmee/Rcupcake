% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my.data.R
\name{my.data}
\alias{my.data}
\title{Query analysis to the API}
\usage{
my.data(query, url, responseFormat = "CSV", outputPath = paste0(getwd(),
  "/queryData.txt"), verbose = FALSE)
}
\arguments{
\item{query}{A JSON query, created with my.query function or contained in a
text file.}

\item{url}{The url.}

\item{outputPath}{Path and the file name where the output file will be saved. By default it
will be saved in the working directory with the name queryData.}

\item{verbose}{By default \code{FALSE}. Change it to \code{TRUE} to get an
on-time log from the function.}
}
\value{
An object of class \code{data.frame} with the query output.
}
\description{
Given an url and a JSON object, it generates a \code{data.frame} object with 
the output of the query.
}
\examples{

#query <- my.data( 
#              query  = system.file("extdata", "jsonQueryNhanes", package="Rcupcake"), 
#              url    = "https://nhanes.hms.harvard.edu/"
#              )
}
