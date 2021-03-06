% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demographic.summary.R
\name{demographic.summary}
\alias{demographic.summary}
\title{Describes the demographic characteristics (sex, age) of the population under study}
\usage{
demographic.summary(input, maleCode, femaleCode, verbose = FALSE,
  warnings = TRUE)
}
\arguments{
\item{input}{Object of \code{cupcakeData} class.}

\item{maleCode}{Characters(s) used to determine the male condition of a patient. 
Depending on the database it can be determined, for example, as \code{Male},
\code{MALE}, \code{M}, with digits as \code{0} or \code{1}.}

\item{femaleCode}{Characters(s) used to determine the female condition of a patient. 
Depending on the database it can be determined, for example, as \code{Female},
\code{FEMALE}, \code{F}, with digits as \code{0} or \code{1}.}

\item{verbose}{By default \code{FALSE}. Change it to \code{TRUE} to get an
on-time log from the function.}

\item{warnings}{By default \code{TRUE}. Change it to \code{FALSE} to don't see
the warnings.}
}
\value{
A multiple graph containing a barplot with age distribution,  
a boxplot representing age distribution by gender and a pie chart representing 
gender distribution.
}
\description{
Given an object of class \code{cupcakeData}, and the characters used to 
specify the gender, a graphic containing 3 plots, a barplot with age distribution,  
a boxplot representing age distribution by gender and a pie chart representing 
gender distribution is obtained.
}
\examples{
load(system.file("extdata", "RcupcakeExData.RData", package="Rcupcake"))
demographic.summary( input = RcupcakeExData, 
                     maleCode   = "male", 
                     femaleCode = "female"
           )
}
