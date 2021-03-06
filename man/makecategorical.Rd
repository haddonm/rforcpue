% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cpuefuncs.r
\name{makecategorical}
\alias{makecategorical}
\title{makecategorical converts given variables into categorical factors}
\usage{
makecategorical(labelModel, indat)
}
\arguments{
\item{labelModel}{is the set of variables/column names from the data.frame
that are to be converted into categorical factors. Any interaction terms
should come last.}

\item{indat}{is the data.frame that is to be analysed in the standardization}
}
\value{
a copy of the database with the selected variables converted into
    factors
}
\description{
makecategorical given a list of variables, as character
    strings, this function converts each variable into a factor after
    checking no mistake has been made with the spelling of the variable
    name. A copy of the data is made
    so that any non-categorical variables can be retained. Any interaction
    terms included must be last in the vector making up labelModel
}
\examples{
\dontrun{
  data(abeg)
  labelM <- c("year","diver","month")
  ab1 <- makecategorical(labelM,abeg)
}
}
