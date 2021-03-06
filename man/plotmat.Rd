% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cpueplots.R
\name{plotmat}
\alias{plotmat}
\title{plotmat plots a matrix of data}
\usage{
plotmat(inm, xlab = "years", ylab = "", legloc = "topright")
}
\arguments{
\item{inm}{a matrix of values whose rownames constitute the x-axis}

\item{xlab}{the xlabel, defaults to ""}

\item{ylab}{the ylabel, defaults to ""}

\item{legloc}{the location of the legend defaults to "topright",
could be "topleft", "bottomleft", or "bottomright"}
}
\value{
nothing, but it does plot a graph
}
\description{
plotmat is a utility function that is used to plot up matrices 
    calculated by cbb, hbb, geobb, and others. It expects a matrix of values 
    and the variable against which it is to be plotted as the rownames of the 
    matrix. Not exported but is used by rforcpue. A legend is only produced 
    if there is a matrix of values and is made up of the column names of the 
    input catchb matrix.
}
\examples{
\dontrun{
 ind=abd2;legloc="topright";block="block";catch="catch";year="year"

}
}
