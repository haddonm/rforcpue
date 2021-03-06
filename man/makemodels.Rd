% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cpuefuncs.r
\name{makemodels}
\alias{makemodels}
\title{makemodels makes a list of models as formula with their labels}
\usage{
makemodels(labelModel, dependent = "LnCE")
}
\arguments{
\item{labelModel}{is the set of variables from the input data.frame that are to
be included as sequentially added models. Thus if labelModel is set equal
to c('Year',"Vessel') the first model will be LnCE ~ Year and the second
will be LnCE ~ Year + Vessel. labelModel will also provide the headings
to the tables in the output list from standLnCE}

\item{dependent}{the name of the dependent variable in your dataset;
defaults to 'LnCE'}
}
\value{
a list of formula representing the set of models to be fitted
    along with the labels for each model
}
\description{
makemodels makes a list of models with their labels from
    an input vector of names. Can be used to prepare the input models
    ready for standLM.
}
\examples{
 labelM <- c("Year","Vessel","Month")
 makemodels(labelM)
}
