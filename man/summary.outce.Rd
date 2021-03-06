% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cpuefuncs.r
\name{summary.outce}
\alias{summary.outce}
\title{summary.outce S3 function to summarize standardization results}
\usage{
\method{summary}{outce}(x)
}
\arguments{
\item{x}{is the output from a standardization perhaps using standLM.
It must be of class outce  an S3 class}
}
\value{
Summarizes the contents of the output from  standardization,
    this includes
\describe{
  \item{Structure}{this lists the top-level structure of the object
    output from the standardizations from standLM}
  \item{Parameters by Model}{This tabulates the yearly parameters for
    each of the models tested, the 'mods' input to standLM}
  \item{Std Errors by Model}{The matching standard errors for each of the
    models, one column for each column of paramters; large sample sizes
    mean these are usually an significant under-estimate of the
    true variaiton}
  \item{Model Diagnostic Properties}{A table of the statistical performance
    of each model, allowing the identification of the optimum model}
  \item{Optimum model}{identifies which column relates to the optimum
    statistical model}
  \item{All parameters}{The un-back-transformed parameters for the complete
    optimum model}
}
}
\description{
summary.outce an S3 function to summarize out the primary
    results from a standardization. It includes the structure of the
    output so other things can be obtained if wished.
}
\examples{
\dontrun{
 data(sps)
 splabel = "Blue-Eye"
 labelM <- c("Year","Vessel","Month")
 sps1 <- makecategorical(labelM,sps)
 mods <- makemodels(labelM)
 out <- standLM(mods,sps1,splabel)
 summary(out)
}
}
