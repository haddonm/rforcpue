% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cpueplots.R
\name{examinevar}
\alias{examinevar}
\title{examinevar tabulates and plots the properties of the input variable}
\usage{
examinevar(
  x,
  invar = "",
  catch = "catch",
  effort = "hours",
  cpue = "cpue",
  year = "year",
  spsname = "",
  resdir,
  runname,
  addlines = 5,
  wid = 6,
  hgt = 5,
  category = invar
)
}
\arguments{
\item{x}{the data.frame of fishery dependent data}

\item{invar}{the name of the variable or factor whose properties are to be
examined}

\item{catch}{the name used to identify the catch factor of the species}

\item{effort}{the name used to identify the effort factor of the species}

\item{cpue}{the name used to identify the cpue factor of the species}

\item{year}{the name used to identify the year factor of the species}

\item{spsname}{the name of the species of interest.}

\item{resdir}{the full path of the results directory into which the plot files
and the .csv files for the tables are to be stored.}

\item{runname}{the name of the particular run being made}

\item{addlines}{the number of lines to be added at the top of the plots, so
The year totals can be included. Default=5, the number needed will depend
on how many unique values there are for 'invar'. If there are many then 
more lines may need to be added to space out the year totals.}

\item{wid}{the width of each plot, default=6}

\item{hgt}{the height of each plot, default=5}

\item{category}{default = invar, but can be changed if required}
}
\value{
nothing but it does generate 4 plots and 2 tables into resdir
}
\description{
examinevar tabulates and plots the properties of an input
    variable contained within an input data.frame. This function is designed
    to simplify the characterization of fisheries dependent data prior to 
    conducting a statistical standardization. It does this by plotting for 
    the input variable (invar), the records-by-year, the catch-by-year, and 
    the effort-by-year. for each level of 'invar' it also counts the records
    across years, and sums the catches and effort across years. Each of these 
    are stored either as .csv or .png files 'in resdir'. It also logs these
    files in resfile, which opens the possibility of displaying all results
    in a local webpage under a tab labelled by the contents of 'invar'. The 
    'resdir', 'resfile', and 'runname' should be the same as those used by
    makehtml to prepare a directory and 'resfile' to store the files to be
    used to make a local webpage of results.
}
\examples{
print("wait on internal data")
}
