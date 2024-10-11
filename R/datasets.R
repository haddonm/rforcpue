

#' @title sps - a 11603 x 10 data.frame for testing cpue and mapping functions
#'
#' @description sps - a 11603 x 10 data.frame for testing CPUE functions
#'    containing simulated trawl shot CPUE data for the years 2003 - 2014,
#'    including details of year, month, day, vessel, catch, longitude,
#'    latitude, depth trawled in meters, a daynight identifier, and effort.
#'    The cpue and log(cpue) can be calculated from this data.
#'
#' @format A data frame with 10603 x 10 variables:
#' \describe{
#'   \item{Year}{The year in which fishing takes place}
#'   \item{Month}{The month in which fishing took place}
#'   \item{Vessel}{a code uniquely identifying vessels through time}
#'   \item{catch_kg}{literally the catch of the species in the shot in kg}
#'   \item{Long}{the longitude of the start of the trawl shot}
#'   \item{Lat}{the latitude of the start of the trawl shot}
#'   \item{Depth}{the average depth of trawling in meters}
#'   \item{DayNight}{a code denoting the daynight status D = day, N = night,
#'      M = mixed}
#'   \item{Effort}{the hours trawled}
#'   \item{Zone}{The fished area split into three latitudinal zones}
#' }
#' @docType data
#' @name sps
#' 
NULL
