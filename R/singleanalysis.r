

#' @title makeonemodel generates a single model for use in dosingle
#'
#' @description makeonemodel puts together the formula needed to
#'     run a statistical model, but, different to makemodels, it
#'     only generates a single model.
#'
#' @param labelModel a vector of labels for each factor to be included
#'    in the analysis
#' @param dependent the name of the dependent variable; defaults to
#'    LnCE
#'
#' @return a formula of 'dependent ~ labelModel components
#' @export makeonemodel
#'
#' @examples
#' labelM <- c("Year","Vessel","DepCat","Zone:Month")
#' makeonemodel(labelM)
#' makeonemodel(labelM,dependent="LnCE")
makeonemodel <- function(labelModel,dependent = "LnCE") { # labelModel=labelM[1:i]; dependent = "LnCE"
  numvars <- length(labelModel)
  interterms <- grep(":", labelModel)
  ninter <- length(interterms)
  form <- paste0(dependent, " ~ ", labelModel[1])
  if (numvars > 1) {
    for (i in 2:(numvars - ninter))
      form <- paste0(form, " + ", labelModel[i])
    if (ninter > 0) for (i in interterms) {
      form <- paste0(form, " + ", labelModel[i])
    }
  }
  form <- as.formula(form)
  return(form)
} # end of makeonemodel

#' @title dosingle conducts a standardization of indat using the inmodel
#'
#' @description dosingle conducts a standardization of indat using the
#'    inmodel.
#'
#' @param inmodel the formula used in the analysis; usually from the
#'    function makeonemodel.
#' @param indat the data.frame containing the data to be analysed.
#'
#' @return a list with a similar structure to the out object, so not
#'    a outce class member but can be used with plotstand
#' @export  dosingle
dosingle <- function(inmodel,indat) {  # inmodel=inmod; indat=ab2
  ans <- lm(inmodel,data=indat)
  bits <- unlist(strsplit(as.character(inmodel)," "))
  modcoef <- summary(ans)$coefficients
  years <- getfact(modcoef,bits[3])
  yrs <- sort(unique(indat[,bits[3]]))

  geo <- paste0(bits[2]," ~ ",bits[3])
  ans2 <- lm(as.formula(geo),indat)
  modcoefG <- summary(ans2)$coefficients
  yearsG <- getfact(modcoefG,bits[3])
  Results <- cbind("Year"=yearsG[,"Scaled"],"optimum"=years[,"Scaled"])
  rownames(Results) <- yrs
  StErr <- Results
  StErr[,1] <- yearsG[,"SE"]
  StErr[,2] <- years[,"SE"]
  optimum <- 2
  result <- list(Results=Results,StErr=StErr, Optimum=optimum,
                 modelcoef=modcoef,optModel=ans,modelG=ans2,years=yrs)
  return(result)
}  # end of dosingle

