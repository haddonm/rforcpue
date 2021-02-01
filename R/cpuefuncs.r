
#' @title addcount adds a count of years and avC to a data.frame
#'
#' @description addcount given an input data.frame containing catch,
#'     diver, and year as variables this function first sums the catch by
#'     diver, by year, then counts each diver's occurrence across the
#'     years, and each diver's catch. Then stepping through each diver it
#'     populates two new colummns named 'count' and 'avC'.
#'
#' @param indat a data.frame containing at least columns named 'catch',
#'    'diver' (or in fact 'varid'), and 'year'.
#' @param varid the variable in the data.frame for which the count and
#'    average catch per year is to be estimated. Defaults to 'diver'
#'    ready for abalone data but could be vessel for other fisheries,
#'    or any other factor of interest.
#' @param group the first grouping variable, typically diver ifor abalone
#' @param catch the variable name for the catch, sometimes blip in abalone
#'
#' @return a data.frame made of the input data.frame plus two new columns
#' @export
addcount <- function(indat,varid="diver",group="year",catch="catch") {
   outdat <- indat
   outdat$count <- NA
   outdat$avC <- NA
   cbv <- tapply(outdat[,catch],list(outdat[,varid],outdat[,group]),sum,na.rm=TRUE)/1000
   years <- as.numeric(colnames(cbv))
   nyrs <- length(years)
   count <- apply(cbv,1,function(x) length(which(x > 0)))
   avC <- apply(cbv,1,mean,na.rm=TRUE)
   vessels <- as.numeric(names(count))
   numves <- length(vessels)
   for (i in 1:numves) {
      pickv <- which(outdat[,varid] == vessels[i])
      outdat$count[pickv] <- count[i]
      outdat$avC[pickv] <- avC[i]
   }
   return(outdat)
} # end of addcount

#' @title checkDF checks a CPUE data.frame has all required fields
#'
#' @description checkDF checks a CPUE data.frame contains all fields
#'    expected for analysis. These are year, month, yearq, diver, catch,
#'    Long, Lat, Depth, DayNight, Zone, Effort, Method, Fishery, LnCE,
#'    and DepCat. If any are missing it identifies them if not it confirms
#'    all is ok. Although, except for their class checkDF does not consider
#'    the contents of each field. This file needs MODIFICATION
#'
#' @param x a data.frame of CPUE data used in the SESSF
#'
#' @return only a text message to the console confirming all ok or
#'    reporting which fields are missing
#' @export
#'
#' @examples
#' \dontrun{
#'  dataf <- as.data.frame(matrix(rnorm(100,mean=10,sd=1),nrow=20,ncol=5))
#'  colnames(dataf) <- c("Year","catch_kg","Long","Zone","LnCE")
#'  checkDF(dataf)
#'  dataf <- as.data.frame(matrix(rnorm(42,mean=10,sd=1),nrow=1,ncol=14))
#'  colnames(dataf) <- c("Year","Month","Vessel","catch_kg","Long","Lat",
#'                       "Depth","DepCat","DayNight","Zone","Effort",
#'                       "Method","Fishery","LnCE")
#'  checkDF(dataf)
#'  head(dataf)
#' }
checkDF <- function(x) {  # x <- sps1
   if (class(x) != "data.frame") stop("Input to checkDF must be a data.frame \n\n")
   needed <- c("Year","Month","Vessel","catch_kg","Long","Lat","Depth","DepCat","Zone",
               "Effort","LnCE","DayNight","Method","Fishery")
   dflocation <- match(needed,colnames(x))
   pick <- which(is.na(dflocation))
   if (length(pick) == 0) {
      cat("All fields needed for SESSF work are present \n")
      whatclass <- sapply(x[dflocation],class)
      numbers <- (whatclass[1:11] == "numeric")
      nonnum <- (whatclass[12:14] == "factor")
      if (length(numbers[numbers == FALSE]) == 0) cat("All numeric fields are numeric \n")
      if (length(nonnum[nonnum == FALSE]) == 0) cat("All non-numeric fields are factors \n")
   } else {
      cat("The following fields are missing ",needed[pick],"\n\n")
   }
   if (dim(x)[1] < 100) cat("Very few records, ",dim(x)[1],", available for standardization \n\n")
}  # end of checkDF


#' @title coef.outce S3 method to extract the parameter values from an outce object
#'
#' @description coef.outce extracts the parameter values from an outce
#'     object from a standardization (e.g. from standLM) by extending
#'     the generic 'coef' function to apply to outce objects. The output
#'     includes the log-space coefficients, the lower and upper 95%
#'     confidence intervals, the standard error of each parameter, and
#'     p the probability that the parameter is significantly different
#'     from zero. These coefficients would need to be back-transformed
#'     to return to the original scale. This would use
#'     x = exp(coef + (sterr*sterr)/2); note the bias correction
#'
#' @param inout is a outce object such as produced by standLM
#'
#' @return a matrix containing the optimum model parameters,
#' @exportMethod coef.outce
#' @examples
#' \dontrun{
#'  data(sps)
#'  splabel = "Species"
#'  labelM <- c("Year","Vessel","Month")
#'  mods <- makemodels(labelM)
#'  sps1 <- makecategorical(labelM,sps)
#'  out <- standLM(mods,sps1,splabel)
#'  coef(out)
#' }
coef.outce <- function(inout) {   # S3 class development
   test <- dim(inout$Parameters$mat)
   if (length(test) > 0) {
      warning("The input object is not an 'outce' object from standLnCE  \n")
   } else {
      ans <- NA
      cat("Output from standLnCE  \n")
      tmp <- inout$Parameters$coefficients
      backTran <- exp((tmp[,"Estimate"] + (tmp[,"Std. Error"]^2)/2))
      ans <- cbind(tmp,backTran)
      colnames(ans)
   }
   return(ans)
} # end of coef.outce

#' @title expandmatrix reshapes a matrix of values into a 3 column data.frame
#' 
#' @description expandmatrix takes an oblong matrix of values and expands it
#'     into a three column data.frame of row, column, value. This is then easier 
#'     to plot as a scattergram or is used within categoryplot. It expects to 
#'     have the year values in the columns = xvalues
#'
#' @param x a matrix of values 
#'
#' @return a 3-column matrix of (rows x cols) rows from the input matrix
#' @export
#'
#' @examples
#' x <- matrix(rnorm(25,5,1),nrow=5,ncol=5,dimnames=list(1:5,1:5))
#' res <- expandmatrix(x)
#' res
expandmatrix <- function(x) { #  x=t(numyr)
   ylabel <- as.numeric(rownames(x))
   xlabel <- as.numeric(colnames(x))
   nx <- length(xlabel)
   ny <- length(ylabel)
   res <- as.data.frame(matrix(0,nrow=(nx*ny),ncol=3))
   count <- 0
   for (i in 1:nx) {
      for (j in 1:ny) {
         count <- count + 1
         res[count,] <- c(xlabel[i],ylabel[j],x[j,i])
      }
   }
   rownames(res) <- paste0(res[,1],"_",res[,2])
   colnames(res) <- c("rows","cols","value")
   
   return(res)
} # end of expandmatrix

#' @title fishery generates vectors of year, catch, effort, and cpue
#'
#' @description fishery generates vectors of year, catch, effort, and cpue
#'    This permits a summary of the basic fishery characteristics within a
#'    data.frame or matrix. It requires the columns to be labelled.
#' @param indat the input data.frame or matrix
#' @param years the name of the year factor; defaults to "year"
#' @param catch the name of the catch variable; defaults to "catch"
#' @param effort the name of effort variable; defaults to "effort"
#' @param cpue the name of the cpue variable; defaults to "cpue"
#' @return a matrix with columns Year, Catch, Effort, Bias Corrected
#'    geometric mean CPUE, the naive geometric means, the arithemtic mean
#'    CPUE, and the number of records
#' @export
#' @examples
#' \dontrun{
#' data(sps)
#' fishery(sps,years="Year",catch="catch_kg",effort="Effort",cpue="CE")
#' }
fishery <- function(indat,years="year",catch="catch",effort="effort",
                    cpue="cpue") {
   yrs <- sort(unique(indat[,years]))
   catches <- tapply(indat[,catch],indat[,years],sum,na.rm=TRUE)
   eff <- tapply(indat[,effort],indat[,years],sum,na.rm=TRUE)
   geo <- tapply(indat[,cpue],indat[,years],geomean)
   ngeo <- exp(tapply(log(indat[,cpue]),indat[,years],mean,na.rm=TRUE))
   arith <- tapply(indat[,cpue],indat[,years],mean,na.rm=TRUE)
   records <- table(indat[,years])
   ans <- cbind(yrs,catches,eff,geo,ngeo,arith,records)
   colnames(ans) <- c("Years","Catch","Effort","BCGeo","NaiveGeo","Arith","Records")
   return(ans)
}  # end of Fishery

#' @title getaav calculates annual absolute variation in catch
#'
#' @description getaav calculates the annual absolute change in catch
#'     for an input vector of catches, which could be across a series
#'     of years or even across different spatial units for a single
#'     year (an unusual use).
#'     The equation used is aav = 100 x sum(|Ct - Ct-1|)/(sum(Ct).
#'
#' @param invect a vector of catches
#' @param narm boolean, should NAs be removed? If not then getaav will
#'     return an NA.  Default = TRUE
#'
#' @return a single scalar value the AAV of the input catches
#' @export
#'
#' @examples
#'   catch <- c(1,2,3,4,5,4,3,2,1)
#'   getaav(catch)  # should equal 0.32
getaav <- function(invect,narm=TRUE) { # invect=cbby[,1]; narm=TRUE
   if (any(is.na(invect)) & (narm)) {
      pick <- which(is.na(invect))
      invect <- invect[-pick]
   }
   nyr <- length(invect)
   totC <- sum(invect)
   aac <- sum(abs(invect[2:nyr] - invect[1:(nyr-1)]))
   aav <- 0.0
   if (totC > 0.0) aav <- aac/totC
   return(aav)
} # end of getaav

#' @title getfact extracts parameter estimates for a given factor
#'
#' @description getfact extracts the parameter estimates for a given factor
#'     from either a matrix, an array, an outce object (from standLM), or an 
#'     lm object or a gam object. It does this by searching the rownames of the
#'     output parameters of the optimum model. It also checks for interaction
#'     terms, which for categorical factors is the same as determining a
#'     trend of the two factors relative to each other. e.g. for Zone:Month
#'     the outcome is the monthly trend for each zone.
#'
#' @param inmat this can be an outce object from standLM, but it can also be
#'     a matrix of coefficients, an lm object, or a gam object
#' @param invar the model variable whose parameters are wanted, eg "month"
#' @param biascorrect when back transforming the coefficients should we use the
#'     log-normal bias-correction or not. default=TRUE
#'
#' @return a matrix containing the parameters for invar
#' @export
getfact <- function(inmat,invar,biascorrect=TRUE) {  # inmat=mat; invar="year"
   allowable <- c("matrix","array","outce","lm","gam")
   whatclass <- class(inmat)
   if (length(whatclass) > 2) {
      if ("gam" %in% whatclass) {
         whatclass <- "gam"
      } else {
         if ("lm" %in% whatclass) {
            whatclass <- "lm"
         } else {
            whatclass <- "no"
         }
      }
   }
   if (length(whatclass) == 2) whatclass <-  whatclass[1] # for R4 matrices
   if (whatclass %in% allowable) {
      if ((whatclass == "matrix") | (whatclass == "array")) pardat <- inmat
      if (whatclass == "outce") pardat <- inmat$Parameters$coefficients
      if (whatclass == "lm") pardat <- summary(inmat)$coefficients
      if (whatclass == "gam") pardat <- summary(inmat)$p.table
   } else {
      stop("Input matrix is not, in fact, a matrix of coefficients")
   }
   pick <- grep(invar,rownames(pardat))
   if (length(pick) == 0) stop("The selected factor is not in the selected model!  \n")
   startmat <- pardat[pick,]               # isolate rows containing variable in question
   pickI <- grep(":",rownames(startmat))  # check for interaction terms and split off
   if (length(pickI) > 0) {               # if present
      intermat <- startmat[pickI,]
      startmat <- startmat[-pickI,]
   }
   lnce <- startmat[,"Estimate"]         # first do the non-interaction terms
   se <- startmat[,"Std. Error"]
   tval <- startmat[,"t value"]
   Prob <- startmat[,"Pr(>|t|)"]
   if (biascorrect) {
      backtran <- exp(lnce + (se * se)/2)
   } else {
      backtran <- exp(lnce)
   }
   ans <- cbind(c(1.0,backtran),c(0,se),c(0,lnce),
                scaleCE(c(1.0,backtran),avCE=1.0),
                c(NA,tval),c(NA,Prob)
   )
   colnames(ans) <- c("Coeff","SE","LogCE","Scaled","t value","Prob")
   rownames(ans)[1] <- invar
   norigvar <- dim(ans)[1]
   if (length(pickI) > 0) {              # do interaction terms if they exist
      terms <- unlist(strsplit(rownames(intermat),":"))
      nrow <- dim(intermat)[1]
      firstvar <- grep(invar,terms)
      nfirst <- length(unique(terms[firstvar]))
      nsecond <- length(unique(terms[-firstvar]))
      if ((nfirst * nsecond) != nrow)
         stop(paste0("something wrong with the interactions terms for ",invar," \n",
                     "the first and second variables are not balanced"))
      start <- 1
      for (i in 1:(nrow/nfirst)) {  # i <- 1
         finish <- (start+nfirst-1)
         tmp <- intermat[start:finish,]
         lnce <- tmp[,"Estimate"]         # first do the non-interaction terms
         se <- tmp[,"Std. Error"]
         backtran <- exp(lnce + (se * se)/2)
         tmpans <- cbind(c(1.0,backtran),c(0,se),c(0,lnce),scaleCE(c(1.0,backtran),avCE=1.0))
         ans <- rbind(ans,tmpans)
         start <- finish + 1
      }
   }
   return(ans)
} # end of getfact

#' @title getlag is used to look for the response of cpue to previous catches
#'
#' @description getlag is a wrapper for the ccf function (cross correlation)
#'     that is used within the spm and aspm analyses to determine at what
#'     negative lag, if any, cpue data is informative about the stock dynamics
#'     beyond any information already available in the catch data.
#'     If the cpue is directly correlated with catches (lag=0 has a strong
#'     correlation) then cpue will not add much more information to an analysis.
#'     Only if there is a significant negative correlation is it likely that the
#'     cpue will increase the information available and make it more likely that
#'     a spm or aspm may be able to be fitted meaningfully to the available data.
#'     If there is no significant negative correlations then it becomes much
#'     more unlikely than a valid model fit will be possible. The getlag
#'     function first finds those rows for which both catch and cpue have values
#'     and then it runs the analysis. Thus, you cannot have gaps in your cpue
#'     data although there can be catches at the start or end or both for which
#'     there are no cpue data.
#'
#' @param fish the matrix or data.frame containing the fishery data (year, catch,
#'     and cpue)
#' @param maxlag the lag.max parameter for the ccf function; defaults to 10
#' @param plotout should a plot be made immediately; defaults to TRUE. If FALSE
#'     then, assuming the result of the analysis is put into an object called
#'     'ans' a call to plot(ans) will generate the required plot.
#' @param indexI if there are more than one time-series of cpue/indices then
#'     this parameter selects which to use
#'
#' @return an object of class acf, which can be plotted
#' @export
#'
#' @examples
#' \dontrun{
#' year <- 1985:2008
#' catch <- c(1018,742,868,715,585,532,566,611,548,499,479,428,657,481,645,961,
#'            940,912,955,935,940,952,1030,985)
#' cpue <- c(0.6008,0.6583,0.6791,0.6889,0.7134,0.7221,0.7602,0.7931,0.8582,
#'           0.8876,1.0126,1.1533,1.2326,1.2764,1.3307,1.3538,1.2648,1.2510,
#'           1.2069,1.1552,1.1238,1.1281,1.1113,1.0377)
#' dat <- makespmdata(cbind(year,catch,cpue))
#' out <- getlag(dat,plotout=FALSE)
#' plot(out,lwd=3,col=2)
#' }
getlag <- function(fish,maxlag=10,plotout=TRUE,indexI=1) { # fish=dat; maxlag=10;plotout=TRUE
   pickI <- grep("cpue",colnames(fish))
   pick <- which((fish[,"catch"] > 0) & (fish[,pickI[indexI]] > 0))
   ans <- ccf(x=fish[pick,"catch"],y=fish[pick,pickI[indexI]],lag.max=maxlag,main="",
              type="correlation",plot=plotout,ylab="Cross-Correlation")
   return(ans)
} # end of getlag


#' @title getrmse calculates the rmse of the input 'invar' series
#'
#' @description getrmse calculates the rmse of the input invar series (defaults
#'     to 'cpue') against an input 'year' time series. This is primarily
#'     designed to generate a more feasible estimate of the intrinsic
#'     variability of a cpue time-series that may be obtained from a cpue
#'     standardization. The year variable is needed to calcualte the loess
#'     curve.
#'
#' @param indat the matrix or data.frame containing both a 'year'
#'     column and an invar column (default to 'cpue')
#' @param invar the column whose rmse is wanted; defaults to 'cpue'
#' @param inyr the column that points to the year name
#' @return a list of the rmse and the loess predicted values of the invar for
#'     each year in the time-series
#' @export
#'
#' @examples
#' \dontrun{
#'  year <- 1986:1995
#'  cpue <- c(1.2006,1.3547,1.0585,1.0846,0.9738,1.0437,0.7759,1.0532,1.284,1.3327)
#'  dat <- as.matrix(cbind(year,cpue))
#'  getrmse(dat,invar="cpue")  # should be 0.08856596
#'  getrmse(dat,invar="cpue")$rmse
#' }
getrmse <- function(indat,invar="cpue",inyr="year"){  # indat=fish; invar="cpue"; inyr="year"
   if(iscol(inyr,indat) & iscol(invar,indat)) {
      nyr <- dim(indat)[1]
      predictedCE <- rep(NA,nyr)
      varloc <- grep(invar,colnames(indat))
      nvar <- length(varloc)
      if (nvar > 1) {
         obsvar <- rep(NA,nyr)
         for (i in 1:nvar) {
            pick <- which(indat[,varloc[i]] > 0)
            obsvar[pick] <- indat[pick,varloc[i]]
         }
      } else {
         obsvar <- indat[,varloc]
      }
      picky <- which(obsvar > 0)
      model <- loess(obsvar[picky] ~ indat[picky,inyr])
      predictedCE[picky] <- model$fitted
      rmse <- sqrt(sum(model$residuals^2)/model$n)
      return(list(rmse=rmse,predictedCE=predictedCE))
   } else {
      cat("Input data should contain both 'year' and 'cpue'  \n")
   }
} # end of getrmseCE

#' @title getStand extracts major results from an outce object
#'
#' @description getStand extracts major results from an outce object. which
#'    is generated using standLM. The main results include the year, the
#'    geometric mean (the first column of the standardization), the optimum
#'    model, the standard error of the optimum, and the lower and upper
#'    confidence intervals
#'
#' @param x the outce object derived from standLM
#' @param P the probability interval for the confidence intervals
#'
#' @return matrix containing the primary results
#' @export
#'
#' @examples
#' \dontrun{
#'  data(abeg)
#'  splabel = "Mollusc"
#'  labelM <- c("year","diverID","month","block","boatID")
#'  ab1 <- makecategorical(labelM,ab)
#'  mods <- makemodels(labelM)
#'  out <- standLM(mods,ab1,splabel)
#'  round(out$Results,4)
#'  round(out$WhichM,4)
#'  getStand(out)
#' }
getStand <- function(x,P=0.95) {
   if (class(x) != "outce") stop("input to getStand is not a outce object")
   yrs <- as.numeric(rownames(x$Results))
   geo <- x$Results[,1]
   opt <- x$Results[,x$Optimum]
   se <- x$StErr[,x$Optimum]
   Zmult <- -qnorm((1-(P/100))/2.0)
   lower <- opt * exp(-Zmult * se)
   upper <- opt * exp(Zmult * se)
   ans <- cbind(yrs,geo,opt,se,lower,upper)
   colnames(ans) <- c("year","geom","optimum","sterr","LCI","UCI")
   return(ans)
}  # end of getStand



#' @title incol is a utility to determine is a column is present in a matrix
#'
#' @description incol is a utility to determine whether a names columns is
#'     present in a given matrix or data.frame.
#'
#' @param incol the name of the column; defaults to "year" as an example
#' @param inmat the matrix or data.frame within which to search for incol
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
#' \dontrun{
#' test <- matrix(c(1,2,3,4),nrow=2,ncol=2,dimnames=list(1:2,c("year","Catch")))
#' print(test)
#' iscol("year",test)
#' iscol("Catch",test)
#' iscol("catch",test)
#' iscol("ages",test)
#' }
iscol <- function(incol="year",inmat) { # incol="ages"; inmat=dat
   if (length(grep(incol,colnames(inmat))) < 1) return(FALSE)
   else return(TRUE)
}


#' @title makemodels makes a list of models as formula with their labels
#'
#' @description makemodels makes a list of models with their labels from
#'     an input vector of names. Can be used to prepare the input models
#'     ready for standLM.
#'
#' @param labelModel is the set of variables from the input data.frame that are to
#'     be included as sequentially added models. Thus if labelModel is set equal
#'     to c('Year',"Vessel') the first model will be LnCE ~ Year and the second
#'     will be LnCE ~ Year + Vessel. labelModel will also provide the headings
#'     to the tables in the output list from standLnCE
#' @param dependent the name of the dependent variable in your dataset;
#'     defaults to 'LnCE'
#'
#' @return a list of formula representing the set of models to be fitted
#'     along with the labels for each model
#' @export
#' @examples
#'  labelM <- c("Year","Vessel","Month")
#'  makemodels(labelM)
makemodels <- function(labelModel,dependent="LnCE") {
   numvars <- length(labelModel)
   interterms <- grep(":",labelModel)
   ninter <- length(interterms)
   mods <- vector("list",(numvars+1))
   form <- paste0(dependent," ~ ",labelModel[1])
   mods[[1]] <- assign(paste0("ff",1),as.formula(form))
   if (numvars > 1){
      if (ninter > 0) {
         for (i in 2:(numvars - ninter)) {
            form <- paste0(form," + ",labelModel[i])
            mods[[i]] <- assign(paste0("ff",i),as.formula(form))
         }
         for (i in interterms) {
            interform <- paste0(form," + ",labelModel[i])
            mods[[i]] <- assign(paste0("ff",i),as.formula(interform))
         }
      } else {
         for (i in 2:numvars) {
            form <- paste0(form," + ",labelModel[i])
            mods[[i]] <- assign(paste0("ff",i),as.formula(form))
         }
      }
   }
   mods[[(numvars+1)]] <- labelModel
   return(mods)
} # end of makemodels

#' @title makecategorical converts given variables into categorical factors
#'
#' @description makecategorical given a list of variables, as character
#'     strings, this function converts each variable into a factor after
#'     checking no mistake has been made with the spelling of the variable
#'     name. A copy of the data is made
#'     so that any non-categorical variables can be retained. Any interaction
#'     terms included must be last in the vector making up labelModel
#'
#' @param labelModel is the set of variables/column names from the data.frame
#'     that are to be converted into categorical factors. Any interaction terms
#'     should come last.
#' @param indat is the data.frame that is to be analysed in the standardization
#'
#' @return a copy of the database with the selected variables converted into
#'     factors
#' @export
#' @examples
#' \dontrun{
#'   data(abeg)
#'   labelM <- c("year","diver","month")
#'   ab1 <- makecategorical(labelM,abeg)
#' }
makecategorical <- function(labelModel,indat) {
   Interact <- grep(":",labelModel)
   nInteract <- length(Interact)
   numvars <- length(labelModel) - nInteract
   for (fac in 1:numvars) {
      if (length(indat[,labelModel[fac]]) > 0) {
         indat[,labelModel[fac]] <- factor(indat[,labelModel[fac]])
      } else { warning(paste0("Factor name ",labelModel[fac],
                              "does not appear in data.frame"))
      }
   }
   return(indat)
} # end of makecategorical

#' @title makeLabel Convert a vector of numbers or strings into a single label
#'
#' @description makeLabel Convert a vector of numbers of strings into a single
#'   label
#' @param invect the vector of numbers or strings to be converted into a
#'   single string.
#' @param insep defaults to '_' but can be any selected character used to
#'   separate each value
#' @return a character string containing the invect as a single string.
#' @export makeLabel
#' @examples
#' \dontrun{
#'  x <- c(1,2,3,4,5)
#'  makeLabel(x)
#'  makeLabel(x,"-")
#' }
makeLabel <- function(invect,insep="_") {
   invect <- as.character(invect)
   invect <- invect[nchar(invect) > 0]
   nlab <- length(invect)
   ans <- invect[1]
   if (nlab > 1) for (i in 2:nlab) ans <- paste(ans, invect[i], sep=insep)
   return(ans)
}  # end of makeLabel


#' @title saveresults sends the tables generated to a text file in resultdir
#'
#' @description savereults saves all the tables generated to a text file
#'     named after the species and fishery and puts it into resultdir. The
#'     outputs include for all data relating to the species: catch-by-method,
#'     catch-by-zone, catch-by-fishery, then only for the data selected for
#'     the standardization, the: catch-by-method, catch-by-zone, and the
#'     catch-by-DepCat, but also the number of observations modified by each
#'     of the selction criteria, the data summary, and the raw output from the
#'     standardizatio.
#'
#' @param sps the data.frame containing all data relating to the species
#' @param answer the list obtained from 'selectdata' containing the data.frame
#'     of the selected data, and the nobs table containing how different
#'     selection criteria affect the number of observations.
#' @param out the output from standLM or dosingle containing the
#'     standardization
#' @param mods the mods defined for the standardization
#' @param splabel the name of the species and fishery
#' @param resdir the path to the results directory
#'
#' @return nothing, but it does write a text file to the results directory.
#' @export
#'
#' @examples
#' \dontrun{
#' print("this will take some doing")
#' }
saveresults <- function(sps,answer,out,mods,splabel,resdir) {
   cmeth <- tapply(sps$catch_kg,list(sps$Year,sps$Method),sum,na.rm=T)/1000
   czone <- tapply(sps$catch_kg,list(sps$Year,sps$Zone),sum,na.rm=T)/1000
   cfishery <- tapply(sps$catch_kg,list(sps$Year,sps$Fishery),sum,na.rm=T)/1000
   sps1 <- answer$sps1
   nobs <- answer$nobs
   LimCfishery <- tapply(sps1$catch_kg,list(sps1$Year,sps1$Fishery),sum,na.rm=T)/1000
   LimCZone <- tapply(sps1$catch_kg,list(sps1$Year,sps1$Zone),sum,na.rm=T)/1000
   LimCDep <- tapply(sps1$catch_kg,list(sps1$Year,sps1$DepCat),sum,na.rm=T)/1000
   #datasum <- makedatasum(sps,sps1,out)
   fileout <- paste(resdir,splabel,".txt",sep="")
   if (file.exists(fileout)) file.remove(fileout)
   sink(fileout)
   print(splabel,quote=F)
   cat("\n")
   print(nobs)
   cat("\n")
   print("Models Used",quote=F)
   cat("\n")
   print(mods)
   cat("\n")
   print("Fishery Summary",quote=F)
   cat("\n")
   #print(datasum)
   cat("\n")
   print("Catch by Year and Method for All Data",quote=F)
   cat("\n")
   print(cmeth)
   cat("\n")
   print("Catch by Year and Zone All data",quote=F)
   cat("\n")
   print(czone)
   cat("\n")
   print("Catch by Year and by Fishery All data",quote=F)
   cat("\n")
   print(cfishery)
   cat("\n")
   print("Catch by Year and Fishery Selected data",quote=F)
   cat("\n")
   print(LimCfishery)
   cat("\n")
   print("Catch by Year and by Zone, selected data",quote=F)
   cat("\n")
   print(LimCZone)
   cat("\n")
   print("Catch by Year and by DepCat, selected data",quote=F)
   cat("\n")
   print(LimCDep)
   cat("\n")
   print("Standardization Output",quote=FALSE)
   cat("\n")
   for (i in 1:7){
      print(names(out)[i],quote=FALSE)
      cat("\n")
      print(out[[i]])
      cat("\n")
   }
   sink()
   return(fileout)
}  # end of saveresults

#' @title scaleCE scales an input vector of CPUE to a mean of one x avCE
#'
#' @description scaleCE scales a vector of CPUE to a mean of
#'     one or avCE. The use of a mean of one means that visual comparisons
#'     between different time-series becomes visually simplified. The
#'     avCE option could be used to scale the CPUE to the average
#'     geometric mean - so as to put it on the nominal scale
#'
#' @param invect a vector of linear scale CPUE
#' @param avCE defaults to one but can be set to any particular value
#'
#' @return a vector of CPUE re-scaled to a mean of one or avCE
#' @export
#'
#' @examples
#' \dontrun{
#'  ce <- c(0.4667187,1.2628564,0.8442146,0.9813531, 0.5554076,0.7426321)
#'  scaleCE(ce)
#'  scaleCE(ce,100.0)
#' }
scaleCE <- function(invect,avCE=1.0) {
   average <- mean(invect,na.rm=TRUE)
   ans <- avCE * (invect/average)
   return(ans)
} # end of scaleCE

#' @title standLM Conduct a standarization on log-transformed CPUE using lm
#'
#' @description standLM conducts a standarization on log-transformed CPUE data using lm.
#'     A number of predefined models are entered (use makemodels) with the raw
#'     data and a label. It produces an object of class outce, which is a list
#'     of length 9 containing a matrix of Results, a matrix of StErr, a matrix
#'     of WhichM, which describes the relative performance of each model,
#'     Optimum, which identifies the optimal model by name (the last factor
#'     included) and column in the Results matrix, the number of year Nyrs,
#'     and the Label included as inlab, the a matrix of parameters, a full
#'     copy of the optModel, and a list of Models.
#' @param inmods is the list of models to be analysed along with their
#'     respective labels - can be generated using makemodels
#' @param indat is the data.frame containing the raw data, which must contain
#'     columns with the same names as the factors being included in the models.
#' @param inlab is an optional label that is added to text and plotted outputs,
#'     which defaults to the empty string
#' @param console defaults to TRUE; if TRUE write each model to the screen
#'      as each model runs, which can mess up auto-documenting documents.
#' @return a list containing the standardization of class 'outce'
#' @export
#' @examples
#' \dontrun{
#'  data(abeg)
#'  splabel = "Abalone"
#'  print(tapply(ab$catch,list(ab$year,ab$block),sum,na.rm=TRUE)/1000)
#'  labelM <- c("year","diverID","month","block","boatID")
#'  ab1 <- makecategorical(labelM,ab)
#'  mods <- makemodels(labelM)
#'  out <- standLM(mods,ab1,splabel)
#'  round(out$Results,4)
#'  round(out$WhichM,4)
#'  plotstand(out,bars=TRUE)
#' }
standLM <- function(inmods,indat,inlab="",console=TRUE){
   # inmods=mod; indat=ab3; inlab="Block13E"; console=TRUE
   NModels <- length(inmods)
   labelM <- inmods[[NModels]]
   NModels <- NModels - 1
   ans <- vector("list",NModels)
   names(ans) <- labelM
   Yearnames <- levels(as.factor(indat[,labelM[1]]))
   Nyrs <- length(Yearnames)
   rows <- c("AIC","RSS", "MSS","Nobs", "Npars","adj_r2","%Change")
   WhichM <- matrix(nrow=length(rows),ncol=NModels,dimnames=list(rows,labelM))
   rows <- Yearnames
   Results <- matrix(nrow=Nyrs,ncol=NModels,dimnames = list(rows,labelM))
   ResStErr <- matrix(nrow=Nyrs,ncol=NModels,dimnames = list(rows,labelM))
   modellist <- vector("list",NModels)
   names(modellist) <- inlab
   geomod <- inmods[[1]]   # the Year term
   model <- lm(geomod,data=indat)    # used to estimate the total sum of squared
   totalssq <- sum(anova(model)[2]) # used in the %variation estimates
   for (index in 1:NModels) {  # index <- 1
      if (console) cat(as.character(inmods[[index]]),"\n")
      model <-  lm(inmods[[index]],data=indat)
      modellist[[index]] <- model
      modelsum <- summary(model)
      mat <- modelsum$coefficients
      ans <- getfact(mat,labelM[1])
      Results[,index] <- scaleCE(ans[,"Coeff"])
      ResStErr[,index] <- ans[,"SE"]
      anv <- anova(model)
      RSS <- tail(anv$"Sum Sq",1)
      WhichM["RSS",index] <- RSS
      WhichM["MSS",index] <- totalssq-RSS    # Model SS
      df <- unlist(anv[1])
      ndf <- length(df)
      nobs <- sum(df) + 1
      npars <- sum(df[1:(ndf-1)]) + 1
      WhichM["AIC",index] <- nobs * log(RSS/nobs) + (2.0 * npars)
      WhichM["Nobs",index] <- nobs
      WhichM["Npars",index] <- npars   #npars
      WhichM["adj_r2",index] <- 100.0 * modelsum$adj.r.squared  # adj_r2
   }
   for (index in 2:NModels) {  # calculate the %Change
      WhichM["%Change",index] <- WhichM["adj_r2",index]-WhichM["adj_r2",index-1]
   }
   WhichM["%Change",1] <- 0.0
   pickinter <- grep(":",labelM)
   if (length(pickinter) > 0) {
      lastsimple <- pickinter[1] - 1
      WhichM["%Change",pickinter] <- WhichM["adj_r2",pickinter] - WhichM["adj_r2",lastsimple]
   }
   optimum <- which.max(WhichM["adj_r2",])
   msg <- paste("Optimum model ",inmods[optimum],sep="")
   if (console) print(msg,quote=F)
   if (NModels >= 3) {
      count <- 0
      for (i in 3:NModels) { # i <- NModels
         if (WhichM["%Change",i] > WhichM["%Change",(i-1)]) count <- count + 1
      }
      if ((count > 0) & (console)) {
         print("Models need re-ordering with the recommended order being:",quote=F)
         msg <- c(labelM[1],labelM[order(WhichM["%Change",2:NModels],decreasing=TRUE)+1])
         print(msg,quote=F)
      }
   }
   out <- list(Results,ResStErr,WhichM,optimum,Nyrs,inlab,
               summary(modellist[[optimum]]),modellist[[optimum]],modellist)
   names(out) <- c("Results","StErr","WhichM","Optimum","Nyrs","Label",
                   "parameters","optModel","Models")
   class(out) <- "outce"
   return(out)
} # end of standLM

#' @title summary.outce S3 function to summarize standardization results
#'
#' @description summary.outce an S3 function to summarize out the primary
#'     results from a standardization. It includes the structure of the
#'     output so other things can be obtained if wished.
#'
#' @param x is the output from a standardization perhaps using standLM.
#'     It must be of class outce  an S3 class
#' @return Summarizes the contents of the output from  standardization,
#'     this includes
#' \describe{
#'   \item{Structure}{this lists the top-level structure of the object
#'     output from the standardizations from standLM}
#'   \item{Parameters by Model}{This tabulates the yearly parameters for
#'     each of the models tested, the 'mods' input to standLM}
#'   \item{Std Errors by Model}{The matching standard errors for each of the
#'     models, one column for each column of paramters; large sample sizes
#'     mean these are usually an significant under-estimate of the
#'     true variaiton}
#'   \item{Model Diagnostic Properties}{A table of the statistical performance
#'     of each model, allowing the identification of the optimum model}
#'   \item{Optimum model}{identifies which column relates to the optimum
#'     statistical model}
#'   \item{All parameters}{The un-back-transformed parameters for the complete
#'     optimum model}
#' }
#' @exportMethod summary.outce
#' @examples
#' \dontrun{
#'  data(sps)
#'  splabel = "Blue-Eye"
#'  labelM <- c("Year","Vessel","Month")
#'  sps1 <- makecategorical(labelM,sps)
#'  mods <- makemodels(labelM)
#'  out <- standLM(mods,sps1,splabel)
#'  summary(out)
#' }
summary.outce <- function(x) {
   cat("Structure of Analytical Output \n")
   str(x,max.level=1)
   cat("\n")
   cat("Parameters by Model \n")
   print(x$Results)
   cat("\n")
   cat("Std Errors by Model \n")
   print(x$StErr)
   cat("\n")
   cat("Model Diagnostic Properties \n")
   print(round(x$WhichM,3))
   cat("\n")
   cat("Optimum Model \n")
   print(x$Optimum)
   cat("\n")
   cat("All Un-backtransformed parameters from the optimum model \n")
   print(x$Parameters$coefficients)
} # end of summary.outce

#' @title tapsum simplifies the use of tapply for summarizing variables
#'
#' @description data exploration commonly uses the tapply function and tapsum
#'     simplifies its use when obtaining the sum of any variable relative to
#'     other variables. For example it is common to want the total catch by
#'     year and, for example, Month, DepCat, Zone, etc.
#'
#' @param indat the data.frame containing the raw fishery data
#' @param first the variable name (in quotes) being summed
#' @param second the first grouping variable
#' @param third the second grouping variable, defaults to NA
#' @param div defaults to 1000 to change Kg to tonnes. set to 1.0 or NA to
#'     avoid its influence
#'
#' @return a vector or matrix of sums of the pickvar by the first and optionally
#'      the second grouping variable
#' @export
#'
#' @examples
#' \dontrun{
#'   data(sps)
#'   tapsum(sps,"catch_kg","Year","Month")
#' }
tapsum <- function(indat,first,second,third=NA,div=1000) {
  if (is.na(third)) {
    result <- tapply(indat[,first],indat[,second],sum,na.rm=TRUE)
  } else {
    result <- tapply(indat[,first],list(indat[,second],indat[,third]),
                     sum,na.rm=TRUE)
  }
  if (is.numeric(div)) result <- result/div
  return(result)
} # end of tapsum

#' @title toXL copies a data.frame or matrix to the clipboard
#'
#' @description toXL copies a data.frame or matrix to the clipboard
#'    so one can then switch to Excel and just type <ctrl> + V to paste the
#'    data in place
#'
#' @param x a vector or matrix
#' @param output a boolean determining whether to print the object to the
#'    screen as well as the clipboard; defaults to TRUE
#' @return Places the object 'x' into the clipboard ready for pasting
#' @export
#' @examples
#' datamatrix <- matrix(data=rnorm(100),nrow=10,ncol=10)
#' colnames(datamatrix) <- paste0("A",1:10)
#' rownames(datamatrix) <- paste0("B",1:10)
#' toXL(datamatrix,output=TRUE)
toXL <- function(x,output=TRUE) {
   write.table(x,"clipboard",sep="\t")
   if(output) print(x)
}

#' @title turnover estimate turnover of vessels from catch by vessel by year data
#'
#' @description turnover estimates turnover of vessels from catch by vessel
#'     by year data. To specify the minimum number of years that a vessel
#'     needs stay in the fishery, then give a value to the variable minyrs.
#'     needs MODIFICATION
#'
#' @param x A matrix of a continuous numeric property by year,
#'    the original usage was to plot catch-by-vessel against year
#' @param minyrs limits the analysis to those vessels that remain in the
#'    fishery for at least minyrs years - which would eliminate the occasional
#'    opportunistic fisher who only fishes for one or two years, or whatever
#'    minimum is selected. Vessels with zero catches are not included in case
#'    zeros and NAs are counted as starting and leaving the fishery.
#'
#' @return a matrix of years by Continue, Leave, Start, Total
#' @export
#' @examples
#' \dontrun{
#' library(r4cpue)
#' data(sps)
#' cbv <- tapply(sps$catch_kg,list(sps$Vessel,sps$Year),sum,na.rm=TRUE)/1000
#' dim(cbv)
#' early <- rowSums(cbv[,1:6],na.rm=TRUE)
#' late <- rowSums(cbv[,7:14],na.rm=TRUE)
#' cbv1 <- cbv[order(late,-early),]
#' plotprep(width=7,height=6)
#' yearBubble(cbv1,ylabel="Catch by Trawl",vline=2006.5,diam=0.2)
#' turnover(cbv)
#' }
turnover <- function(x,minyrs=1) {
   years <- as.numeric(colnames(x))
   ny <- length(years)
   count <- apply(x,1,countgtzero)
   pick <- which(count == 0)
   if (length(pick) > 0) {
      warning("Some Rows only have zeros or NAs")
      x <- x[-pick,]
   }
   pick <- which(count > (minyrs - 1))
   if (length(pick) > 0) x <- x[pick,]
   columns <- c("Continue","Leave","Start","Total")
   turnover <- matrix(0,nrow=ny,ncol=length(columns),
                      dimnames=list(years,columns))
   turnover[1,1] <- length(which(x[,1] > 0))
   for (yr in 2:ny) {
      pair <- x[,(yr-1):yr]
      pickC <- which((pair[,1] > 0) & (pair[,2] > 0))
      pickL <- which((pair[,1] > 0) & ((is.na(pair[,2])) |
                                          (pair[,2] < 0.001)))
      pickS <- which(((is.na(pair[,1])) | (pair[,2] < 0.001)) &
                        (pair[,2] > 0))
      turnover[yr,1:3] <- c(length(pickC),length(pickL),length(pickS))
   }
   turnover[,4] <- turnover[,1] + turnover[,3]
   return(turnover)
} # end of turnover

#' @title yearNA - counts NAs per year in each numeric field in a data.frame
#'
#' @description yearNA - counts the number of NAs in each year of each numeric
#'    field in a data.frame and outputs the results as a matrix
#' @param indat the data.frame whose numeric fields are to be considered
#' @param years identifies the name of the "Year" field in the data.frame
#' @param empty logical default=FALSE, determines whether columns which have
#'    no NA present are printed or not
#' @return a matrix of years x numeric fields with the number of records per
#'    field per year as reference
#' @export
#' @examples
#' \dontrun{
#'  year <- sort(rep(1990:1994,5))
#'  columns <- c("year","Var1","Var2","Var3")
#'  dat <- matrix(runif(100),nrow=25,ncol=4,dimnames=list(year,columns))
#'  dat[trunc(100*runif(20))] <- NA
#'  dat[,1] <- year
#'  print(dat)
#'  yearNA(as.data.frame(dat),years="year")
#' }
yearNA <- function(indat,years="Year",empty=FALSE) {
   records <- table(indat[,years])
   nna <- function(x) sum(is.na(x))
   columns <- colnames(indat)
   clas <- sapply(indat, class)
   numbers <- c("integer", "numeric")
   pick <- which(clas %in% numbers)
   num <- length(pick)
   ans <- NULL
   for (i in 1:num) {
      ans <- cbind(ans,tapply(indat[,columns[pick[i]]],indat[,years],nna))
   }
   ans <- cbind(ans,records)
   colnames(ans) <- c(columns[pick],"Records")
   if (!empty) {
      pick <- which(colSums(ans,na.rm=TRUE) > 0)
      if (length(pick) > 0)  ans <- ans[,pick]
   }
   return(ans)
}  #end of yearNA

#' @title yearZero - examine listed variables for zeros and NAs
#'
#' @description yearZero - examines an input data.frame of matrix in the
#'    variables listed in the parameter 'label' and counts the zeros,
#'    NAs, and those >0. It counts these relative to a 'Year' variable,
#'    or any grouping variable identified using the 'years' parameter.
#' @param indat the data.frame or matrix to be examined
#' @param years the grouping variable for counts to be compared across,
#'    defaults to 'Year'
#' @param label the collection of column names which are to be examined,
#'    defaults to c('catch_kg','Effort'), which is suitable for SESSF
#'    data sets.#'
#' @return A matrix of the variables in 'label' with a column for each of
#'    Zeros, >0, and NAs, with a final column of the number of records
#'    per grouping variable value.
#' @export
#'
#' @examples
#' \dontrun{
#'  insps <- matrix(runif(30),nrow=15,ncol=2)
#'  insps[trunc(runif(7)*30)+1] <- 0
#'  insps[trunc(runif(7)*30)+1] <- NA
#'  insps <- cbind(sort(rep(2000:2004,3)),insps)
#'  colnames(insps) <- c("Year","catch_kg","Effort")
#'  print(round(insps,4))
#'  yearZero(insps)
#' }
yearZero <- function(indat,years="Year",label=c("catch_kg","Effort")) { # indat <- sps
   columns <- colnames(indat)
   pick <- which(columns %in% label)
   npick <- length(pick)
   if (npick != length(label)) warning(paste0("Only ",columns[pick]," found"))
   if (npick == 0) stop("None of the listed variable found; rename them")
   records <- table(indat[,years])
   nna <- function(x) sum(is.na(x))  # sums the logical outcomes
   gtzero <- function(x) sum(x > 0,na.rm=TRUE)
   ans <- NULL
   for (i in 1:npick) {
      gtz <- tapply(indat[,columns[pick[i]]],indat[,years],gtzero)
      nas <- tapply(indat[,columns[pick[i]]],indat[,years],nna)
      zeros <- records - gtz - nas
      ans <- cbind(ans,zeros,gtz,nas)
   }
   ans <- cbind(ans,records)
   tmplab <- sort(c(paste0(columns[pick],"NA"),paste0(columns[pick],"GT0"),paste0(columns[pick],"0")))
   colnames(ans) <- c(tmplab,"Records")
   return(ans)
} # end of yearZero



