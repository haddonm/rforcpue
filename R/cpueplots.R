

#' @title diagnosticPlot produces diagnostic plots of the regression.
#'
#' @description  diagnosticPlot produces diagnostic plots of the regression.
#'   these include a plot of the residuals agsinst the predicted values, the
#'   normal Q-Q plot, a histogram of the Observed Log(CPUE) with a fitted
#'   normal distribution, and a histogram of the predicted Log(CPUE) with a
#'   fitted normal distribution and the normal distribution from the observed
#'   data.
#'
#' @param inout is the list from standLM.
#' @param indat is the data.frame used in the standardization.
#' @param inmodel is a single number identifying the model inside 'inout' to
#'    plotted, defaults to inout's Optimum
#' @param depend the dependent variable defaults to "LnCE"
#' @param inlabel provides the means of giving a title to the 2 x 2 plot
#' @param dorug default=TRUE, should the rug of points be included in 
#'     the QQplot?
#'
#' @return a 2 x 2 plot of the residuals, the Q-Q plot, and the distributions
#'    of the logged observed and predicted values of CPUE.
#' @export diagnosticPlot
#' @examples
#' \dontrun{
#' data(sps)
#' splabel = "Species"
#' sps$Year <- factor(sps$Year)
#' sps$Month <- factor(sps$Month)
#' sps$Vessel <- factor(sps$Vessel)
#' labelM <- c("Year","Vessel","Month")
#' mods <- makemodels(labelM)
#' out <- standLM(mods,sps,splabel)
#' diagnosticPlot(out,3,sps,splabel)
#' }
diagnosticPlot <- function(inout, indat, inmodel=inout$Optimum,
                           depend="LnCE",inlabel="",dorug=TRUE) {
  if ("outce" %in% class(inout)) {
    model <- inout$Models[[inmodel]]
  } else {
    model <- inout
  }
  resids <- model$residuals
  fitted.vals <- model$fitted.values
  labs <- 1.1
  par(mfrow= c(2,2))
  par(mai=c(0.5,0.5,0.3,0.05),oma=c(0,0,0,0))
  par(cex=0.85, mgp=c(1.5,0.35,0), font.axis=7)
  plot(fitted.vals,resids, main="",pch=1,
       ylab="", xlab="",panel.first=grid())
  title(main=list(inlabel, cex=labs, font=7),
        xlab=list("Predicted Values", cex=labs, font=7),
        ylab=list("Residuals", cex=labs, font=7))
  abline(h=0.0,col=2)
  qqnorm(resids, ylab=list("Std Deviance Resid.", cex=labs, font=7),
         xlab=list("Theoretical Quantiles", cex=labs, font=7),
         main=list("Normal Q-Q Plot",cex=labs,font=7),panel.first=grid())
  qqline(resids, col=2,lwd=2)
  if (dorug) rug(resids,ticksize=0.02,col="royal blue")
  abline(v=c(-1.96,1.96),col="green")
  par(mai=c(0.5,0.5,0.1,0.2))
  meance <- mean(indat[,depend],na.rm=TRUE)
  sdce <- sd(indat[,depend],na.rm=TRUE)
  lower <- floor(min(indat[,depend],na.rm=TRUE))
  upper <- ceiling(max(indat[,depend],na.rm=TRUE))
  lowerf <- floor(min(fitted.vals,na.rm=TRUE))
  upperf <- ceiling(max(fitted.vals,na.rm=TRUE))
  minx <- min(lower,lowerf)
  maxx <- max(upper,upperf)
  cebin <- 0.25
  x <- seq(minx,maxx,0.01)
  y <- dnorm(x,mean=meance,sd=sdce)
  bins <- seq(minx,maxx,cebin)
  histout <- hist(indat[,depend],breaks=bins,main="",xlab="",ylab="")
  totn <- sum(histout$counts)
  ymax <- max(histout$counts)
  lines(x,(totn/(1/cebin))*y,col=4,lwd=2)
  title(xlab=list(paste0("Observed ",depend), cex=labs, font=7),
        ylab=list("Frequency", cex=labs, font=7))
  text(minx+5*cebin,0.975*ymax,paste("Mean  ",round(meance,3),sep=""),cex=labs,font=7)
  text(minx+5*cebin,0.9*ymax,paste("StDev ",round(sdce,3),sep=""),cex=labs,font=7)
  meancef <- mean(fitted.vals,na.rm=TRUE)
  sdcef <- sd(fitted.vals,na.rm=TRUE)
  yf <- dnorm(x,mean=meancef,sd=sdcef)
  histoutf <- hist(fitted.vals,breaks=bins,main="",xlab="",ylab="")
  totnf <- sum(histoutf$counts)
  ymaxf <- max(histoutf$counts)
  lines(x,(totnf/(1/cebin))*yf,col=2,lwd=2)
  lines(x,(totn/(1/cebin))*y,col=4,lwd=2)
  title(xlab=list("Predicted Log(CE)", cex=labs, font=7),
        ylab=list("Frequency", cex=labs, font=7))
  text(lower+6*cebin,0.975*ymaxf,paste("Mean  ",round(meancef,3),sep=""),cex=labs,font=7)
  text(lower+6*cebin,0.9*ymaxf,paste("StDev ",round(sdcef,3),sep=""),cex=labs,font=7)
}  # end of diagnosticPlot

#' @title examinedata plots the yearly distributions of catch and effort from 'x'
#' 
#' @description examinedata plots the yearly distributions of the number of 
#'     records, the catch, the effort, and the geometric mean cpue by year for 
#'     the data in the fishery dependent data in the data.frame x. It also 
#'     tabulates these values.  Each of these are stored either as .csv or .png 
#'     files 'in rundir'. It also logs these files in resfile, which opens the 
#'     possibility of displaying all results in a local webpage under a tab 
#'     labelled 'yeardata'. The 'rundir', 'resfile', and 'runname' should be the 
#'     same as those used by makehtml to prepare a directory and 'resfile' to 
#'     store the files to be used to make a local webpage of results. 
#'
#' @param x  the data.frame of fishery dependent data
#' @param catch the name used to identify the catch factor of the species
#' @param labcatch the axis label for catch
#' @param effort the name used to identify the effort factor of the species
#' @param labeffort the axis label for effort
#' @param cpue the name used to identify the cpue factor of the species
#' @param labcpue the axis label for cpue
#' @param LnCE column name of log(cpue) in x, default='LnCE'
#' @param labLcpue the axis label for log(cpue)
#' @param year the name used to identify the year factor of the species
#' @param spsname the name of the species of interest.
#' @param rundir the full path of the results directory into which the plot files
#'     and the .csv files for the tables are to be stored. 
#' @param runname the name of the particular run being made 
#' @param plotnum the number of rows an columns of plots used, default=c(1,1)
#' @param wid the width of each plot, default=6
#' @param hgt the height of each plot, default=5
#' @param xlimit a veector of 12 containing the xlim values, Lbound, Rbound, 
#'     and inc, for four of the plots. Default is rep(NA,12). For the 
#'     third plot no inc value is required so set it to zero (it will be ignored)
#' @param category default='yeardata' but in case this is called for a subset
#'     of data then it can be set to whatever you wish
#' 
#' @return nothing but it does add 4 plots and two tables to the results
#' @export
#'
#' @examples
#' print("wait on internal data")
examinedata <- function(x,catch="catch",labcatch="catch",
                        effort="hours",labeffort="effort",
                        cpue="cpue",labcpue="cpue",
                        LnCE="LnCE",labLcpue="log(cpue)",
                        year="year",spsname="",
                        rundir="",runname="",
                        plotnum=c(1,1),wid=6,hgt=5,
                        xlimit=rep(NA,12),
                        category="yeardata") { 
# x=ab;catch="catch";labcatch="catch";effort="hours";labeffort="effort"
#  cpue="cpue";labcpue="cpue";LnCE="LnCE";labLcpue="log(cpue)"
#  year="year";spsname="";rundir=rundir;runname="east"
#  plotnum=c(1,1);wid=6;hgt=5;xlimit=rep(NA,12);category="yeardata"
  
  limitx <- matrix(xlimit,nrow=4,ncol=3,byrow=TRUE)
  records <- as.numeric(table(x[,year]))
  cby <- tapply(x[,catch],x[,year],sum,na.rm=TRUE)/1000
  eby <- tapply(x[,effort],x[,year],sum,na.rm=TRUE)
  geoby <-  tapply(x[,cpue],x[,year],geomean)
  annsum <- cbind(records=records,catch=cby,effort=eby,geomet=geoby)
  
  filen <- filenametopath(rundir,paste0("catch_by_year_",runname,".png"))
  plotprep(width=wid,height=hgt,newdev=FALSE,filename=filen,cex=0.9,verbose=FALSE)
  ans <- histyear(x,xlimit=limitx[1,],years="year", plots=plotnum,
                 pickvar=catch,varlabel=labcatch,normadd=FALSE,left=FALSE)
  caption <- paste0("Distribution of positive catches for ",spsname," by year.")
  addplot(filen,rundir=rundir,category=category,caption)
  
  filen <- filenametopath(rundir,paste0(effort,"_by_year_",runname,".png"))
  plotprep(width=wid,height=hgt,newdev=FALSE,filename=filen,cex=0.9,verbose=FALSE)
  ans <- histyear(x,xlimit=limitx[2,],years="year",plots=plotnum,
                  pickvar=effort,varlabel=labeffort,
                  normadd=FALSE,left=FALSE)
  caption <- paste0("Distribution of effort levels for ",spsname," by year.")
  addplot(filen,rundir=rundir,category=category,caption)
  
  filen <- filenametopath(rundir,paste0("catch_by_effort_",runname,".png"))
  plotprep(width=wid,height=hgt,newdev=FALSE,filename=filen,cex=0.9,verbose=FALSE)
  xyplotyear(x,yvar=catch,xvar=effort,year=year,plotnum=plotnum,xlim=limitx[3,1:2],
             addline=TRUE,origin=TRUE,xlab=labeffort,ylab=labcatch)
  caption <- paste0("Distribution of catch vs effort for ",spsname," by year. ",
                    "Lines are linear regressions through each year's data-set.")
  addplot(filen,rundir=rundir,category=category,caption)
  
  pickCE <- which((x[,catch] > 0) & (x[,effort] > 0))
  filen <- filenametopath(rundir,paste0("annual_geometric_mean_CPUE_",runname,".png"))
  plotprep(width=wid,height=hgt,newdev=FALSE,filename=filen,cex=0.9,verbose=FALSE)
  ans <- histyear(x[pickCE,],xlimit=limitx[4,],years="year",plots=plotnum,
                  pickvar=LnCE,varlabel=labLcpue,left=FALSE)
  caption <- paste0("Annual Geometric Mean CPUE for ",spsname," by year.")
  addplot(filen,rundir=rundir,category=category,caption)
  
  yrs <- as.numeric(rownames(annsum))
  filen <- filenametopath(rundir,paste0("year_summary_",runname,".png"))
  label <- colnames(annsum)
  plotprep(width=wid,height=hgt,newdev=FALSE,filename=filen,cex=0.9,verbose=FALSE)
  parset(plots=c(2,2))
  for (i in 1:4) {
    ymax <- getmax(annsum[,i])
    plot(yrs,annsum[,i],type="l",lwd=2,ylim=c(0,ymax),yaxs="i",ylab=label[i],
         xlab="",panel.first=grid())
  }
  caption <- paste0("Plots of records, catch, effort, and geometric mean cpue ",
                    "by year for ",spsname,".")
  addplot(filen,rundir=rundir,category=category,caption)

  filen <- paste0("year_summary_",runname,".csv")
  addtable(annsum,filen,rundir,category=category,
           caption="Annual summary for scallop Hammerhead sharks.")
  } # end of examinedata

#' @title examinevar tabulates and plots the properties of the input variable
#' 
#' @description examinevar tabulates and plots the properties of an input
#'     variable contained within an input data.frame. This function is designed
#'     to simplify the characterization of fisheries dependent data prior to 
#'     conducting a statistical standardization. It does this by plotting for 
#'     the input variable (invar), the records-by-year, the catch-by-year, and 
#'     the effort-by-year. for each level of 'invar' it also counts the records
#'     across years, and sums the catches and effort across years. Each of these 
#'     are stored either as .csv or .png files 'in rundir'. It also logs these
#'     files in resfile, which opens the possibility of displaying all results
#'     in a local webpage under a tab labelled by the contents of 'invar'. The 
#'     'rundir', 'resfile', and 'runname' should be the same as those used by
#'     makehtml to prepare a directory and 'resfile' to store the files to be
#'     used to make a local webpage of results.
#'
#' @param x the data.frame of fishery dependent data
#' @param invar the name of the variable or factor whose properties are to be
#'     examined
#' @param catch the name used to identify the catch factor of the species
#' @param effort the name used to identify the effort factor of the species
#' @param cpue the name used to identify the cpue factor of the species
#' @param year the name used to identify the year factor of the species
#' @param spsname the name of the species of interest.
#' @param rundir the full path of the results directory into which the plot files
#'     and the .csv files for the tables are to be stored. 
#' @param runname the name of the particular run being made 
#' @param addlines the number of lines to be added at the top of the plots, so
#'     The year totals can be included. Default=5, the number needed will depend
#'     on how many unique values there are for 'invar'. If there are many then 
#'     more lines may need to be added to space out the year totals.
#' @param wid the width of each plot, default=6
#' @param hgt the height of each plot, default=5
#' @param textcex what text size to use in the plots, default=0.75
#' @param category default = invar, but can be changed if required
#'
#' @return nothing but it does generate 4 plots and 2 tables into rundir
#' @export
#'
#' @examples
#' print("wait on internal data")
#' # x=ab;invar="block";spsname="blacklip";rundir=rundir;runname="zone"
#' # catch="catch";effort="hours";cpue="cpue";year="year";addlines=3;wid=6;hgt=5;category=invar
examinevar <- function(x,invar="",catch="catch",effort="hours",cpue="cpue",
                       year="year",spsname="",
                       rundir,runname,
                       addlines=5,wid=6,hgt=5,textcex=0.75,
                       category=invar) { 
  filen <- filenametopath(rundir,paste0("records_by_",invar,"_",runname,".png"))
  records <- as.matrix(table(x[,invar],x[,year])) 
  ymax <- max(records,na.rm=TRUE)
  plotprep(width=wid,height=hgt,newdev=FALSE,filename=filen,cex=0.9,verbose=FALSE)
  categoryplot(records,ylab=paste0("Total records by ",invar," by Year"),
               mult=1/ymax,addtotal=TRUE,addlines=addlines,textcex=textcex)
  caption <- paste0("The relative number of records per ",category," for ",spsname,
                    " by year. The numbers are year totals")
  addplot(filen,rundir=rundir,category=category,caption)
  
  filen <- filenametopath(rundir,paste0("catch_by_",invar,"_",runname,".png"))
  catchV <- tapply(x[,catch],list(x[,invar],x[,year]),sum,na.rm=TRUE)/1000
  ymax <- getmax(catchV)
  plotprep(width=wid,height=hgt,newdev=FALSE,filename=filen,cex=0.9,verbose=FALSE)
  categoryplot(catchV,ylab=paste0("Total catch by ",invar," by Year"),
               mult=1/ymax,addtotal=TRUE,addlines=addlines,textcex=textcex)
  caption <- paste0("The relative ",catch," per ",invar," for ",spsname,
                    " by year. The numbers are year totals.")
  addplot(filen,rundir=rundir,category=category,caption)
  
  filen <- filenametopath(rundir,paste0(effort,"_by_",invar,"_",runname,".png"))
  effortV <- tapply(x[,effort],list(x[,invar],x[,year]),sum,na.rm=TRUE)/1000
  ymax <- getmax(effortV)
  plotprep(width=wid,height=hgt,newdev=FALSE,filename=filen,cex=0.9,verbose=FALSE)
  categoryplot(effortV,ylab=paste0("Total ",effort," by ",invar," by Year"),
               mult=1/ymax,addtotal=TRUE,addlines=addlines,textcex=textcex)
  caption <- paste0("The relative ",effort," per ",invar," for ",spsname,
                    " by year. The numbers are year totals.")
  addplot(filen,rundir=rundir,category=category,caption)
  
  pick <- which((x[,catch] > 0) & (x[,effort] > 0))
  ceyr <- tapply(x[pick,cpue],list(x[pick,invar],x[pick,year]),geomean)
  ymax <- getmax(ceyr)
  filen <- filenametopath(rundir,paste0("geometric_CPUE_by_",invar,"_",runname,".png"))
  plotprep(width=wid,height=hgt,newdev=FALSE,filename=filen,cex=0.9,verbose=FALSE)
  categoryplot(ceyr,mult=1/ymax,ylab=paste0("Geometric Mean CPUE by ",invar),
               textcex=textcex)
  caption <- paste0("Geometric Mean CPUE by ",invar," for ",spsname," by year.")
  addplot(filen,rundir=rundir,category=category,caption)
  
  yrsact <- apply(records,1,countgtzero)
  recs <- apply(records,1,sum,na.rm=TRUE) 
  catV <- apply(catchV,1,sum,na.rm=TRUE)
  effV <- apply(effortV,1,sum,na.rm=TRUE)
  ans <- cbind("records"=recs,"catch"=catV,"effort"=effV,"yrsActive"=yrsact)
  ans <- ans[order(ans[,"catch"]),]  
  
  filen <-  paste0("geometric_CPUE_by_",invar,"_",runname,".csv")
  if (nrow(ceyr) > 20) large=TRUE else large=FALSE
  addtable(round(ceyr,5),filen,rundir,category=category,
           caption=paste0("Geometric Mean CPUE by ",invar," for ",spsname,
                          " by year."),big=TRUE)  
  
  filen <- paste0("summary_for_",invar,"_",runname,".csv")
  if (nrow(ans) > 20) large=TRUE else large=FALSE
  addtable(ans,filen,rundir,category=category,
           caption=paste0("Annual summary for ",spsname," across ",invar,"."),
           big=large)
} # end of examinevar


#' @title impactplot Plots relative contribution to CPUE trend of each Factor
#'
#' @description impactplot plots out the contribution to a standardized CPUE
#'    trend of each of the factors included in the standardization. The number on
#'    each graph is the sum of squared differences between the previous model
#'    and the current model - as a measure of the relative effect of the
#'    factor concerned. Positive effects are shown as blue bars, negative
#'    effects are shown as red bars.
#'
#' @param inout is the list output from the standLM function containing
#'    the standardization
#' @param mult default value is 3 - it is used to scale the bars on the plot.
#' @param FY - are the years fishing seasons or calender years; defaults to FALSE,
#'    which assumes calendar years for the x-axis.
#'
#' @return generates a plot of the impact of each factor on the CPUE trend, in
#'   addition, the function returns, invisibly, two tables as a list, the first
#'   $result summarizes the impact of each factor included in the analysis,
#'   including the sum of absolute differences between each model and the one
#'   before it (the top plot is for the complete model). The second table
#'   $deviates lists the actual differences between the particular vaiables
#'   and the previous models.
#' @export impactplot
#' @examples
#' \dontrun{
#'  data(sps)
#'  splabel = "Species"
#'  sps$Year <- factor(sps$Year)
#'  sps$Month <- factor(sps$Month)
#'  sps$Vessel <- factor(sps$Vessel)
#'  labelM <- c("Year","Vessel","Month")
#'  mods <- makemodels(labelM)
#'  out <- standLM(mods,sps,splabel)
#'  impactplot(out,mult=3)
#' }
impactplot <- function(inout,mult=3,FY=FALSE) {
  incoef <- inout[[1]]
  label <- colnames(incoef)                    # get the factor names
  nmods <- length(label)   # How many factors
  if (FY) {
       fyears <- rownames(incoef)
       ny <- length(fyears)
       years <- numeric(ny)
       for (i in 1:ny) years[i] <- as.numeric(unlist(strsplit(fyears[i],"/"))[1])
     } else {
       years <- as.numeric(rownames(incoef))        # get the years used
  }
  nyr <- length(years)
  result <- matrix(0,nrow=nmods,ncol=4,dimnames=list(label,c("SumAbsDev","%Diff","adj_r2","DeltaV")))
  deviates <- matrix(0,nrow=nyr,ncol=nmods,dimnames=list(years,label))
  par(mfrow=c(nmods,1))
  par(mai=c(0.25,0.35,0.05,0.05),oma=c(0.0,1.0,0.05,0.25))
  par(cex=0.85, mgp=c(1.5,0.35,0), font.axis=7)
  ymax <- max(incoef)*1.15   # leave enough room for the labels
  # plot the Geometric Mean trend = model 1
  plot(years,incoef[,1],type="l",ylim=c(0,ymax),ylab="",xlab="",lwd=2,yaxs="i",xaxs="r")
  lines(years,incoef[,nmods],lwd=2,col=2)
  devs <- (incoef[,nmods]-incoef[,1])        # calc diff between trends
  deviates[,1] <- devs
  pick <- which(devs < 0)                    # plot negative deviates as red
  if (length(pick)) { lines(years[pick],mult*abs(devs[pick]),type="h",lwd=4,col=2) }
  pick <- which(devs >= 0)                   # positive deviates as blue
  if (length(pick)) { lines(years[pick],mult*devs[pick],type="h",lwd=4,col=4) }
  sumdev <- sum(abs(devs))
  result[1,1] <- sumdev
  mtext(paste(label[1],round(sumdev,3),sep="  "),3,line=-1,font=6,cex=0.9)
  for (gr in 2:nmods) {                  # now step through the remaining factors
    plot(years,incoef[,gr],type="l",ylim=c(0,ymax),ylab="",xlab="",lwd=2,yaxs="i",xaxs="r")
    lines(years,incoef[,gr-1],lwd=2,col="grey")     # add previous factor
    devs <- (incoef[,gr]-incoef[,gr-1])        # calc diff between trends
    deviates[,gr] <- devs
    pick <- which(devs < 0)                    # plot negative deviates as red
    if (length(pick)) { lines(years[pick],mult*abs(devs[pick]),type="h",lwd=4,col=2) }
    pick <- which(devs >= 0)                   # positive deviates as blue
    if (length(pick)) { lines(years[pick],mult*devs[pick],type="h",lwd=4,col=4) }
    sumdev <- sum(abs(devs))
    result[gr,1] <- sumdev
    mtext(paste(label[gr],round(sumdev,3),sep="  "),3,line=-1,font=6,cex=0.9)
  }
  vlabel <- paste("Standardized Catch Rates ",inout$Label,sep="")
  mtext(vlabel,2,outer=TRUE,line=-0.5,font=6,cex=1.1)
  for (gr in 3:nmods) {                       # calc % difference between abs diff
    result[gr,2] <- 100*result[gr,1]/result[gr-1,1]
  }
  pick <- which(rownames(inout[[3]]) == "adj_r2")
  result[,3] <- inout[[3]][pick,]
  result[,4] <- inout[[3]][(pick+1),]
  deviates[,1] <- rowSums(deviates) # this sums the raw deviates to show the final
  # total change from the full model to the geometric mean
  ans <- list(result,deviates)
  names(ans) <- c("result","deviates")
  return(invisible(ans))
} # end of impactplot

#' @title lefthist draws a histogram up the y-axis
#'
#' @description lefthist translates a histogram from along the x-axis to
#'     flow along the y-axis - it transposes a histogram.
#'
#' @param x a vector of the data to be plotted
#' @param bins the breaks from the histogram, can be a single number of a
#'     sequence of values; defaults to 25
#' @param mult the multiplier for the maximum count in the histogram. Becomes
#'     the upper limit of teh x-axis.
#' @param col the colour for the histogram polygons; default = 2
#' @param lwd the line width for each polygon; default = 1
#' @param width the width of each bar in teh histogram; default = 0.9
#' @param border the colour for the border line; default = 1 = black
#' @param xinc the step size for the x-axis (counts) labels; default= NA,
#'     which means the increment will equal the bin width.
#' @param yinc the step size for the y-axis (breaks) labels; default= 1.
#' @param title the title for the left-histogram; defaults to ""
#' @param xlabel the xlab; defaults to ""
#' @param ylabel the ylab; defaults to "Frequency"
#' @param cex the size of text in teh plot. defaults = 1.0
#' @param textout prints input data range to console; default = FALSE
#' @param hline if this has a value a horizontal line will be plotted;
#'     default = NA
#'
#' @return the output from hist but done so invisibly.
#' @export
#'
#' @examples
#' dat <- rnorm(1000,mean=5,sd=1)
#' dev.new(width=6,height=4,noRStudioGD = TRUE)
#' par(mai=c(0.45,0.45,0.05,0.05))
#' lefthist(dat)
#' lefthist(dat,textout=TRUE,width=0.8,border=3)
lefthist <- function(x,bins=25,mult=1.025,col=2,lwd=1,width=0.9,border=1,
                     xinc=1,yinc=NA,title="",xlabel="Frequency",ylabel="",
                     cex=1.0,textout=FALSE,hline=NA) {
  outh <- hist(x,breaks=bins,plot=FALSE)
  cw <- outh$breaks[2]-outh$breaks[1]
  newcount <- c(outh$counts,0)
  ymax <- max(newcount,na.rm=TRUE) * mult
  nvalues <- length(newcount)
  values <- outh$breaks
  if (is.na(yinc)) yinc <- values[2] - values[1]
  xlabs <- seq(0,(ymax+(2 * xinc)),xinc)
  xlabs <- xlabs[xlabs < ymax]
  plot(seq(0,ymax,length=nvalues),values,type="n",xlim=c(0,ymax),
       xaxs="i",ylim=c(range(values)),yaxs="r",xlab="",ylab="",xaxt="n",yaxt="n")
  grid()
  axis(side=1,at=xlabs,labels=xlabs)
  title(ylab=list(ylabel,cex=cex),xlab=list(xlabel,cex=cex))
  values1 <- seq(values[1],values[nvalues],yinc)
  axis(side=2,at=values1,labels=values1,cex.lab=cex)
  for (i in 1:nvalues) {  # i <- 1
    y1 <- values[i]
    y2 <- values[i] + (cw * width)
    yplot <- c(y1,y1,y2,y2,y1)
    xplot <- c(0,newcount[i],newcount[i],0,0)
    if (is.null(border)) border <- col
    polygon(xplot,yplot,col=col,border=border,lwd=lwd)
  }
  if (!is.na(hline)) abline(h=hline,col=(col+2))

  if (textout) cat("  input data range: ",range(x,na.rm=TRUE),"\n\n")
  return(invisible(outh))
}  # end of lefthist



#' @title plotbasic generates 6 plots of fisheries data and diver data
#'
#' @description plotbasic expects to receive plots six graphs,
#'
#' @param indat the data.frame containing the fisheries data
#' @param title the main title for the plots
#' @param sau the spatial management unit used, in Tasmania = block
#' @param leg1 legend location for the catch vs hours by sau plot,
#'     default="topleft"
#' @param leg2 legend location for the geometric mean cpue by sau plot,
#'     default="bottomleft"
#' @param leg3 legend location for the active diver by sau plot,
#'     default="bottomleft"
#'
#' @return nothing but it generates a 3 row x 2 col plot
#' @export
#'
#' @examples
#' \dontrun{
#'   indat=abd; title="Fortescue";sau="block";leg1="topleft";
#'   leg2="bottomleft";leg3="bottomleft"
#'   print("Waiting on an internal data.frame")
#' }    # indat=abd; title="blk13E"; sau="block";
plotbasic <- function(indat,title,sau="block",leg1="topleft",leg2="bottomleft",
                      leg3="bottomleft") {
  parset(plots=c(3,2),outmargin=c(0,0,1,0),margin=c(0.4,0.4,0.05,0.05))
  cbyb <- cbb(indat,sau=sau)  # catch by block by year
  hbyb <- hbb(indat,sau=sau)  # hour by block by year
  cvsh(cbyb,hbyb,legloc=leg1) # catch vs hours
  gbyb <- geobb(indat,legloc=leg2) # geometric mean by year
  dbb <- divbb(indat,sau,legloc=leg3) #active divers by block
  divact <- as.matrix(table(indat$diver,indat$year))
  ybd <- apply(divact,1,countgtzero)
  yrs <- as.numeric(colnames(divact))
  nyrs <- length(yrs)
  duration <- 1:nyrs
  dyr <- as.matrix(table(ybd))
  obsdur <- as.matrix(as.numeric(rownames(dyr)),dyr[,1])
  inthist(cbind(obsdur,dyr),width=0.8,col=2,border=1,
          panel.first=grid(),ylabel="Number of Divers",
          xlabel="Years of Experience")
  mtext(title,side=3,outer=TRUE,cex=1.0,font=7)
} # end of plotbasic


#' @title plotdata generates graphs of CE and log-transformed CE data.
#'
#' @description plotdata generates graphs of CE and log-transformed CE
#'   data. Included in the graph of the log-transformed cpue data is a
#'   fitted normal distribution with the associated bias-corrected average.
#'
#' @param indat is the data.frame containing the data being standardized.
#' @param dependent variable name; defaults to LnCE.
#'
#' @return a plot of the untransformed cpue data and of the log-transformed
#'   data. Included on the log-transformed data is a fitted normal
#'   distribution and the bias-corrected mean estiamte of average CPUE.
#' @export
#' @examples
#' \dontrun{
#' data(sps)
#' pick <- which(sps$Year == 2005)
#' sps2 <- droplevels(sps[pick,])
#' plotprep()
#' plotdata(sps2)
#' plotdata(sps2[sps2$Depth > 200,])
#' }
plotdata <- function(indat,dependent="LnCE") {
   colnames(indat) <- tolower(colnames(indat))
   dependent <- tolower(dependent)
   av1 <- mean(indat[,dependent],na.rm=TRUE)
   sd1 <- sd(indat[,dependent],na.rm=TRUE)
   n <- length(indat[,dependent])
   par(mfrow= c(2,1))
   par(mai=c(0.3,0.3,0,0.1), oma=c(0,1.0,0.0,0.0))
   par(cex=0.9, mgp=c(1.5,0.3,0), font.axis=7)
   outce <- hist(indat$ce,breaks=25,main="",ylab="",xlab="",col=2)
   outlnce <- hist(indat$lnce,breaks=25,main="",ylab="",xlab="",col=2)
   low <- outlnce$breaks[1]
   upp <- tail(outlnce$breaks,1)
   inc <- outlnce$breaks[2] - low
   ymax <- max(outlnce$counts)
   span <- seq(low,upp,0.05)
   lines(span,(n*inc)*dnorm(span,av1,sd1),lwd=3,col=1)
   abline(v=av1,col=3,lwd=2)
   mtext("Frequency",side=2,outer=TRUE,line=0.0,font=7,cex=1.0)
   label <- paste0("AvCE  ",round(exp(av1 + (sd1*sd1)/2),3))
   text(low,0.7*ymax,label,cex=1.0,font=7,pos=4)
}  # end of plotdata

#' @title plotlag plots the effect of a lag between two variables
#'
#' @description the use of the function ccf can suggest a lagged relationship
#'     between a driver variable and a react(ing) variable. For example, cpue
#'     may respond to catches in a negative manner after a lag of a few years.
#'     One looks for a negative lag, which would imply that the driver variable
#'     influences the react(ing) variable after the given lag has passed. The
#'     lag is always assumed to be based on yearly intervals, though this can
#'     be changed.
#'
#' @param x the matrix containing columns of the named variables. It must
#'     contain columns with the same names as the driver and react(ing)
#'     variables
#' @param driver the variable doing the influencing
#' @param react the variable being influenced
#' @param lag the time lag before the influence is felt
#' @param interval the name of the time-interval variable, default='year'
#' @param filename default is empty. If a filename is put here a .png file
#'     with that name will be put into the working directory.
#' @param resol the resolution of the png file, defaults to 200 dpi
#' @param fnt the font used in the plot and axes. Default=7, bold Times. Using
#'     6 gives Times, 1 will give SansSerif, 2 = bold Sans
#' @param label a name for the plot, perhaps the sau or black name to identify
#'     the data source.
#'
#' @return a list containing some summary results, the anova of the linear
#'     model fitted in aov, and a summary of the linear model in summ
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
#' dat <- cbind(year,catch,cpue)
#' out <- plotlag(dat,driver="catch",react="cpue",lag=7)
#' round(out$results,5)
#' out$summ
#' }
plotlag <- function(x, driver="catch",react="cpue",lag=0,interval="year",
                    filename="",resol=200,fnt=7,label=""){
  # filename="";fnt=7;x=x;driver="catch";react="cpue";lag=0;interval="year";lag=4
  lenfile <- nchar(filename)
  if (lenfile > 3) {
    end <- substr(filename,(lenfile-3),lenfile)
    if (end != ".png") filename <- paste0(filename,".png")
    png(filename=filename,width=5,height=5.5,units="in",res=resol)
  }
  parset(plots=c(2,2),margin=c(0.4, 0.45, 0.1, 0.05),outmargin=c(1.0,0,0,0),
         byrow=FALSE,cex=0.85)
  colnames(x) <- tolower(colnames(x))
  nobs <- dim(x)[1]
  # plot 1
  ymax <- max(x[,driver],na.rm=TRUE) * 1.025
  plot(x[1:(nobs-lag),interval],x[1:(nobs-lag),driver],type="l",lwd=2,xlab="",
       ylab=paste0(driver,"  ",label),ylim=c(0,ymax),yaxs="i",
       panel.first = grid(col="grey"))
  abline(h=0,col=1)
  text(x[1,interval],0.2*ymax,paste0("lag = ",lag),cex=1.2,pos=4)
  #plot 2
  ymax <- max(x[,react],na.rm=TRUE) * 1.025
  plot(x[(1+lag):nobs,interval],x[(1+lag):nobs,react],type="l",lwd=2,xlab="",
       ylab=paste0(react,"  ",label),ylim=c(0,ymax),yaxs="i",
       panel.first=grid())
  abline(h=0,col=1)
  #plot 3
  plot(x[1:(nobs-lag),driver],x[(1+lag):nobs,react],type="p",pch=16,
       ylim=c(0,max(x[,react],na.rm=TRUE)),panel.first = grid(),
       ylab=paste0(react,"  ",label),xlab=driver)

  model <- lm(x[(1+lag):nobs,react] ~ x[1:(nobs-lag),driver])
  abline(model,col=2)
  #plot4
  ccf(x[,driver],x[,react],lag.max=10)
  if (lenfile > 0) {
    outfile <- paste0(getwd(),"/",filename)
    print(outfile)
    dev.off()
  }
  ano <- anova(model)
  summ <- summary(model)
  results <- c(lag=lag,p=ano$`Pr(>F)`[1],
               adj_r2=summ$adj.r.squared,df=ano$Df[2])
  return(list(results=results,aov=anova(model),summ=summary(model)))
} # end of plotlag


#' @title plotmat plots a matrix of data
#'
#' @description plotmat is a utility function that is used to plot up matrices 
#'     calculated by cbb, hbb, geobb, and others. It expects a matrix of values 
#'     and the variable against which it is to be plotted as the rownames of the 
#'     matrix. Not exported but is used by rforcpue. A legend is only produced 
#'     if there is a matrix of values and is made up of the column names of the 
#'     input catchb matrix.
#'
#' @param inm a matrix of values whose rownames constitute the x-axis
#' @param xlab the xlabel, defaults to ""
#' @param ylab the ylabel, defaults to ""
#' @param legloc  the location of the legend defaults to "topright",
#'     could be "topleft", "bottomleft", or "bottomright"
#'
#' @return nothing, but it does plot a graph
#'
#' @examples
#' \dontrun{
#'  ind=abd2;legloc="topright";block="block";catch="catch";year="year"
#'
#' }
plotmat <- function(inm,xlab="years",ylab="",legloc="topright") {
  maxy <- getmax(inm)
  nblk <- ncol(inm)
  yrs <- as.numeric(rownames(inm))
  plot(yrs,inm[,1],type="l",lwd=2,col=1,xlab=xlab,ylab=ylab,
       ylim=c(0,maxy),panel.first=grid())
  if (nblk > 1) {
    label <- colnames(inm)
    for (i in 2:nblk) lines(yrs,inm[,i],lwd=2,col=i)
    legend(legloc,label,lwd=3,bty="n",col=1:nblk)
  }
} # end of plotmat

#' @title plotstand plot optimum model from standLM  vs Geometric mean
#'
#' @description plot optimum model from standLM  vs Geometric mean.
#'   Has options that allow for log-normls P% intervals around each time
#'   period's parameter estimate. Also can rescale the graph to have an average
#'   the same as the geometric mean average of the original time series of data.
#' @param stnd is the list output from standLM
#' @param bars is a logical T or F determining whether to put confidence bounds
#'   around each estimate; defaults to FALSE
#' @param geo is an estimate of the original geometric mean catch rate across
#'   all years. If this is > 0.0 it is used to rescale the graph to the
#'   nominal scale, otherwise the mean of each time-series will be 1.0, which
#'   simplifies visual comparisons. geo defaults to 0.0.
#' @param P is the percentile used for the log-normal confidence bounds, if
#'   they are plotted; defaults to 95.
#' @param catch if it is desired to plot the catch as well as the CPUE
#'   then a vector of catches needs to be input here
#' @param usefont enables the font used in the plot to be modified. Most
#'   publications appear to prefer usefont=1; defaults to 7 - Times bold
#' @param maxy default=NA, if numeric then it will be used rather than letting
#'     the software estimate a maximum  
#' @param miny default=0, this is used to define the bottom of the CPUE plot
#'   
#' @return a plot of the model with the smallest AIC (solid line) and the
#'   geometric mean (model 1, always = LnCE ~ Year, the dashed line). 'Year'
#'   could be some other time step.
#' @export plotstand
#' @examples
#' \dontrun{
#' data(abeg)
#' splabel = "SpeciesName"
#' labelM <- c("year","diver","month")
#' ab1 <- makecategorical(labelM[1:3],abeg)
#' mods <- makemodels(labelM)
#' out <- standLM(mods,ab1,splabel)
#' plotprep()
#' plotstand(out, bars=TRUE, P=90,geo=100.0,usefont=1)
#' plotstand(out)
#' }
plotstand <- function(stnd,bars=FALSE,geo=0.0,P=95,catch=NA,usefont=7,maxy=NA,
                      miny=0) {
  result <- stnd$Results
  if (geo > 0.0) result <- stnd$Results*geo
  sterr <- stnd$StErr
  whichM <- stnd$WhichM
  optimum <- stnd$Optimum
  years <- rownames(result)
  fishyr <- FALSE
  if (nchar(years[1]) > 4) {
    yrs <- 1:length(years)
    fishyr <- TRUE
  } else {
    yrs <- as.numeric(rownames(result))
  }
  laby <- paste(stnd$Label," CPUE",sep="")
  if (bars) {
    Zmult <- -qnorm((1-(P/100))/2.0)
    lower <- result[,optimum] * exp(-Zmult*sterr[,optimum])
    upper <- result[,optimum] * exp(Zmult*sterr[,optimum])
    ifelse(is.na(maxy),
        ymax <- max(result[,1],result[,optimum],upper,na.rm=TRUE)*1.025,
        ymax <- maxy)
  } else {
    ifelse(is.na(maxy),
           ymax <- max(result[,1],result[,optimum],na.rm=TRUE)*1.025,
           ymax <- maxy)
  }
  if (length(catch) > 1) par(mfrow= c(2,1)) else par(mfrow= c(1,1))
  par(mai=c(0.4,0.5,0,0), oma=c(0,0,0.25,0.25))
  par(cex=0.85, mgp=c(1.5,0.3,0), font.axis=usefont)
  plot(yrs,result[,1],type="l",lty=2,lwd=2,ylim=c(miny,ymax),yaxs="i",ylab="",
       xlab="",xaxs="r",panel.first=grid())
  lines(yrs,result[,optimum],lwd=3)
  if (bars) {
    arrows(x0=yrs[-1],y0=lower[-1],x1=yrs[-1],y1=upper[-1],
           length=0.035,angle=90,col=2,lwd=2,code=3)
  }
  title(ylab=list(laby, cex=1.0, col=1, font=usefont))
  if (geo > 0.0) {
    abline(h=geo,col="grey")
  } else {
    abline(h=1.0,col="grey")
  }
  if (length(catch) > 1) {
    if (length(catch) != length(yrs)) {
      stop("input catch data has incorrect number of years")
    }
    ymax <- max(catch,na.rm=TRUE) * 1.05
    plot(yrs,catch,type="b",pch=16,cex=0.8,lwd=2,ylim=c(0,ymax),yaxs="i",ylab="",
         xlab="",xaxs="r")
    grid()
    title(ylab=list("Catch", cex=1.0, col=1, font=usefont))
  }
} # End of plotstand

#' @title plotyrexp generates a plot of count by yrs of experience diving
#'
#' @description plotyrexp generates a plot of count of divers versus
#'     years of experience from teh input data.frame. Exeperience is
#'     defined as the number of years in which the diver reports
#'     catches in the docket catch-effort data.frame.
#'
#' @param indat the data.frame of docket abalone catch-effort fishery data
#' @param title a title for the plot, defaults top ""
#'
#' @return invisibly returns the counts of divers by years of experience
#' @export
#'
#' @examples
#' \dontrun{
#'   print("waiting on an inbuilt data-set")
#' }
plotyrexp <- function(indat,title="") {  #  indat=abd
  divact <- as.matrix(table(indat$diver,indat$year))
  ybd <- apply(divact,1,countgtzero)
  yrs <- as.numeric(colnames(divact))
  dyr <- as.matrix(table(ybd))
  obsdur <- as.matrix(cbind(as.numeric(rownames(dyr)),dyr[,1]))
  if (nchar(title) > 0) {
    parset(outmargin=c(0,0,1,0))
  } else {
    parset()
  }
  inthist(obsdur,width=0.9,col=2,border=3,ylabel="Number of Divers",
          xlabel="Years of Experience",panel.first=grid())
  return(invisible(obsdur))
} # end of plotyrexp



#' @title qqplotout plots up a single qqplot for a lm model
#'
#' @description qqplotout generates a single qqplot in isolation from the
#'     plot of a model's diagnostics. It is used with lefthist to
#'     illustrate how well a model matches a normal distribution
#'
#' @param inmodel the optimum model from standLM or dosingle
#' @param title a title for the plot, defaults to 'Normal Q-Q Plot'
#' @param cex the size of the font used, defaults to 0.9
#' @param ylow the lower limit of the residuals
#' @param yhigh he upper limit of the residuals
#' @param plotrug a logical value determinning whether a rug is included
#'
#' @return currently nothing, but it does generate a qqplot to the current
#'     device
#' @export
#'
#' @examples
#' \dontrun{
#'  y <- rep(1:100,2)
#'  x <- rnorm(200,mean=10,sd=1)
#'  model <- lm(y ~ x)
#'  dev.new(width=6,height=3.5,noRStudioGD = TRUE)
#'  par(mai=c(0.45,0.45,0.15,0.05),font.axis=7)
#'  qqplotout(model,ylow=-50,yhigh=50)
#' }
qqplotout <- function(inmodel, title="Normal Q-Q Plot", cex=0.9,
                      ylow=-5,yhigh=5,plotrug=FALSE)  {
   resids <- inmodel$residuals
   labs <- cex
   qqnorm(resids, ylab=list("Standardized Residuals", cex=labs, font=7),
          xlab=list("Theoretical Quantiles", cex=labs, font=7),
          main=list(title,cex=labs,font=7),ylim=c(ylow,yhigh),pch=1)
   qqline(resids, col=2,lwd=2)
   grid()
   if (plotrug) rug(resids)
   abline(v=c(-2.0,2.0),col="grey")
}  # end of qqplotout

#' @title qqdiag generates a qqplot with a histogram of residuals
#'
#' @description qqdiag generates a qqplot with a complementary histogram of
#'     the residuals to illustrate the proportion of all residuals along the
#'     qqline. If the qqline deviates from the expected straigt line, which
#'     is red i colour to make for simpler comparisons, then the histogram
#'     enables one to estiamte what proportion of records deviate from
#'     normality. The zero point is identified with a line, as are the
#'     approximate 5% and 95% percentiles. In both cases > 5% is above or
#'     below the blue lines, with < 90% in between depending on the
#'     proportions in each class. To get a more precise estimate use the
#'     invisibly returned histogram values.
#'
#' @param inmodel the optimum model being considered
#' @param plotrug a logical term determining whether a rug is plotted on the
#'     qqplot.
#' @param bins defaults to NA, but can be set to a given series
#' @param hline Include some horizontal lines on the histogram. defaults to 0.
#' @param xinc the increment for tick marks on the xaxis of the histogram
#' @param yinc the increment for tick marks on the y-axis of the histogram
#' @param ylab the y-axis label for the histogram, defaults to 'residuals'
#'
#' @return plots a graph and invisibly returns the output from the histogram
#' @export
#'
#' @examples
#' \dontrun{
#'  y <- rep(1:100,2)
#'  x <- rnorm(200,mean=10,sd=1)
#'  model <- lm(y ~ x)
#'  dev.new(width=6,height=3.5,noRStudioGD = TRUE)
#'  par(mai=c(0.45,0.45,0.15,0.05),font.axis=7)
#'  qqdiag(model,xinc=1,yinc=10,bins=seq(-55,50,2.5))
#' }
qqdiag <- function(inmodel,plotrug=FALSE,bins=NA,hline=0.0,
                   xinc=100,yinc=1.1,ylab="residuals") {
   layout(matrix(c(1,2),ncol=2),widths=c(5,2.5))
   par(mai=c(0.45,0.45,0.15,0.05),oma=c(0.0,0,0.0,0.0))
   par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
   resids <- inmodel$residuals
   qs <- quantile(resids,probs=c(0.01,0.05,0.95,0.99))
   if (!is.numeric(bins)) {
      loy <- min(resids); hiy <- max(resids)
      scale <- trunc(100*(hiy - loy)/35) / 100
      loy <- round(loy - (scale/2),2); hiy <- round(hiy + scale,2)
      tmp <- seq(loy,hiy,scale)
      bins <- seq(loy,hiy,length=length(tmp))
   } else {
      loy <- min(bins); hiy <- max(bins)
   }
   qqplotout(inmodel,plotrug=plotrug,ylow=loy,yhigh=hiy)
   abline(h=qs,lwd=c(1,2,2,1),col=4)
   # now draw histogram
   outL <- lefthist(resids,bins=bins,hline=0.0,yinc=yinc,xinc=xinc,
                    ylabel=ylab,width=0.9,border=1)
   abline(h=qs,lwd=c(1,2,2,1),col=4)
   ans <- addnorm(outL,resids)
   lines(ans$y,ans$x,lwd=2,col=3)
   return(invisible(outL))
}  # end of qqdiag

