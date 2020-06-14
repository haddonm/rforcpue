
#' @title addcntcat adds the years of experience by record to data.frame
#'
#' @description addcntcat adds the count of years of experience, and the total
#'     catch across years, for each diver, as recorded in the input catch-effort 
#'     data.frame. Additions are made to the right-hand side of the input 
#'     data.frame. The count of years and total catch for each diver will 
#'     obviously change depending on which block, or collection of blocks, are 
#'     selected. The value of this is that it facilitates the exploration of the 
#'     influence of years of experience and total catch on everything else.
#'
#' @param indat the docket catch-effort data.frame of interest
#' @param diver character string name of the active diver/vessel or whoever/
#'     whatever is fishing each year, default='diver'
#' @param year character string name of the year, default='year'
#' @param catch character string name of the catch variable in the data.frame
#'
#' @return the same input data.frame with the addition of two columns
#' @export
#'
#' @examples
#' \dontrun{
#'   print("waiting on an integral data-set")
#' }
addcntcat <- function(indat,diver="diver",year="year",catch="catch",
                      count="count",totC="totC") {
  #indat=ab1;diver="diver";year="year";catch="catch";count="count";totC="totC"
  indat[,count] <- NA
  indat[,totC] <- NA
  cbd <- tapply(indat[,catch],list(indat[,diver],indat[,year]),sum,na.rm=TRUE)/1000
  cnt <- apply(cbd,1,countgtzero)
  ctot <- rowSums(cbd,na.rm=TRUE) 
  ndiver <- length(cnt)
  divers <- as.numeric(names(cnt))
  for (i in 1:ndiver) {
    pick <- which(indat[,diver] == divers[i])
    indat$count[pick] <- cnt[i]
    indat$totC[pick] <- ctot[pick]
  }
  return(indat)
} #end of addcntcat

#' @title cbb catch by block or sau plot
#'
#' @description cbb plots out the catch by block or by sau, A legend is
#'     produced if there is a matrix of values and is made up of
#'     the column names of the input catchb matrix.
#'
#' @param ind  the input data.frame or matrix
#' @param sau the column name of the sau variable default = "block"
#' @param legloc the location of the legend defaults to "topright",
#'     could be "topleft", "bottomleft", or "bottomright"
#' @param catch the column name of the catch variable default = "catch"
#' @param year the column name of the year variable default = "year"
#'
#' @return invisibly the catch by year by block as a matrix
#' @export
#'
#' @examples
#' \dontrun{
#'  ind=abd2;legloc="topright";sau="block";catch="catch";year="year"
#' }
cbb <- function(ind,sau="block",legloc="topright",catch="catch",year="year") {
  cbyb <- as.matrix(tapply(ind[,catch],list(ind[,year],ind[,sau]),sum,na.rm=TRUE)/1000)
  plotmat(cbyb,xlab="years",ylab="Catch (t)",legloc=legloc)
  return(invisible(cbyb))
} #end of cbb - catch by block

#' @title cleanname tidies a vector of names by removing commas and spaces
#' 
#' @description cleanname tides character strings in an input vector by 
#'     removing commas, apostrophes, and spaces. 
#'
#' @param invect the vector of character strings to be tidied.
#' @param removeNA a boolean determining whether NA values are replaced with 
#'     'unknown'
#'
#' @return a vector the same length as was input
#' @export
#'
#' @examples
#' x <- c("John Smith", "Port of Hobart","An example, and another")
#' cleanname(x)
cleanname <- function(invect,removeNA=TRUE) { # invect=ab$ports
  outvect <- invect
  types <- unique(invect)
  if (any(invect == "NA")) {
    pick <- which(invect == "NA")
    outvect[pick] <- "unknown"
    pick <- which(types == "NA")
    types <- types[-pick]
  }  
  outtype <- gsub(",","",types)
  outtype <- gsub("'","_",outtype)
  outtype <- gsub(" ","_",outtype)
  ntype <- length(types)
  for (i in 1:ntype) {
    pick <- which(outvect == types[i])
    outvect[pick] <- outtype[i]
  }
  return(outvect)
} # end of cleanname

#' @title cvsh plots the catch versus hours
#'
#' @description cvsh plots the catch versus hours, one can obtain the
#'     catches and hours from the functions cbb and hbb. A legend is
#'     produced if there is a matrix of values and is made up of
#'     the column names of the input catchb matrix.
#'
#' @param catchb a vector or matrix of catches by year by block
#' @param hoursb a matching vector or matrix of hours by year by block
#' @param legloc the location of the legend defaults to "topright",
#'     could be "topleft", "bottomleft", or "bottomright", usually
#'     determined interactively for each instance
#'
#' @return nothing, but it does plot a graph
#' @export
#'
#' @examples
#' \dontrun{
#'    ind=abd2;legloc="topright";sau="block";hours="hours";year="year"
#'    cbyb <- cbb(ind,sau)
#'    hbyb <- hbb(ind,sau)
#'    cvsh(cbyb,hbyb)
#' }
cvsh <- function(catchb,hoursb,legloc="topleft") {
  nblk <- ncol(catchb)
  maxy <- getmax(catchb)
  maxx <- getmax(hoursb)
  plot(hoursb[,1],catchb[,1],type="p",pch=16,col=1,cex=1.0,
       xlab="'000s Hours",ylab="Catch (t)",ylim=c(0,maxy),
       xlim=c(0,maxx),panel.first=grid())
  if (nblk > 1) {
    label <- colnames(catchb)
    for (i in 2:nblk) points(hoursb[,i],catchb[,i],pch=16,cex=1.0,col=i)
    legend(legloc,label,lwd=3,bty="n",col=1:nblk)
  }
} # end of cvsh catch vs hours

#' @title divbb plots a count of active diver by block by year
#'
#' @description divbb plots a count of divers active by block by year
#'
#' @param ind input data.frame or matrix
#' @param sau the column name of the block variable default = "block"
#' @param legloc location of the legend defaults to "topright",
#'     could be "topleft", "bottomleft", or "bottomright", usually
#'     determined interactively for each instance
#' @param diver the column name of the diver variable default = "diver"
#' @param year the column name of the year variable default = "year"
#'
#' @return invisibly returns the count of active divers by year and block
#' @export
#'
#' @examples
#' \dontrun{
#'   ind=abd2;legloc="topright";block="block";diver="diver";year="year"
#' }
divbb <- function(ind,sau="block",legloc="topright",diver="diver",year="year") {
  dbyb <- table(ind[,diver],ind[,year],ind[,sau])
  nblk <- dim(dbyb)[3]
  dbb <- matrix(0,nrow=(dim(dbyb)[2]),ncol=(dim(dbyb)[3]),
                dimnames=list(dimnames(dbyb)[[2]],dimnames(dbyb)[[3]]))
  for (i in 1:nblk) dbb[,i] <- apply(dbyb[,,i],2,countgtzero)
  plotmat(dbb,xlab="years",ylab="Active Divers",legloc=legloc)
  return(invisible(dbb))
} # end of divdd

#' @title factorprop summarizes the properties of a potential factor
#' 
#' @description factorprop summarizes the properties of a potential factor to be
#'     used in a statistical standardization of cpue data. It does this in terms 
#'     of the number of records and catches by year, and how many years the 
#'     factor involved reported catches. Thus if dealing with divers and vessels
#'     it provides catch by diver or vessel by year, plus number of records
#'     reported, and summarizes
#'
#' @param indat the data.frame being used for the standardization
#' @param fact the name of the factor being explored, a character string
#' @param resdir the results directory into which all results for the runname are
#'     put
#' @param runname the name of the particular analysis
#' @param resfile the result logfile, generated by setuphtml
#' @param year the name of the year factor in the data.frame. default='year'
#' @param catch the name of the catch factor in the data.frame. default='catch'
#' @param effort the name of the effort factor in the data.frame. default='hours'
#' @param cpue the name of the cpue factor in the data.frame. default='cpue'
#'
#' @return invisibly a list of records by year, catch by year and a table with
#'     total records, actively reporting years, and total catch for each value 
#'     of the factor
#' @export
#'
#' @examples
#' print("need to wait on an internal data.frame")
factorprop <- function(indat,fact,resdir,runname,resfile,
                       year="year",catch="catch",effort="hours",cpue="cpue") {
  #  indat=ab2; fact="ports";year="year";catch="catch";effort="hours";cpue="cpue"
  records <- table(indat[,fact])
  rby <- table(indat[,fact],indat[,year])
  fcby <- tapply(indat[,catch],list(indat[,fact],indat[,year]),sum,na.rm=TRUE)/1000
  county <- apply(fcby,1,countgtzero)
  totC <- rowSums(fcby,na.rm=TRUE)
  byfact <- cbind(records,years=county,totalC=totC)
  
  filename <- filenametopath(resdir,paste0(fact,"_activeyr_",runname,".png"))
  plotprep(width=7,height=4,filename=filename,verbose=FALSE)
  inthist(county,width=0.9,col=2,border=3,ylabel="Frequency",
          xlabel=paste0("Years ",fact," Reported Catches"),
          panel.first=grid())
  addplot(filen=filename,resfile=resfile,category=fact,
          caption=paste0("Years ",fact," Reported Catches"))    
  
  filename <- filenametopath(resdir,paste0(fact,"_rby_",runname,".csv"))
  addtable(rby,filen=filename,resfile=resfile,category=fact,
           caption=paste0("Records by year by ",fact,"."),big=TRUE)
  filename <- filenametopath(resdir,paste0(fact,"_cby_",runname,".csv"))
  addtable(fcby,filen=filename,resfile=resfile,category=fact,
           caption=paste0("Catch by year by ",fact,"."),big=TRUE)
  filename <- filenametopath(resdir,paste0(fact,"_",runname,".csv"))
  addtable(byfact,filen=filename,resfile=resfile,category=fact,
           caption=paste0("Total records active years and catch by ",fact,"."),
           big=TRUE)
  return(invisible(list(rby=rby,cby=fcby,byfact=byfact)))
} # end of factorprop


#' @title filelist literally lists the files in a directory
#'
#' @description filelist generate a data.frame of files in a directory
#'     including their properties - name, size, isdir, mdata and mtime,
#'     being the last modification data and time, adate and atime, being
#'     the last access dat and time. and finally a reference number to
#'     ease file selection. The option of searching for a text string
#'     within the complete filename is provided by the findtext argument.
#'
#' @param indir the directory whose contents are to be listed
#' @param findtext a text string to be searched for, default=""
#' @param ignorecase search on exact case or not, default=FALSE
#' @param silent should no summary be sent to the console, default=FALSE
#'
#' @return a dataframe of filenames is returned and, if silent=TRUE, two
#'     lines of information are sent to the console
#' @export
#'
#' @examples
#' \dontrun{
#'   filelist(indir=getwd())
#'   filelist(indir=getwd(),findtext="txt",ignorecase=TRUE)
#' }
filelist <- function(indir,findtext="",ignorecase=FALSE,silent=FALSE) {
  contents <- dir(indir)
  if (nchar(findtext) > 0) {
    pickf <- grep(pattern=findtext,contents,ignore.case=ignorecase)
    contents <- contents[pickf]
  }
  nfile <- length(contents)
  refNo <- seq(1,nfile,1)
  #resdirL <- nchar(indir)
  columns <- c("filename","size","isdir","mdate","mtime","adate","atime",
               "refNo")
  fileout <- as.data.frame(matrix(0,nrow=nfile,ncol=length(columns),
                                  dimnames=list(refNo,columns)))
  fileout[,"refNo"] <- refNo
  for (i in 1:nfile) {
    x <- file.info(paste(indir,contents[i],sep="/"))
    # remove the result directory from all filenames
    fileout[i,1] <- contents[i]
    fileout[i,2] <- x$size
    fileout[i,3] <- x$isdir
    fileout[i,4] <- substr(as.character(x$mtime),1,10)
    fileout[i,5] <- substr(as.character(x$mtime),12,19)
    fileout[i,6] <- substr(as.character(x$atime),1,10)
    fileout[i,7] <- substr(as.character(x$atime),12,19)
  }
  if (!silent) {
    cat("Number of Files : ",nfile,"\n")
    cat("Source Directory: ",indir,"\n")
  }
  return(fileout)
} # end of filelist


#' @title geobb plots the naive geometric mean by year and block
#'
#' @description geobb plots the naive geometric mean by year and block.
#'     It expects to find a LnCE variable, the natural log of coue, in
#'     the input data.frame or matrix.
#'
#' @param ind the input data.frame or matrix
#' @param sau the column name of the block variable default = "block"
#' @param legloc the location of the legend defaults to "topright",
#'     could be "topleft", "bottomleft", or "bottomright", usually
#'     determined interactively for each instance
#' @param LnCE the column name of the LnCE variable default = "LnCE"
#' @param year the column name of the year variable default = "year"
#'
#' @return invisibly returns the naive gemetric mean of cpue by year and block
#' @export
#'
#' @examples
#' \dontrun{
#'    print("wait")
#' }
geobb <- function(ind,sau="block",legloc="topright",LnCE="LnCE",year="year") {
  gbb <- as.matrix(exp(tapply(ind[,LnCE],list(ind[,year],ind[,sau]),mean,na.rm=TRUE)))
  plotmat(gbb,xlab="years",ylab="Geometric Mean CE",legloc=legloc)
  return(invisible(gbb))
}

#' @title hbb plots the hours by year by block
#'
#' @description  hbb plots the hours by year by block
#'
#' @param ind  the input data.frame or matrix
#' @param sau the column name of the block variable default = "block"
#' @param legloc  the location of the legend defaults to "topright",
#'     could be "topleft", "bottomleft", or "bottomright"
#' @param hours the column name of the effort variable default = "hours"
#' @param year the column name of the year variable default = "year"
#'
#' @return invisibly a matrix of effort(hours) by year by block
#' @export
#'
#' @examples
#' \dontrun{
#'  ind=abd2;legloc="topright";block="block";hours="hours";year="year"
#'
#' }
hbb <- function(ind,sau="block",legloc="topright",hours="hours",year="year") {
  hbyb <- as.matrix(tapply(ind[,hours],list(ind[,year],ind[,sau]),sum,na.rm=TRUE)/1000)
  plotmat(hbyb,xlab="years",ylab="'000s Hours",legloc=legloc)
  return(invisible(hbyb))
} #end of hbb - hours by block


#' @title tidynames can replace awkward data.frame names with better ones
#'
#' @description tidynames can replace awkward or overly long data.frame
#'     column names with better ones than are easier to use. It also
#'     permits one to maintain the same set of column names within an
#'     analysis even when the source data.frame includes alterations.
#'
#' @param columns the vector of names that shoudl include the ones to be
#'     altered
#' @param replace the names to be changed, as a vector of character
#'     strings
#' @param repwith the replacement names as a vector of character strings
#'
#' @return a vector of new columns names
#' @export
#'
#' @examples
#' \dontrun{
#'  print("wait")
#' }
tidynames <- function(columns,replace,repwith) {
  nreplace <- length(replace)
  if (nreplace != length(repwith))
    stop("Different number of names in replace and repwith \n")
  for (i in 1:nreplace) {
    pick <- grep(replace[i],columns)
    #cat(i,pick,"\n")
    if (pick[1] > 0) {
      columns[pick[1]] <- repwith[i]
    } else {
      warning(paste0(replace[i]," not in the dataset"))
    }
  }
  return(columns)
} # end of tidynames

#' @title yearprop summarizes the properties of the year as a factor
#' 
#' @description yearprop summarizes the properties of the year as a factor,
#'     which is useful before conducting a cpue standardization.
#'
#' @param indat the data.frame being analysed
#' @param resdir the results directory into which all results for the runname are
#'     put
#' @param runname the name of the particular analysis
#' @param resfile the result logfile, generated by setuphtml
#' @param year the name of the year factor in the data.frame. default='year'
#' @param catch the name of the catch factor in the data.frame. default='catch'
#' @param effort the name of the effort factor in the data.frame. default='hours'
#' @param cpue the name of the cpue factor in the data.frame. default='cpue'
#'
#' @return invisibly returns a matrix of year, records, catch, hours, and 
#'    bias-corrected geometric mean cpue. Also adds a table to the resdir and 
#'    resfile
#' @export
#'
#' @examples
#' print("need to wait on an internal data.frame")
yearprop <- function(indat,resdir,runname,resfile,
                     year="year",catch="catch",effort="hours",cpue="cpue") {
  rbf <- table(indat[,year])
  factnames <- as.numeric(names(rbf))
  cbf <- tapply(indat[,catch],indat[,year],sum,na.rm=TRUE)/1000
  ebf <- tapply(indat[,effort],indat[,year],sum,na.rm=TRUE)
  geom <- tapply(indat[,cpue],indat[,year],geomean)
  yearprops <- cbind(factnames,rbf,cbf,ebf,geom)
  colnames(yearprops) <- c("year","records","catch","hours","geom")
  filename <- filenametopath(resdir,paste0("yearprops_",runname,".png"))
  plotprep(width=7,height=5,filename=filename,verbose=FALSE)
  parset(plots=c(2,2),margin=c(0.3,0.45,0.05,0.1))
  ymax <- getmax(yearprops[,"records"])
  plot(yearprops[,"year"],yearprops[,"records"],type="l",lwd=2,xlab="",yaxs="i",
       ylab="Total Number of Records",panel.first=grid(),ylim=c(0,ymax))
  ymax <- getmax(yearprops[,"catch"])
  plot(yearprops[,"year"],yearprops[,"catch"],type="l",lwd=2,xlab="",yaxs="i",
       ylab="Catch (t)",panel.first=grid(),ylim=c(0,ymax))
  ymax <- getmax(yearprops[,"hours"])
  plot(yearprops[,"year"],yearprops[,"hours"],type="l",lwd=2,xlab="",yaxs="i",
       ylab="Effort as Total Hours",panel.first=grid(),ylim=c(0,ymax))
  ymax <- getmax(yearprops[,"geom"])
  plot(yearprops[,"year"],yearprops[,"geom"],type="l",lwd=2,xlab="",yaxs="i",
       ylab="Geometric Mean CPUE",panel.first=grid(),ylim=c(0,ymax))
  addplot(filen=filename,resfile=resfile,category="year",
          caption=paste0("Summary of properties by Year"))    
  filename <- filenametopath(resdir,paste0("yearprops_",runname,".csv"))
  addtable(yearprops,filen=filename,resfile=resfile,category="year",
           caption=paste0("Properties of the year factor."))
  return(invisible(yearprops))
} # end of yearprop
