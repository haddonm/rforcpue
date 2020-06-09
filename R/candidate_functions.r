
#' @title addyrexp adds the years of experience by record to data.frame
#'
#' @description addyrexp adds the years of experience of each diver, as
#'     recorded in the docket catch-effort data.frame, to
#'     the right-hand side of the input data.frame. The years of
#'     experience for each diver will obviously change depending on
#'     which block, or collection of blocks, are selected, though it
#'     would be possible to apply this function to the whole data.frame
#'     of information. The value of this is that it facilates the
#'     exploration of the influence of years of experience on everything
#'     else.
#'
#' @param indat the docket catch-effort data.frame of interest
#'
#' @return the same input data.frame with the addition of one column
#' @export
#'
#' @examples
#' \dontrun{
#'   print("waiting on an integral data-set")
#' }
addyrexp <- function(indat) {
  indat$yrexp <- NA
  divact <- as.matrix(table(indat$diver,indat$year))
  ybd <- apply(divact,1,countgtzero)
  dyr <- as.matrix(table(ybd))
  expyr <- as.numeric(rownames(dyr))
  nstep <- length(expyr)
  for (i in 1:nstep) {
    pick <- which(ybd == expyr[i])
    divs <- as.numeric(names(pick))
    pickdiv <- which(indat$diver %in% divs)
    indat$yrexp[pickdiv] <- expyr[i]
  }
  return(indat)
} #end of addyrexp

#' @title cbb catch by block or smu plot
#'
#' @description cbb plots out the catch by block or by smu, A legend is
#'     produced if there is a matrix of values and is made up of
#'     the column names of the input catchb matrix.
#'
#' @param ind  the input data.frame or matrix
#' @param smu the column name of the smu variable default = "block"
#' @param legloc the location of the legend defaults to "topright",
#'     could be "topleft", "bottomleft", or "bottomright"
#' @param catch the column name of the catc variable default = "catch"
#' @param year the column name of the year variable default = "year"
#'
#' @return invisibly the catch by year by block as a matrix
#' @export
#'
#' @examples
#' \dontrun{
#'  ind=abd2;legloc="topright";smu="block";catch="catch";year="year"
#' }
cbb <- function(ind,smu="block",legloc="topright",catch="catch",year="year") {
  cbyb <- as.matrix(tapply(ind[,catch],list(ind[,year],ind[,smu]),sum,na.rm=TRUE)/1000)
  plotmat(cbyb,xlab="years",ylab="Catch (t)",legloc=legloc)
  return(invisible(cbyb))
} #end of cbb - catch by block

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
#'    ind=abd2;legloc="topright";smu="block";hours="hours";year="year"
#'    cbyb <- cbb(ind,smu)
#'    hbyb <- hbb(ind,smu)
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
#' @param smu the column name of the block variable default = "block"
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
divbb <- function(ind,smu="block",legloc="topright",diver="diver",year="year") {
  dbyb <- table(ind[,diver],ind[,year],ind[,smu])
  nblk <- dim(dbyb)[3]
  dbb <- matrix(0,nrow=(dim(dbyb)[2]),ncol=(dim(dbyb)[3]),
                dimnames=list(dimnames(dbyb)[[2]],dimnames(dbyb)[[3]]))
  for (i in 1:nblk) dbb[,i] <- apply(dbyb[,,i],2,countgtzero)
  plotmat(dbb,xlab="years",ylab="Active Divers",legloc=legloc)
  return(invisible(dbb))
}


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
#' @param smu the column name of the block variable default = "block"
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
geobb <- function(ind,smu="block",legloc="topright",LnCE="LnCE",year="year") {
  gbb <- as.matrix(exp(tapply(ind[,LnCE],list(ind[,year],ind[,smu]),mean,na.rm=TRUE)))
  plotmat(gbb,xlab="years",ylab="Geometric Mean CE",legloc=legloc)
  return(invisible(gbb))
}

#' @title hbb plots the hours by year by block
#'
#' @description  hbb plots the hours by year by block
#'
#' @param ind  the input data.frame or matrix
#' @param smu the column name of the block variable default = "block"
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
hbb <- function(ind,smu="block",legloc="topright",hours="hours",year="year") {
  hbyb <- as.matrix(tapply(ind[,hours],list(ind[,year],ind[,smu]),sum,na.rm=TRUE)/1000)
  plotmat(hbyb,xlab="years",ylab="'000s Hours",legloc=legloc)
  return(invisible(hbyb))
} #end of hbb - hours by block


#' @title oneyrdivers removes divers who only fished for one year
#'
#' @description oneyrdivers takes in a data.frame of abalone fishery
#'     data and removes records relating to divers who only fished for
#'     a total of one year. Some only have a few records but there are
#'     instances of some divers having hundreds of records. In most
#'     standardizations single year divers have no influence, although
#'     one would expect an influence if one treated divers as a random
#'     effect.
#'
#' @param indat the data.frame of abalone fishery data
#'
#' @return a data.frame with single year divers removed
#' @export
#'
#' @examples
#' \dontrun{
#'  print("still being developed")
#' }
oneyrdivers <- function(indat) {
  divact <- as.matrix(table(indat$diver,indat$year))
  ybd <- apply(divact,1,countgtzero)
  remove <- which(ybd == 1)
  dropdiv <- as.numeric(names(remove))
  pickdrop <- which(indat$diver %in% dropdiv)
  outdat <- droplevels(indat[-pickdrop,])
  return(outdat)
} # end of oneyrdivers


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
