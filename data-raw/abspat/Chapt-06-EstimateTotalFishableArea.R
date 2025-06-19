## --------------------------------------------
#| echo: false
#| warning: false
#| message: false
#| cache: false

source("_common.R")
library(patchwork)
library(mixedup)
library(makehtml)
library(rforcpue)
#library(abmaps)
library(abspatR)
library(codeutils)
library(hplot)  # https://github.com/haddonm
library(sf)
library(flextable)
library(dplyr)


datadir <-  sprintf("C:/Users/%s/Dropbox/AbaloneData/", Sys.info()[["user"]])



## ----"getGPSdata"----------------------------
bl <- readRDS(paste0(datadir,"bl.rds"))   # instead of lines 14 - 50 below.
# bl <- bl[order(bl$oid),]
# totE <- tapply(bl$minutes,bl$oid,sum,na.rm=TRUE)
# totC <- tapply(bl$catch,bl$oid,sum,na.rm=TRUE)/1000
# maxE <- tapply(bl$minutes,bl$oid,max,na.rm=TRUE)
# 
# 
# noid <- length(maxE)
# oids <- as.integer(names(maxE))
# n <- nrow(bl)
# bl$totE <- NA
# bl$totC <- NA
# bl$maxE <- NA
# for (i in 1:noid) {  # this takes a while
#   pick <- which(bl$oid == oids[i])
#   bl$totE[pick] <- totE[i]
#   bl$totC[pick] <- totC[i]
#   bl$maxE[pick] <- maxE[i]
# }
prop <- properties(bl)
# saveRDS(bl,file=paste0(rundir,"bl.rds"))


## ----"summarizedata"-------------------------
oidsum <- summoid(bl)
blk <- sort(unique(bl$block)) # now by blocks
nblk <- length(blk)


## --------------------------------------------
#| label: tbl-blfields
#| tbl-cap: "The data fields used in the tipy database for the years 2012 - 2019, showing their general properties. 'coastdist is the distance in m of the centroid to the coast, divers is the number of unique divers that have visted an oid across years, exposure is an index of wave exposure, count_yr = occur, totE, totC, and maxE reflect oid values across all years."
#| fig-pos: "H"
# 

#kable(prop, align="r",  booktabs = TRUE)

 prop |> tibble::rownames_to_column(var = "varname") |> flextable::flextable()



## ----"pickout example blocks"----------------
pickE <- which((bl$zone == "E") & (bl$block %in% c(13, 21)))
pickN <- which((bl$zone == "N") & (bl$block == 6))
pickW <- which((bl$zone == "W") & (bl$block == 11))
pickall <- c(pickE,pickN,pickW)
blb <- bl[pickall,]
blb <- blb[order(blb$oid),]
# catch by year by block
cbybb <- tapply(blb$catch,list(blb$year,blb$block),sum,na.rm=TRUE)/1000
#  records by year by block
rbybb <- tapply(blb$catch,list(blb$year,blb$block),countgtzero)
ebybb <- tapply(blb$minutes,list(blb$year,blb$block),sum,na.rm=TRUE)


## --------------------------------------------
#| label: fig-figOC1a
#| fig-cap: "The annual catch, hours of effort, and ratio cpue (total catch/total effort) in the GPS data logger data set for each of the four example blocks."
#| fig-pos: "H"
#| fig-width: 6
#| fig-height: 7.5

yrs <- as.numeric(rownames(cbybb))
blks <- colnames(cbybb)
nblk <- length(blks)
ymax <- getmax(cbybb)
parset(plots=c(3,1),margin = c(0.3,0.45,0.1,0.1)) 
plot1(yrs,cbybb[,blk[1]],lwd=2,defpar=FALSE,maxy=ymax, ylab="Annual Catch (t)")
for (i in 2:nblk) lines(yrs,cbybb[,blk[i]],lwd=2,col=i)
legend(x=2018,y=130,legend=blks,col=1:nblk,lwd=3,bty="n",cex=1.2)
hbybb <- ebybb/60  # hours of effort
ymax <- getmax(hbybb)
plot1(yrs,hbybb[,blk[1]],lwd=2,defpar=FALSE,maxy=ymax, ylab="Hours Effort per Year")
for (i in 2:nblk) lines(yrs,hbybb[,blk[i]],lwd=2,col=i)
cpue <- 1000*cbybb/hbybb 
ymax <- getmax(cpue)
plot1(yrs,cpue[,blk[1]],lwd=2,defpar=FALSE,maxy=ymax, ylab="Ratio CPUE kg/hr per Year")
for (i in 2:nblk) lines(yrs,cpue[,blk[i]],lwd=2,col=i)


## ----"count the oids by block"---------------
columns <- c("nobs","nhexagons","hours","catch t","catchKg/hex","RatioCE")
countoids <- matrix(0,nrow=nblk,ncol=length(columns),dimnames=list(blks,columns))
for (i in 1:nblk) {
  pick <- which(blb$block == as.numeric(blks[i]))
  tmp <- summoid(blb[pick,],effort=0.01)
  countoids[i,] <- c(tmp[1:2],tmp[3]/60,tmp[4]/1000,tmp[4]/tmp[2],
                     tmp[4]/(tmp[3]/60))
}


## --------------------------------------------
#| label: tbl-tabOC1a
#| tbl-cap: "Summary information for each block across all years. nobs is the number of observations or records, nhexagons is the number of unique hexagons reporting effort, hours is the total effort, catch is the total catch, catchKg/Hex is the average catch in Kg per hexagon per block, and the RatioCE is the ratio CPUE for the block averaged across years."
#| fig-pos: "H"

kable(countoids,digits=c(0,0,1,3,3,3),row.names=TRUE,  booktabs = TRUE)


## ----"Blk11 analysis", echo=TRUE-------------
pickB <- which(blb$block == 11)
ble <- blb[pickB,]
oidoccur <- table(ble$tipy,ble$occur)


## ----"getuniqueoids for 11", echo=TRUE-------
# Get unique oids for each occur vs tipy combination
oidoccur <- fillmat(oidoccur,replace=0)
unoid <- matrix(0,nrow=32,ncol=8,dimnames=list(1:32,1:8))
unoid[,1] <- oidoccur[,1]
unoid[,8] <- oidoccur[,8]/8
for (i in 2:7) {
  for (j in 1:32) {
    if (oidoccur[j,i] > 0) {
      pickO <- which((ble$occur == i) & (ble$tipy == j))
      unoid[j,i] <- length(unique(ble$oid[pickO]))
    }
  }
}


## ----"occur vs tipy for blk11"---------------
#| label: tbl-tabOC2
#| tbl-cap: "The frequency of occurrence of each combination of occur and tipy within the 24730 observations within block 11's dataset."
#| fig-pos: "H"


kable(oidoccur,row.names=TRUE,  booktabs = TRUE)




## ----"uniqueoids for 11"---------------------
#| label: tbl-tabOC3
#| tbl-cap: "The frequency of occurrence of unique hexagons in each combination of occur and tipy within the 5881 unique hexagons within block 11's dataset."
#| fig-pos: "H"

kable(unoid,row.names=TRUE,  booktabs = TRUE)


## ----"calc the proportions", echo=TRUE-------
propoid <- matrix(0,nrow=3,ncol=8,dimnames=list(c("N","Prop","CumProp"),1:8))
propoid[1,] <- colSums(unoid)
propoid[2,] <- propoid[1,]/sum(propoid[1,],na.rm=TRUE)
propoid[3,] <- cumsum(propoid[2,])


## ----"proportions of occur1"-----------------
#| label: tbl-tabOC4
#| tbl-cap: "The proportion of hexagons experiencing different numbers of years of fishing."
#| fig-pos: "H"

kable(digitsbyrow(propoid,digits=c(0,3,3)),align="r",  booktabs = TRUE)


## ----"effortminutesin block11"---------------
#| label: fig-figOC2
#| fig-cap: "The distribution of effort as minutes in each hexagon within block 11. The lower plot is identical to the upper plot except with an upper bound of 60 minutes and breaks at 3 minutes so as to expand the visibility of the smaller values."
#| fig-pos: "H"
#| fig-width: 6
#| fig-height: 4

parset(plots=c(2,1),margin = c(0.4,0.45,0.1,0.1)) 
hist(ble$minutes,main="",xlab="Minutes") 
uphist(ble$minutes,maxval=60,main="",xlab="Minutes (maximum at 100)",breaks=seq(0,60,length=20)) 


## ----"plottipyfequencies"--------------------
#| label: fig-figOC3
#| fig-cap: "The frequency distribution of the tipy index for each example statistical block, 6N, 11W, 13E, 21E."
#| fig-pos: "H"
#| fig-width: 7
#| fig-height: 4.5


# plot tipy by block across years 2012 - 2019 -------------------------------
tiptable <- blocktipprops(blb,effort=0.01)
maxt <- 32
tipycount <- matrix(0,nrow=maxt,ncol=nblk,dimnames=list(1:maxt,blks))
parset(plots=pickbound(nblk),margin=c(0.3,0.3,0.05,0.05),
       outmargin=c(1.25,1.25,0,0),byrow=FALSE)
for (i in 1:nblk) { # i = 5
  pickB <- which(blb$block == blks[i])
#  ans <- tapply(blb[pickB,"minutes"],blb[pickB,"tipy"],sum,na.rm=TRUE)/60
  ans <- rowSums(table(blb$tipy[pickB],blb$occur[pickB]))
  if (length(ans) < maxt) ans <- fillmat(ans,rown=maxt)
  vals <- 1:maxt
  counts <- ans
  res <- cbind(vals,counts)
  outh <- inthist(res,width=0.9,border=3,col=2,main=blks[i],roundoff = FALSE)
  tipycount[,i] <- outh[,"counts"]
  maxy <- max(outh[,"counts"],na.rm=TRUE) # tiptable from line 296
  text(0.75*maxt,1.1*maxy,paste0("records ",tiptable[i,1]),cex=1,pos=4)
  text(0.75*maxt,1.0*maxy,paste0("oids     ",tiptable[i,2]),cex=1,pos=4)
  text(0.75*maxt,0.9*maxy,paste0("hours ",trunc(tiptable[i,3])),cex=1,pos=4)
  text(0.75*maxt,0.8*maxy,paste0("catch ",round(tiptable[i,4],3)),cex=1,pos=4)
#  text(0.75*maxt,0.575*maxy,paste0("tip1    ",outh[1,"counts"]),cex=1,pos=4)
 # text(0.75*maxt,0.475*maxy,paste0("area*   ",gpscount[i,11]),
 #      cex=1,pos=4)
}  
mtext("tipy value",side=1,outer=TRUE,cex=1.1)  
mtext("Number of Records",side=2,outer=TRUE,cex=1.1)  



## ----"plottipycatches1"----------------------
#| label: fig-figOC4
#| fig-cap: "The distribution of catches for each tipy value for each example statistical block, 6N, 11W, 13E, 21E. The Eastern Zone blocks have more weight towards the higher tipy values."
#| fig-pos: "H"
#| fig-width: 7
#| fig-height: 4.5


# plot tipy by block across years 2012 - 2019 -------------------------------
#tiptable <- blocktipprops(blb,effort=0.01)
maxt <- 32
tipycatch <- matrix(0,nrow=maxt,ncol=nblk,dimnames=list(1:maxt,blks))
parset(plots=pickbound(nblk),margin=c(0.3,0.3,0.05,0.05),
       outmargin=c(1.25,1.25,0,0),byrow=FALSE)
for (i in 1:nblk) { # i = 5
  pickB <- which(blb$block == blks[i])
  ans <- tapply(blb[pickB,"catch"],blb[pickB,"tipy"],sum,na.rm=TRUE)/1000
  if (length(ans) < maxt) ans <- fillmat(ans,rown=maxt)
  vals <- 1:maxt
  counts <- ans
  res <- cbind(vals,counts)
  outh <- inthist(res,width=0.9,border=3,col=2,main=blks[i],roundoff=FALSE)
  tipycatch[,i] <- outh[,"counts"]
  maxy <- max(outh[,"counts"],na.rm=TRUE) # tiptable from line 296
  text(0.75*maxt,1.1*maxy,paste0("records ",tiptable[i,1]),cex=1,pos=4)
  text(0.75*maxt,1.0*maxy,paste0("oids     ",tiptable[i,2]),cex=1,pos=4)
  text(0.75*maxt,0.9*maxy,paste0("hours ",trunc(tiptable[i,3])),cex=1,pos=4)
  text(0.75*maxt,0.8*maxy,paste0("catch ",round(tiptable[i,4],3)),cex=1,pos=4)
}  
mtext("tipy value",side=1,outer=TRUE,cex=1.1)  
mtext("Catch (t) per tipy value",side=2,outer=TRUE,cex=1.1)  



## ----"plottipycatches2"----------------------
#| label: fig-figOC5
#| fig-cap: "The mean catch per tipy value with 99% Normal CI for each example statistical block, 6N, 11W, 13E, 21E. The number of observations for each tipy value are so large that the CI are all small except for the tipy values with low representation."
#| fig-pos: "H"
#| fig-width: 7
#| fig-height: 5

# plot tipy by block across years 2012 - 2019 -------------------------------
#tiptable <- blocktipprops(blb,effort=0.01)

calcCI <- function(av,std,count,normCI=TRUE,P=0.95) {
  if (!normCI) {
    Zmult <- -qnorm((1-(P/100))/2.0)
    lower <- av * exp(-Zmult*(std/count))
    upper <- av * exp(Zmult*(std/count))
  } else {
    pval <- 1-(1-P)/2.0
    Zmult <- qnorm(p=pval,mean=0,sd=std/count)
    lower <- av - Zmult
    upper <- av + Zmult
  }
  return(list(lower=lower,upper=upper))
} # end of calcCI   
   
maxt <- 32
meancatch <- matrix(0,nrow=maxt,ncol=nblk,dimnames=list(1:maxt,blks))
sdcatch <- matrix(0,nrow=maxt,ncol=nblk,dimnames=list(1:maxt,blks))
countcatch <- matrix(0,nrow=maxt,ncol=nblk,dimnames=list(1:maxt,blks)) 
parset(plots=pickbound(nblk),margin=c(0.3,0.3,0.05,0.05),
       outmargin=c(1.25,1.25,0,0),byrow=FALSE)
for (i in 1:nblk) { # i = 1
  pickB <- which(blb$block == blks[i])
  meancatch[,i] <- fillmat(tapply(blb[pickB,"catch"],blb[pickB,"tipy"],mean,na.rm=TRUE))
  sdcatch[,i] <- fillmat(tapply(blb[pickB,"catch"],blb[pickB,"tipy"],sd,na.rm=TRUE))
  countcatch[,i] <- fillmat(rowSums(table(blb$tipy[pickB],blb$occur[pickB])))
  CI <- calcCI(meancatch,sdcatch,countcatch,normCI=TRUE,P=0.99)
  lower <- CI$lower
  upper <- CI$upper
  maxy <- getmax(upper[,i])
  plot(1:maxt,meancatch[,i],type="p",pch=16,cex=0.75,ylim=c(0,maxy),xlab="",
       ylab="")
  for (j in 1:maxt) lines(c(j,j),c(lower[j,i],upper[j,i]),lwd=2)
  text(0.05*maxt,0.95*maxy,paste0("records ",tiptable[i,1]),cex=1,pos=4)
  text(0.05*maxt,0.85*maxy,paste0("oids     ",tiptable[i,2]),cex=1,pos=4)
  text(0.05*maxt,0.75*maxy,paste0("hours ",trunc(tiptable[i,3])),cex=1,pos=4)
  text(0.05*maxt,0.65*maxy,paste0("catch ",round(tiptable[i,4],3)),cex=1,pos=4)
  mtext(blks[i],side=3,line=-1.5,cex=1.25)
}  
mtext("tipy value",side=1,outer=TRUE,cex=1.1)  
mtext("Mean Catch (kg) per tipy value",side=2,outer=TRUE,cex=1.1)  



## ----"plottipycatches3"----------------------
#| label: fig-figOC6
#| fig-cap: "The mean catch per tipy value by year with 99% Normal CI for example statistical block 6N."
#| fig-pos: "H"
#| fig-width: 7
#| fig-height: 8

pickB <- which(blb$block == 6)
blp <- blb[pickB,]
yrs <- sort(unique(blp$year))
nyr <- length(yrs)
meancatch <- matrix(0,nrow=maxt,ncol=nyr,dimnames=list(1:maxt,yrs))
sdcatch <- matrix(0,nrow=maxt,ncol=nyr,dimnames=list(1:maxt,yrs))
countcatch <- matrix(0,nrow=maxt,ncol=nyr,dimnames=list(1:maxt,yrs)) 
parset(plots=pickbound(nyr),margin=c(0.3,0.3,0.05,0.05),
       outmargin=c(1.25,1.25,0,0),byrow=FALSE)
for (i in 1:nyr) { # i = 1
  pickY <- which(blp$year == yrs[i])
  meancatch[,i] <- fillmat(tapply(blp[pickY,"catch"],blp[pickY,"tipy"],mean,na.rm=TRUE))
  sdcatch[,i] <- fillmat(tapply(blp[pickY,"catch"],blp[pickY,"tipy"],sd,na.rm=TRUE))
  countcatch[,i] <- fillmat(rowSums(table(blp$tipy[pickY],blp$occur[pickY])))
  CI <- calcCI(meancatch,sdcatch,countcatch,normCI=TRUE,P=0.95)
  lower <- CI$lower
  upper <- CI$upper
  maxy <- getmax(upper[,i])
  plot(1:maxt,meancatch[,i],type="p",pch=16,cex=0.75,ylim=c(0,maxy),xlab="",
       ylab="")
  for (j in 1:maxt) lines(c(j,j),c(lower[j,i],upper[j,i]),lwd=2)
  text(0.05*maxt,0.95*maxy,paste0("records ",length(pickY)),cex=1,pos=4)
  text(0.05*maxt,0.85*maxy,paste0("oids     ",
                                  length(unique(blp$oid[pickY]))),cex=1,pos=4)
  text(0.05*maxt,0.75*maxy,paste0("hours ",
                    round(sum(blp$minutes[pickY],na.rm=TRUE)/60,3)),cex=1,pos=4)
  text(0.05*maxt,0.65*maxy,paste0("catch ",
                    round(sum(blp$catch[pickY],na.rm=TRUE)/1000,3)),cex=1,pos=4)
  mtext(yrs[i],side=3,line=-1.5,cex=1.25)
}  
mtext("tipy value",side=1,outer=TRUE,cex=1.1)  
mtext("Mean Catch (kg) per tipy value",side=2,outer=TRUE,cex=1.1)  



## ----"oids with range of effort", echo=TRUE----
pickL <- which(blb$minutes <= 3)
oids <- unique(blb[pickL,"oid"])
pickO <- which(blb$oid %in% oids)
lt3 <- blb[pickO,]


## ----"effortwhenminsLTE3used"----------------
#| label: fig-figOC7
#| fig-cap: "The distribution of effort as minutes in each unique hexagon within block 11 that has at least one year where effort was <= 3 minutes. The lower plot is identical to the upper plot except with an upper bound of 50 minutes so as to expand the visibility of the smaller values."
#| fig-pos: "H"
#| fig-width: 7
#| fig-height: 4


parset(plots=c(2,1),margin = c(0.4,0.45,0.1,0.1)) 
hist(lt3$minutes,main="",xlab="Minutes") 
uphist(lt3$minutes,maxval=60,main="",xlab="Minutes (maximum at 100)",
       breaks=seq(0,60,length=20)) 


## ----"search different maxE values", echo=TRUE----
effrange=seq(1,20,0.5)
N <- length(effrange)
columns <- c("maxE","noid","select","prop","catch","propcatch")
result <- matrix(0,nrow=N,ncol=length(columns),dimnames=list(effrange,columns))
ans <- vector(mode="list",length=nblk)
cols2 <- c("noid","catch")
blkchar <- matrix(0,nrow=nblk,ncol=length(cols2),dimnames=list(blks,cols2))
names(ans) <- blks
for (blck in 1:nblk) {
  pickB <- which(blb$block == blks[blck])
  ble <- blb[pickB,]
  unoids <- length(unique(ble$oid))
  catch <- sum(ble$catch,na.rm=TRUE)
  blkchar[blck,] <- c(unoids,catch)
  for (step in 1:N) {
    efflim <- effrange[step]
    countmaxE <- matrix(0,nrow=32,ncol=8,dimnames=list(1:32,1:8))
    sumcatch <- countmaxE
    for (i in 1:8) {
      for (j in 1:32) {
        pickC <- which((ble$occur == i) & (ble$tipy == j))
        combto <- ble[pickC,]
        pickE <- which(combto$maxE <= efflim)
        if (length(pickE) > 0) {
           lowoids <- unique(combto[pickE,"oid"]) 
           countmaxE[j,i] <- length(lowoids)
           sumcatch[j,i] <- sum(combto[pickE,"catch"])
        }
      }
    }
    result[step,] <- c(efflim,unoids,sum(countmaxE),sum(countmaxE)/unoids,
                       sum(sumcatch),sum(sumcatch)/catch)
  } # effort step
  ans[[blck]] <- result
#  cat(blks[blck],unoids,"\n")
}


## ----"plotmaxEsearch"------------------------
#| label: fig-figOC8
#| fig-cap: "The proportion of the unique hexagons reported in each example block selected by different values of the maximum effort across years value (maxE). There are no obvious inflection points in any block and the vertical red line at 3 minutes is the nominally selected value of maxE that will be used leading to percentages of hexagons excluded of 14.3, 13.7, 11.4 and 17.1 in blocks 6, 11, 13, and 21, respectively."
#| fig-pos: "H"
#| fig-width: 7
#| fig-height: 7


parset(plots=c(2,1),cex=1.0)
result <- ans[[1]]
plot(effrange,result[,"prop"],type="l",lwd=2,ylim=c(0,0.52),
      ylab="Proportion Hexagons <= X value",xlab="maxE Value",
     panel.first = grid(),yaxs="i")
for (i in 2:nblk) {
  result <- ans[[i]]  
  lines(effrange,result[,"prop"],lwd=2,col=i)
}
abline(v=3,lwd=1,col=2)
legend("bottomright",legend=blks,col=1:nblk,lwd=3,bty="n",cex=1.25)  

result <- ans[[1]]
plot(effrange,result[,"propcatch"],type="l",lwd=2,
     ylim=c(0,0.045),ylab="Proportion of Total Block Catch",xlab="maxE Value",
     panel.first = grid(),yaxs="i")
for (i in 2:nblk) {
  result <- ans[[i]]  
  lines(effrange,result[,"propcatch"],lwd=2,col=i)
}
abline(v=3,lwd=1,col=2)
legend("topleft",legend=blks,col=1:nblk,lwd=3,bty="n",cex=1.25)  


## ----"getselectedarea", echo=TRUE------------
finalarea <- countoids[,c(1,3,4,2,5,6)]
colnames(finalarea) <- c(colnames(countoids)[c(1,3,4,2)],"area*","fishable Ha")
for (i in 1:nblk) finalarea[i,"area*"] <- ans[[i]]["3","prop"] * 
                                                       countoids[i,"nhexagons"]
finalarea[,"fishable Ha"] <- finalarea[,"nhexagons"] - finalarea[,"area*"]


## ----"proportions of occur2"-----------------
#| label: tbl-tabOC5
#| tbl-cap: "Summary information for each block across year. nhexagons is the total number of unique hexagons identified in the observations for each block, area* is the number of hexagons selected with maximum effort levels of <= 3 minutes, and fishable Ha is the estimated fishable reef area in hectares after the false positive hexagons are removed."
#| fig-pos: "H"

kable(finalarea,digits=c(0,1,3,0,0,0),  booktabs = TRUE)


## ----"countmaxE for 3 minutes",echo=TRUE-----
blkans <- vector(mode="list",length=nblk)
names(blkans) <- blks
efflim <- 3.0
for (blck in 1:nblk) {
  pickB <- which(blb$block == blks[blck])
  ble <- blb[pickB,]
  countmaxE <- matrix(0,nrow=32,ncol=8,dimnames=list(1:32,1:8))
  sumcatch <- countmaxE
  alloids <- NULL
  for (i in 1:8) {
    for (j in 1:32) {
      pickC <- which((ble$occur == i) & (ble$tipy == j))
      if (length(pickC) > 0) {
        combto <- ble[pickC,]
        pickE <- which(combto$maxE <= efflim)
        if (length(pickE) > 0) {
          lowoids <- unique(combto[pickE,"oid"]) 
          alloids <- c(alloids,lowoids)
          countmaxE[j,i] <- length(lowoids)
          sumcatch[j,i] <- sum(combto[pickE,"catch"])
        }
      }
    }
  }
  blkans[[blck]] <- list(alloids=sort(alloids),countmaxE=countmaxE,
                         sumcatch=sumcatch)
}


## ----"proportions of occur3"-----------------
#| label: tbl-tabOC6
#| tbl-cap: "The count of hexagons in block 6 having a maxE value <= 3 in each combination of tipy and occur (see appendix for the other block results."
#| fig-pos: "H"

countmaxE <- blkans[[1]]$countmaxE
kable(countmaxE[1:8,],row.names=TRUE,  booktabs = TRUE)


## ----"summaryacross blocks"------------------
#| label: tbl-tabOC7
#| tbl-cap: "A summary across blocks of the result of selecting on maxE <= 3 minutes."
#| fig-pos: "H"

blkres <- rbind(ans[[1]][5,],ans[[2]][5,],ans[[3]][5,],ans[[4]][5,])
columns <- colnames(blkres)
blkres <- cbind(blkres,blkchar[,"catch"])
rownames(blkres) <- blks
colnames(blkres) <- c(columns,"totalcatch")
kable(blkres,digits=c(0,0,0,4,3,5,1),row.names=TRUE,  booktabs = TRUE)


## ----"hexagon_use_through_years"-------------
#| label: tbl-tabOC8
#| tbl-cap: "A summary of the hexagon use across years for block 6."
#| fig-pos: "H"

  pickB <- which(blb$block == 6)
  ble <- blb[pickB,]
  outall <- gethexdat(ble)
  kable(outall,digits=c(0,0,0,0,0,3,3,3),row.names=TRUE,  booktabs = TRUE)


## ----"annual proportion used by year by block"----
outtrim <- vector(mode="list",length=nblk); names(outtrim) <- blks
for (blck in 1:nblk) {
  pickB <- which(blb$block == blks[blck])
  ble <- blb[pickB,]
  pickE <- which(ble$maxE > 3.0)
  blet <- ble[pickE,]
  outtrim[[blck]] <- gethexdat(blet)
}


## ----"hexagon_use_through_years-trimmed"-----
#| label: tbl-tabOC9
#| tbl-cap: "A summary of the hexagon use across years for block 6 after all hexagons with maximum effort levels <= 3 minutes are excluded."
#| fig-pos: "H"

  kable(outtrim[[1]],digits=c(0,0,0,0,0,3,3,3),row.names=TRUE,  booktabs = TRUE)


## ----"make prop table"-----------------------
columns <- c("ReefArea","minimum","maximum","average")
proptab <- matrix(0,nrow=nblk,ncol=length(columns),dimnames=list(blks,columns))
for (blck in 1:nblk) {
  x <- outtrim[[blck]]
  proptab[blck,] <- c(x["2019","cumul"],range(x[,"propused"]),mean(x[,"propused"]))
  
}


## ----"reefarea_use_through_years-trimme6"----
#| label: tbl-tabOC10
#| tbl-cap: "The range of the total known reef area used across years for all blocks after all hexagons with maximum effort levels <= 3 minutes are excluded. Block 6 should really not include the last two years as they were exceptionally low."
#| fig-pos: "H"

  kable(proptab,digits=c(0,3,3,3),row.names=TRUE,  booktabs = TRUE)


## ----child="6_appendix.Rmd"------------------

## ----"Blk6 analysis"-------------------------
pickB <- which(blb$block == 6)
ble <- blb[pickB,]
oidoccur <- table(ble$tipy,ble$occur)


## ----"occur vs tipy for blk6"----------------
#| label: tbl-tabAP1
#| tbl-cap: "The frequency of occurrence of each combination of occur and tipy within block 6's dataset."
#| fig-pos: "H"

kable(oidoccur,row.names=TRUE)


## ----"getuniqueoids for 6"-------------------
# Get unique oids for each occur vs tipy combination
oidoccur <- fillmat(oidoccur,replace=0)
unoid <- matrix(0,nrow=32,ncol=8,dimnames=list(1:32,1:8))
unoid[,1] <- oidoccur[,1]
unoid[,8] <- oidoccur[,8]/8
for (i in 2:7) {
  for (j in 1:32) {
    if (oidoccur[j,i] > 0) {
      pickO <- which((ble$occur == i) & (ble$tipy == j))
      unoid[j,i] <- length(unique(ble$oid[pickO]))
    }
  }
}


## ----"uniqueoids for 6"----------------------
#| label: tbl-tabAP2
#| tbl-cap: "The frequency of occurrence of unique hexagons in each combination of occur and tipy within block 6's dataset."
#| fig-pos: "H"

kable(unoid,row.names=TRUE)


## ----"Blk13 analysis"------------------------
pickB <- which(blb$block == 13)
ble <- blb[pickB,]
oidoccur <- table(ble$tipy,ble$occur)


## ----"occur vs tipy for blk13"---------------
#| label: tbl-tabAP3
#| tbl-cap: "The frequency of occurrence of each combination of occur and tipy within block 13's dataset."
#| fig-pos: "H"

kable(oidoccur,row.names=TRUE)


## ----"getuniqueoids for 13"------------------
# Get unique oids for each occur vs tipy combination
oidoccur <- fillmat(oidoccur,replace=0)
unoid <- matrix(0,nrow=32,ncol=8,dimnames=list(1:32,1:8))
unoid[,1] <- oidoccur[,1]
unoid[,8] <- oidoccur[,8]/8
for (i in 2:7) {
  for (j in 1:32) {
    if (oidoccur[j,i] > 0) {
      pickO <- which((ble$occur == i) & (ble$tipy == j))
      unoid[j,i] <- length(unique(ble$oid[pickO]))
    }
  }
}


## ----"uniqueoids for 13"---------------------
#| label: tbl-tabAP4
#| tbl-cap: "The frequency of occurrence of unique hexagons in each combination of occur and tipy within block 13's dataset."
#| fig-pos: "H"

kable(unoid,row.names=TRUE)


## ----"Blk21 analysis"------------------------
pickB <- which(blb$block == 21)
ble <- blb[pickB,]
oidoccur <- table(ble$tipy,ble$occur)


## ----"occur vs tipy for blk21"---------------
#| label: tbl-tabAP5
#| tbl-cap: "The frequency of occurrence of each combination of occur and tipy within block 21's dataset."
#| fig-pos: "H"

kable(oidoccur,row.names=TRUE)


## ----"getuniqueoids for 21"------------------
# Get unique oids for each occur vs tipy combination
oidoccur <- fillmat(oidoccur,replace=0)
unoid <- matrix(0,nrow=32,ncol=8,dimnames=list(1:32,1:8))
unoid[,1] <- oidoccur[,1]
unoid[,8] <- oidoccur[,8]/8
for (i in 2:7) {
  for (j in 1:32) {
    if (oidoccur[j,i] > 0) {
      pickO <- which((ble$occur == i) & (ble$tipy == j))
      unoid[j,i] <- length(unique(ble$oid[pickO]))
    }
  }
}


## ----"uniqueoids for 21"---------------------
#| label: tbl-tabAP5a
#| tbl-cap: "The frequency of occurrence of unique hexagons in each combination of occur and tipy within block 21's dataset."
#| fig-pos: "H"

kable(unoid,row.names=TRUE)


## ----"plottipycatchesblk11"------------------
#| label: fig-figAP6
#| fig-cap: "The mean catch per tipy value by year with 99% Normal CI for example statistical block 11."
#| fig-pos: "H"
#| fig-width: 7
#| fig-height: 8

pickB <- which(blb$block == 11)
blp <- blb[pickB,]
yrs <- sort(unique(blp$year))
nyr <- length(yrs)
meancatch <- matrix(0,nrow=maxt,ncol=nyr,dimnames=list(1:maxt,yrs))
sdcatch <- matrix(0,nrow=maxt,ncol=nyr,dimnames=list(1:maxt,yrs))
countcatch <- matrix(0,nrow=maxt,ncol=nyr,dimnames=list(1:maxt,yrs)) 
parset(plots=pickbound(nyr),margin=c(0.3,0.3,0.05,0.05),
       outmargin=c(1.25,1.25,0,0),byrow=FALSE)
for (i in 1:nyr) { # i = 1
  pickY <- which(blp$year == yrs[i])
  meancatch[,i] <- fillmat(tapply(blp[pickY,"catch"],blp[pickY,"tipy"],mean,na.rm=TRUE))
  sdcatch[,i] <- fillmat(tapply(blp[pickY,"catch"],blp[pickY,"tipy"],sd,na.rm=TRUE))
  countcatch[,i] <- fillmat(rowSums(table(blp$tipy[pickY],blp$occur[pickY])))
  CI <- calcCI(meancatch,sdcatch,countcatch,normCI=TRUE,P=0.95)
  lower <- CI$lower
  upper <- CI$upper
  maxy <- getmax(upper[,i])
  plot(1:maxt,meancatch[,i],type="p",pch=16,cex=0.75,ylim=c(0,maxy),xlab="",
       ylab="")
  for (j in 1:maxt) lines(c(j,j),c(lower[j,i],upper[j,i]),lwd=2)
  text(0.05*maxt,0.95*maxy,paste0("records ",length(pickY)),cex=1,pos=4)
  text(0.05*maxt,0.85*maxy,paste0("oids     ",
                                  length(unique(blp$oid[pickY]))),cex=1,pos=4)
  text(0.05*maxt,0.75*maxy,paste0("hours ",
                    round(sum(blp$minutes[pickY],na.rm=TRUE)/60,3)),cex=1,pos=4)
  text(0.05*maxt,0.65*maxy,paste0("catch ",
                    round(sum(blp$catch[pickY],na.rm=TRUE)/1000,3)),cex=1,pos=4)
  mtext(yrs[i],side=3,line=-1.5,cex=1.25)
}  
mtext("tipy value",side=1,outer=TRUE,cex=1.1)  
mtext("Mean Catch (kg) per tipy value",side=2,outer=TRUE,cex=1.1)  



## ----"plottipycatchesblk13"------------------
#| label: fig-figAP7
#| fig-cap: "The mean catch per tipy value by year with 99% Normal CI for example statistical block 13."
#| fig-pos: "H"
#| fig-width: 7
#| fig-height: 8

pickB <- which(blb$block == 13)
blp <- blb[pickB,]
yrs <- sort(unique(blp$year))
nyr <- length(yrs)
meancatch <- matrix(0,nrow=maxt,ncol=nyr,dimnames=list(1:maxt,yrs))
sdcatch <- matrix(0,nrow=maxt,ncol=nyr,dimnames=list(1:maxt,yrs))
countcatch <- matrix(0,nrow=maxt,ncol=nyr,dimnames=list(1:maxt,yrs)) 
parset(plots=pickbound(nyr),margin=c(0.3,0.3,0.05,0.05),
       outmargin=c(1.25,1.25,0,0),byrow=FALSE)
for (i in 1:nyr) { # i = 1
  pickY <- which(blp$year == yrs[i])
  meancatch[,i] <- fillmat(tapply(blp[pickY,"catch"],blp[pickY,"tipy"],mean,na.rm=TRUE))
  sdcatch[,i] <- fillmat(tapply(blp[pickY,"catch"],blp[pickY,"tipy"],sd,na.rm=TRUE))
  countcatch[,i] <- fillmat(rowSums(table(blp$tipy[pickY],blp$occur[pickY])))
  CI <- calcCI(meancatch,sdcatch,countcatch,normCI=TRUE,P=0.95)
  lower <- CI$lower
  upper <- CI$upper
  maxy <- getmax(upper[,i])
  plot(1:maxt,meancatch[,i],type="p",pch=16,cex=0.75,ylim=c(0,maxy),xlab="",
       ylab="")
  for (j in 1:maxt) lines(c(j,j),c(lower[j,i],upper[j,i]),lwd=2)
  text(0.05*maxt,0.95*maxy,paste0("records ",length(pickY)),cex=1,pos=4)
  text(0.05*maxt,0.85*maxy,paste0("oids     ",
                                  length(unique(blp$oid[pickY]))),cex=1,pos=4)
  text(0.05*maxt,0.75*maxy,paste0("hours ",
                    round(sum(blp$minutes[pickY],na.rm=TRUE)/60,3)),cex=1,pos=4)
  text(0.05*maxt,0.65*maxy,paste0("catch ",
                    round(sum(blp$catch[pickY],na.rm=TRUE)/1000,3)),cex=1,pos=4)
  mtext(yrs[i],side=3,line=-1.5,cex=1.25)
}  
mtext("tipy value",side=1,outer=TRUE,cex=1.1)  
mtext("Mean Catch (kg) per tipy value",side=2,outer=TRUE,cex=1.1)  



## ----"plottipycatchesblk21"------------------
#| label: fig-figAP8
#| fig-cap: "TThe mean catch per tipy value by year with 99% Normal CI for example statistical block 21."
#| fig-pos: "H"
#| fig-width: 7
#| fig-height: 8

pickB <- which(blb$block == 21)
blp <- blb[pickB,]
yrs <- sort(unique(blp$year))
nyr <- length(yrs)
meancatch <- matrix(0,nrow=maxt,ncol=nyr,dimnames=list(1:maxt,yrs))
sdcatch <- matrix(0,nrow=maxt,ncol=nyr,dimnames=list(1:maxt,yrs))
countcatch <- matrix(0,nrow=maxt,ncol=nyr,dimnames=list(1:maxt,yrs)) 
parset(plots=pickbound(nyr),margin=c(0.3,0.3,0.05,0.05),
       outmargin=c(1.25,1.25,0,0),byrow=FALSE)
for (i in 1:nyr) { # i = 1
  pickY <- which(blp$year == yrs[i])
  meancatch[,i] <- fillmat(tapply(blp[pickY,"catch"],blp[pickY,"tipy"],mean,na.rm=TRUE))
  sdcatch[,i] <- fillmat(tapply(blp[pickY,"catch"],blp[pickY,"tipy"],sd,na.rm=TRUE))
  countcatch[,i] <- fillmat(rowSums(table(blp$tipy[pickY],blp$occur[pickY])))
  CI <- calcCI(meancatch,sdcatch,countcatch,normCI=TRUE,P=0.95)
  lower <- CI$lower
  upper <- CI$upper
  maxy <- getmax(upper[,i])
  plot(1:maxt,meancatch[,i],type="p",pch=16,cex=0.75,ylim=c(0,maxy),xlab="",
       ylab="")
  for (j in 1:maxt) lines(c(j,j),c(lower[j,i],upper[j,i]),lwd=2)
  text(0.05*maxt,0.95*maxy,paste0("records ",length(pickY)),cex=1,pos=4)
  text(0.05*maxt,0.85*maxy,paste0("oids     ",
                                  length(unique(blp$oid[pickY]))),cex=1,pos=4)
  text(0.05*maxt,0.75*maxy,paste0("hours ",
                    round(sum(blp$minutes[pickY],na.rm=TRUE)/60,3)),cex=1,pos=4)
  text(0.05*maxt,0.65*maxy,paste0("catch ",
                    round(sum(blp$catch[pickY],na.rm=TRUE)/1000,3)),cex=1,pos=4)
  mtext(yrs[i],side=3,line=-1.5,cex=1.25)
}  
mtext("tipy value",side=1,outer=TRUE,cex=1.1)  
mtext("Mean Catch (kg) per tipy value",side=2,outer=TRUE,cex=1.1)  



## ----"proportions of occur11"----------------
#| label: tbl-tabAP6
#| tbl-cap: "The count of hexagons in block 11 having a maxE value <= 3 in each combination of tipy and occur (see appendix for the other block results."
#| fig-pos: "H"


countmaxE <- blkans[[2]]$countmaxE
kable(countmaxE[1:8,],row.names=TRUE)


## ----"proportions of occur13"----------------
#| label: tbl-tabAP7
#| tbl-cap: "The count of hexagons in block 13 having a maxE value <= 3 in each combination of tipy and occur (see appendix for the other block results."
#| fig-pos: "H"

countmaxE <- blkans[[3]]$countmaxE
kable(countmaxE[1:8,],row.names=TRUE)


## ----"proportions of occur21"----------------
#| label: tbl-tabAP8
#| tbl-cap: "The count of hexagons in block 21 having a maxE value <= 3 in each combination of tipy and occur (see appendix for the other block results."
#| fig-pos: "H"

countmaxE <- blkans[[4]]$countmaxE
kable(countmaxE[1:8,],row.names=TRUE)


## ----"hexagon_use_through_years11"-----------
#| label: tbl-tabAP9
#| tbl-cap: "A summary of the hexagon use across years for block 11."
#| fig-pos: "H"


  pickB <- which(blb$block == 11)
  ble <- blb[pickB,]
  outall <- gethexdat(ble)
  kable(outall,digits=c(0,0,0,0,0,3,3,3),row.names=TRUE)


## ----"hexagon_use_through_years-trimmed11"----
#| label: tbl-tabAP10
#| tbl-cap: "A summary of the hexagon use across years for block 11 after all hexagons with maximum effort levels <= 3 minutes are excluded."
#| fig-pos: "H"


  kable(outtrim[[2]],digits=c(0,0,0,0,0,3,3,3),row.names=TRUE)


## ----"hexagon_use_through_years13"-----------
#| label: tbl-tabAP11
#| tbl-cap: "A summary of the hexagon use across years for block 13."
#| fig-pos: "H"


  pickB <- which(blb$block == 13)
  ble <- blb[pickB,]
  outall <- gethexdat(ble)
  kable(outall,digits=c(0,0,0,0,0,3,3,3),row.names=TRUE)


## ----"hexagon_use_through_years-trimmed13"----
#| label: tbl-tabAP12
#| tbl-cap: "A summary of the hexagon use across years for block 13 after all hexagons with maximum effort levels <= 3 minutes are excluded."
#| fig-pos: "H"


  kable(outtrim[[3]],digits=c(0,0,0,0,0,3,3,3),row.names=TRUE)


## ----"hexagon_use_through_years21"-----------
#| label: tbl-tabAP13
#| tbl-cap: "A summary of the hexagon use across years for block 21."
#| fig-pos: "H"

  pickB <- which(blb$block == 21)
  ble <- blb[pickB,]
  outall <- gethexdat(ble)
  kable(outall,digits=c(0,0,0,0,0,3,3,3),row.names=TRUE)


## ----"hexagon_use_through_years-trimmed21"----
#| label: tbl-tabAP14
#| tbl-cap: "A summary of the hexagon use across years for block 21 after all hexagons with maximum effort levels <= 3 minutes are excluded."
#| fig-pos: "H"

  kable(outtrim[[4]],digits=c(0,0,0,0,0,3,3,3),row.names=TRUE)


