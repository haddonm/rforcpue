
cat("\n\n GLM based on CE and Gamma errors with a log link \n\n")

if (dir.exists("C:/Users/Malcolm/Dropbox")) {
  ddir <- "C:/Users/Malcolm/Dropbox"
} else {
  ddir <- "C:/Users/User/Dropbox"
}
datadir <- paste0(ddir,"/","AbaloneData")

wkdir <- "./../../manuscripts/abalonecpue/"
rawdir <- paste0(ddir,"/rcode2/rforcpue/data-raw/")

options("show.signif.stars"=FALSE,"stringsAsFactors"=FALSE,
        "max.print"=50000,"width"=240)

library(rutilsMH)
library(rforcpue)    # https://github.com/haddonm
library(makehtml)

source("C:/Users/User/Dropbox/rcode2/rforcpue/data-raw/sourcefile.R")


# First get the data
datafile <- paste0(datadir,"/CatchEffortData_2020-02-11.RData")
load(datafile)
ab <- abCEbl # simplify, simplify, simplify! Don't you just mean simplify?
columns <- colnames(ab) 
replace <- c("diver_id","fishing_date","fishyear","fishmonth","blockno",
             "subblockno","newzone","totalcatch","propblip",
             "entitlement.num","blprop.factor")
repwith <- c("diver","date","year","month","block","subblock","zone",
             "allcatch","pblip","entitle","percbl")
colnames(ab) <- tidynames(columns,replace,repwith)
ab$ports <- cleanname(ab$ports)
ab$take_vessel <- cleanname(ab$take_vessel)
pick <- which(ab[,"year"] > 2019) # lastyr set to 2019 to remove
ab <- droplevels(ab[-pick,])      # any 2020 records
dim(ab)

# make selection using criteria described in supplementary material
pickrec2 <- which((ab$hours > 0) & (ab$hours < 12) &
                    (ab$catch > 0) & (ab$catch < 1300) &
                    (ab$cpue < 340) & (ab$cpue > 5.0) & (ab$zone == "E") &
                    (ab$block == 13))
ab1 <- droplevels(ab[pickrec2,])
ab3 <- droplevels(ab[-pickrec2,])
dim(ab1)
properties(ab1)

# add total catch and number of active years to ab1
ab2 <- addcntcat(ab1)
properties(ab2)


mod4 <- makeonemodel(labelM)# First a simple linear model
splabel <- "Block 14 Blacklip" # with 83000 records this takes time
labelM <- c("year","diver","take_vessel","month","ports","numdivers")
#pick <- which (ab2$count > 1)
ab3 <- makecategorical(labelM,ab2) # only use divers >1 year
mods <- makemodels(labelM)
model1 <- standLM(mods,ab3,splabel,console=TRUE) 

geom <- geomean(ab2$cpue)
pickB <- which((ab$block == 13) & (ab$zone == "E"))
yrtotC <- tapply(ab$catch[pickB],ab$year[pickB],sum,na.rm=TRUE)/1000
plotprep(width=7,height=5,newdev=FALSE)
plotstand(model1,bars=TRUE,geo=geom,catch=yrtotC)

labelM <- c("year","diver","month","numdivers")
#pick <- which (ab2$count > 1)
ab3 <- makecategorical(labelM,ab2) # only use divers >1 year
mods <- makemodels(labelM)
model2 <- standLM(mods,ab3,splabel,console=TRUE) 

geom <- geomean(ab2$cpue)
pickB <- which((ab$block == 13) & (ab$zone == "E"))
yrtotC <- tapply(ab$catch[pickB],ab$year[pickB],sum,na.rm=TRUE)/1000
plotprep(width=7,height=5,newdev=FALSE)
plotstand(model2,bars=TRUE,geo=geom,catch=yrtotC)

plotprep(width=6,height=7,newdev=FALSE)
impactplot(model2)

mod4 <- makeonemodel(labelModel = labelM, dependent="cpue")
model4 <- glm(mod4,family=Gamma(link="log"),data=ab3) # use the same data
m4 <- summary(model4)$coefficients
yrparm4 <- getfact(m4,"year",biascorrect=FALSE)
yrparm1 <- getfact(model1$optModel,"year",biascorrect=TRUE)
ans <- round(cbind(yrparm1[,"Scaled"],yrparm4[,"Scaled"]),4)

plotprep(width=7, height=6,newdev=FALSE)
parset(plots=c(2,1))
plot(1992:2019,ans[,1]*geom,type="l",lwd=2,ylim=c(0,100),panel.first=grid())
points(1992:2019,ans[,2]*geom,pch=16,cex=1.2)
plot(1992:2019,(ans[,2]-ans[,1])*geom,type="p",pch=16,cex=1.2,panel.first=grid())
abline(h=0.0,col=3)

plotprep(width=7, height=6,newdev=FALSE)
parset(plots=c(2,2))
plot(model4)


rown <- as.numeric(rownames(ab2))
pick <- which(rown == 15453)
ab2[15453,]

m4yr <- m4[1:30,]
m4yr[1,] <- c(0,0,0,0)
m4pars <- exp(m4yr[,"Estimate"]) # no bias correction necessary
answer4 <- cbind(m4pars,m4pars/mean(m4pars))
colnames(answer4) <- c("m4pars","Scaled")
back4 <- cbind(lmgeo,answer1[,"Scaled"],answer2[,"Scaled"],answer3[,"Scaled"],
               answer4[,"Scaled"])
colnames(back4) <- c("lm_geo","lm_LnCE","glm_LnCE","glm_CE_Log","glm_CE_Gamma")
back4
AIC(model1, model2, model3, model4)



# test factor properties ------------------------------------------------------
source("C:/Users/User/Dropbox/rcode2/rforcpue/data-raw/sourcefile.R")

resdir <- "C:/Users/User/Dropbox/rcode2/rforcpue/data-raw/blk13E"
dirExists(resdir)
runname <- "firstrun"
resfile <- setuphtml(resdir,runname)

properties(ab2)

labelM <- c("year","diver","month","numdivers")
nfact <- length(labelM)


yearprops <- yearprop(ab2, resdir, runname, resfile)
for (fct in 2:length(labelM)) {
  dofact <- labelM[fct]
  tmp <- factorprop(ab2,fact=dofact,resdir=resdir,runname=runname,resfile=resfile)
}

make_html(replist=NULL,resdir=resdir,packagename = "rforcpue")


getfile <- filenametopath(resdir,"ports_firstrun.csv")
dat <- read.csv(getfile,header=TRUE)
head(dat,20)
dats <- dat[order(dat[,"totalC"],decreasing = TRUE),]
datsp <- cbind(dats,prop=dats[,"totalC"]/sum(dats[,"totalC"]))
datsp <- cbind(datsp,cumprop=cumsum(datsp[,"prop"]),cumsum(dats[,"totalC"]))


