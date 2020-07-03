---
title: "rforcpue"
author: "Malcolm Haddon"
date: "2020-07-01"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Data Exploration and CPUE Standardization}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---






## Catch Effort and Data Exploration

Before proceeding to generating a statistical standardization of commercial catch rates, data exploration, perhaps through plotting up different variables and how they might change through the years, or relative to one another, can often be informative about changes in any fishery for a particular species. The __rforcpue__ R package, in combination with __rutilsMH__, includes an array of functions that should assist with such data exploration and standardization. If a species' fishery includes CPUE data then plots of the distribution of catches, effort, and CPUE (perhaps as log(CPUE)) can be helpful in the interpretation of such CPUE, especially if there is sufficient data to allow for CPUE standardization. __rforcpue__ includes numerous functions that can assist with CPUE standardization. All of these functions are described below with examples of their use. 

There should be no expectation that the functions described here to be used in the standardization of CPUE constitute anything like a complete treatment. This vignette only provides a very brief introduction or pointer to get people started in the very broad field of statistical standardization of CPUE. There are many aspects not considered (e.g. how or whether to treat zeros). This vignette remains a draft and if you find errors, omissions, or obscurities do please let me know (see DESCRIPTION for email address). In addition if you wish to reference this package you can obtain one by typing `citation("rforcpue")` into the console, which will give you the latest version. The developmental version is held in my github repository _www.github.com/haddonm/rforcpue_, from where it can be installed or cloned.

The help written for each function can be accessed by typing ?<function-name>.
For example C:/~/R-4.0.0/library/rforcpue/help/addnorm. In RStudio that will bring up a help page that provides details of the function and its syntax, plus a working example that can be copied into the console to illustrate the working of the particular function. More generally, if you scroll to the bottom of any help page at the bottom you will see a hyperlink 'Index' that will take you to a complete list of the help pages available within __cede__. At the top of that list are links to the package Description and to this vignette. Another way to access the vignette is to type _browseVignettes("cede")_ into the console.

## Data Exploration

The main data set included with __cede__ is called _sps_ and contains typical fisheries data from a scalefish fishery. It is there mainly to assist with learning the operation and use of the different functions. Generally it would be better to use your own data but if you consider the _sps_ data set you will gain an understanding of a typical format. The _sps_ dataset is modelled on real fisheries data but is greatly altered in all of its variables. This reflects an on-going issue that fisheries scientists need to deal with, which is the need for confidentiality. It would have been better had I simply included some real data for a known fishery. This could have been done but then details of location and of which vessel was doing the fishing for each record would need to be removed. As spatial information and the vessel fishing are very often highly influential, omitting such data would have defeated the primary purpose of including the data. Fortunately, the results of standardization can usually be displayed, but the raw data needs to be treated carefully. Similar considerations need to be given to each separate analysis to ensure that issues of confidentiality are not compromised. This is especially important when using the sketch mapping functions in __cede__. Such sketch maps can be very useful for detecting spatial heterogeneity but very often cannot be widely displayed. Nevertheless, such information should be considered even if not disseminated.  


```r
data(sps)
head(sps)
```

```
##   Year Month Vessel catch_kg     Long       Lat Depth DayNight Effort Zone
## 1 2004     4      1      220 145.1167 -43.06667   125        N   4.00    1
## 2 2004     4      1      280 145.2500 -43.23333   130        M   3.66    1
## 3 2004     4      1      180 145.1500 -43.08333   115        D   3.50    1
## 4 2004     4      1       70 145.2333 -43.21667   120        N   4.75    1
## 5 2004     4      1      200 145.1000 -43.03333   120        M   4.75    1
## 6 2004     4      1      100 145.7667 -43.68333   130        M   2.01    1
```

```r
properties(sps)
```

```
##          Index isNA Unique     Class      Min   Max   Example
## Year         1    0     12   numeric     2003  2014      2004
## Month        2    0     12   numeric        1    12         4
## Vessel       3    0     23   numeric        1    27         1
## catch_kg     4    0    442   numeric        1  4500       220
## Long         5    0    447   numeric 144.1167 146.3  145.1167
## Lat          6    0    512   numeric     <NA>  <NA> -43.06667
## Depth        7    0    191   numeric        2   366       125
## DayNight     8    0      3 character        0     0         N
## Effort       9    0    377   numeric     0.16  9.66         4
## Zone        10    0      3   numeric        1     3         1
```

The _properties_ function categorizes the contents of a data.frame, counting the number of NAs in each variable, if any, listing their class, their minimum and maximum (if applicable) and finally printing an example of the contents. I find this function quite useful when beginning to use a different data.frame. Generally I refer to variables within a data.frame by their names, as in, for example, `yrs = sort(unique(sps[,"Year"])`, so it is important to know if the names are capitalized or not, as well as knowing exactly which variables are present.

Once we have our data available for analysis it is often a good idea to find ways to summarize how they vary relative to one another. With fisheries data it is common to want to know how different factors influence the total catch or effort and whether these vary by year. Typically one might use the R function _tapply_ to conduct such examinations. I tend to use this function many times so to simplify its use one can use the _tapsum_ function from within __cede__.

For example, the seasonality of catches can be indicative of the typical behaviour of the fishery within a year.


```r
round(tapsum(sps,"catch_kg","Year","Month"),2)
```

```
##           1     2     3      4     5     6     7     8     9    10    11    12
## 2003  33.61 26.02 37.29  30.36 14.73  3.72  4.77 11.58 14.46  5.14  6.38 33.83
## 2004  73.71 66.19 52.70 100.79 55.94 18.26 12.69 22.76  8.38  9.37 30.08 21.65
## 2005 114.92 83.87 34.96  37.42  7.35 15.12 11.82  6.09  4.07 13.32 13.94 36.01
## 2006  79.80 53.14 45.80  27.37  0.26  1.83  2.77  3.06  0.44  5.06  9.20 55.66
## 2007  31.81 60.13 27.33   1.55 13.64  4.58  2.47  0.77  0.26  0.16  7.03 20.65
## 2008  76.32 21.60 33.00   5.45  2.07  0.72  1.33  0.46  0.20  3.16  6.38 14.12
## 2009  16.65 25.36  9.54   2.55  2.38  0.68  0.58  1.98  0.67  6.75 18.23 11.18
## 2010  40.93 22.47 11.36   1.99  0.26  0.48  1.79  2.33  1.40  1.58  0.71  4.39
## 2011  25.00 38.58 10.61   6.30  2.73  3.19  1.51  2.72  2.13  2.15  5.11 23.46
## 2012  35.34 49.44 24.92   6.41  2.91  2.57  5.08  1.60  1.62  3.45  4.43 13.09
## 2013  47.32 48.78 41.04  10.98 17.09  0.31  2.28  0.54  1.31  6.63  6.34  6.35
## 2014  10.95 10.30 21.48  12.06  6.37 11.04  8.06 15.48  3.90  3.82 26.62 49.43
```

Then we might examine the catch (or effort, or etc, etc) by zone where the zones are in sequence along the coast (or they would be if this was a real fisheries data).


```r
tapsum(sps,"catch_kg","Year","Zone")
```

```
##             1         2      3
## 2003  94.6190  98.06400 29.197
## 2004 215.2230 210.47900 46.804
## 2005 112.7670 216.02300 50.079
## 2006  82.4370 120.29100 81.663
## 2007  42.7560  91.46240 36.161
## 2008  51.9840  93.81300 19.020
## 2009  33.9920  33.62310 28.931
## 2010  11.8070  18.71400 59.165
## 2011  37.1840  79.41725  6.892
## 2012  55.2330  65.35600 30.263
## 2013  50.3015  83.81800 54.848
## 2014  46.6240  81.44250 51.455
```

We are not limited to summarizing catch but, for example could also look at the distribution of effort as total number of hours (note the change to the default value of div so that the total number of hours is not divided by 1000). By pointing the function call to a new object, in this case _effbyyr_, one can then plot the results.


```r
effbyyr <- tapsum(sps,"Effort","Year","Zone",div=1.0)
effbyyr
```

```
##            1       2      3
## 2003 2473.36 1998.01 724.13
## 2004 3558.32 2541.13 709.58
## 2005 2095.92 2750.78 639.01
## 2006 2001.37 2055.52 941.46
## 2007 1192.94 1279.45 481.96
## 2008 1426.79 1072.82 495.61
## 2009  877.81  739.13 488.86
## 2010  471.06  493.39 691.16
## 2011  855.54 1185.06 293.93
## 2012 1278.07  981.93 508.41
## 2013 1323.23  960.89 816.47
## 2014 1036.63 1222.02 681.42
```


```r
# plotprep(width=7,height=4.5)
ymax <- max(effbyyr,na.rm=TRUE)
label <- colnames(effbyyr)
yrs <- as.numeric(rownames(effbyyr))
par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05)) 
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7) 
plot(yrs,effbyyr[,label[1]],type="l",lwd=2,col=1,ylim=c(0,ymax),
     ylab="Total Effort (Hours) by Zone per Year",xlab="",
     panel.first=grid())
lines(yrs,effbyyr[,label[2]],lwd=2,col=2)
lines(yrs,effbyyr[,label[3]],lwd=2,col=3)
# the "topright"" can also be "bottomright", "bottom", "bottomleft", "left", 
# # "topleft", "top", "right" and "center". Alternatively one can give an
# x and a y location, see ?legend, which is a function from graphics package
legend("topright",label,col=c(1,2,3),lwd=3,bty="n",cex=1.25)
```

<img src="rforcpue_files/figure-html/ploteffort-1.png" style="display: block; margin: auto;" />

__Figure 1.__ A plot of total effort by zone, showing that a visual illustration can often more easily highlight changes in a fishery's dynamics.



DayNight is another factor that can have large consequences for catches and catch rates. Check the description of the _sps_ data set using `?sps to see details concerning each field in the data.frame.


```r
tapsum(sps,"catch_kg","Year","DayNight")
```

```
##              D        M       N
## 2003  80.54300  81.3930 59.9440
## 2004 226.67300 153.7910 92.0420
## 2005 157.21800 133.5640 88.0870
## 2006 127.24900 104.6120 52.5300
## 2007  72.13700  61.5024 36.7400
## 2008  75.67900  56.9030 32.2350
## 2009  35.10710  34.7680 26.6710
## 2010  39.00500  25.8060 24.8750
## 2011  46.14625  44.6535 32.6935
## 2012  52.92000  59.4950 38.4370
## 2013  72.16750  66.8170 49.9830
## 2014  52.40750  64.0420 63.0720
```

One of the most influential factors within each fishery is the vessel doing the catching. Often this is also a reflection of the skipper of the vessel as well as the relative performance of the boat itself. If both skipper and vessel are available then use both. Nevertheless, it is often the case the vessel name is the only information available about the vessel's fishing power relative to other vessels (or divers, etc) in the fleet. It is possible to pay special attention to catch-per-vessel, although the following analysis is more general than that and can be applied to, for example, catch-by-month relative to Depth Category.


```r
cbv <- tapsum(sps,"catch_kg","Vessel","Year") # often more vessels than years
total <- rowSums(cbv,na.rm=TRUE)
cbv1 <- cbv[order(total),]   # sort by total catch smallest catches first
round(cbv1,2)
```

```
##     2003   2004  2005  2006  2007  2008  2009  2010  2011  2012  2013  2014
## 21  0.06     NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
## 27    NA     NA    NA    NA    NA  0.15    NA    NA    NA    NA    NA    NA
## 16  0.12     NA  0.20    NA    NA    NA    NA    NA    NA    NA    NA    NA
## 20  0.81     NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
## 24    NA     NA  0.18    NA  0.65    NA    NA    NA    NA    NA    NA    NA
## 23    NA     NA  1.23    NA    NA    NA    NA    NA    NA    NA    NA    NA
## 19  0.01     NA    NA    NA    NA    NA  0.13  0.04  0.79  0.36  0.25  0.23
## 17  2.46     NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
## 25    NA     NA    NA    NA    NA    NA    NA  0.10  0.10    NA  0.66  2.84
## 11  0.01   3.66  7.55    NA    NA    NA    NA    NA    NA    NA    NA    NA
## 4   0.34  10.96    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
## 12  2.96   4.91  1.25  3.04    NA    NA  0.06    NA    NA    NA    NA    NA
## 14  6.21   0.32  0.60  4.57  6.05  1.19  0.48    NA    NA    NA  0.03  0.10
## 10  9.81  17.97 22.77 21.99    NA    NA    NA    NA    NA    NA    NA    NA
## 9   1.25   9.35  1.41  6.10 14.96  6.44  2.11  1.90  5.76  3.18 12.46 13.25
## 6  19.41  41.67  3.66 13.76    NA    NA    NA    NA    NA    NA    NA    NA
## 8  38.62  32.11 17.24    NA    NA    NA    NA    NA    NA    NA    NA    NA
## 5   0.12  27.66 40.81 29.50    NA    NA    NA    NA    NA    NA    NA  9.21
## 13  8.60   8.22 10.05  4.29 26.70  9.54  1.14  4.02  0.30  9.35  8.89 17.10
## 3  31.57  15.67 39.95 32.83 21.66 16.29 12.29 25.78 20.03 21.40 41.68 15.00
## 7  45.35  37.83 61.51 49.74 14.10 20.72 11.46  6.15  6.30 16.17 23.05  3.22
## 1   1.68 107.33 73.13 32.57 23.28 25.06 18.34 17.11 30.70 42.87 31.42 80.22
## 2  52.48 154.84 97.33 86.00 62.99 85.43 50.53 34.59 59.50 57.52 70.52 38.36
```

Obviously some vessels will be much more influential than others simply because they catch a great deal more than others and hence introduce many more records into the database. Similar things could be said about effort, but also the number of records present for a particular vessel will influence any subsequent analysis.



```r
# plotprep(width=8,height=6) # not needed in the vignette
to <- turnover(cbv1)
yearBubble(cbv1,ylabel="sqrt(catch-per-vessel)",diam=0.125,txt=c(2,3,4,5),
           hline=TRUE)
```

<img src="rforcpue_files/figure-html/yeasrbubble-1.png" style="display: block; margin: auto;" />

__Figure 2.__ This hypothetical fishery is clearly dominated by four or five vessels with numerous minor players. Additionally, before 2007 there were a few more productive fishers present (this reflects the structural adjustment in the Commonwealth from which this simulated data derives). The optional horizontal lines merely delineate the individual vessels; leave out _hline_ for a cleaner plot with less ink. The top two rows of numbers is the total catch per year and the bottom row of numbers is the number of vessels reporting in each year.


It is likely that if the data from the bottom nine vessels were omitted there would be no effect on any results as their catches are so minor in a relative sense. It is clear those vessels are merely casual occurrences within the fishery. In a real fishery one might expect many more active vessels.

While the main vessels are reasonably consistent in terms of reporting from this fishery, other vessels came and went. To summarize such activity one can use the _turnover_ function which summarizes the year-to-year changes in which vessels report being active.


```r
print(to)
```

```
##      Continue Leave Start Total
## 2003       19     0     0    19
## 2004       14     5     0    14
## 2005       13     1     3    16
## 2006       11     5     0    11
## 2007        7     4     1     8
## 2008        7     1     1     8
## 2009        7     1     2     9
## 2010        7     2     1     8
## 2011        8     0     0     8
## 2012        7     1     0     7
## 2013        7     0     2     9
## 2014        9     0     1    10
```

The _Continue_ column lists how many vessels continued from the preceding year, the _Leave_ column designates how many left relative to the previous year, while the _Start_ column is literally how many started reporting in that year. The _Total_ is the total reporting in each year. No attempt is made to follow individual vessels, for which a separate function would need to be written. This distinction is necessary because some vessels start reporting and then only occasionally report from a fishery until they eventually stop reporting. If such vessels were classed as continuing across their initial year and final year, irrespective of whether they fished in every year, that would generate a somewhat different matrix of catch-by-vessel. Which analytical strategy to use would depend very much on what question is being asked.

### The Addition of CPUE data

You will have noticed that the data came with catch and effort but not CPUE, so we need to calculate that. In the following I test for the presence of zeros in the catch and effort to avoid generating errors of division and when taking logs (divide-by-zero errors will compromise the analysis). In fact, as the _properties_ call showed there were no _NA_ values, but it remains worth checking when dealing with real data. While we are adding CPUE we can also group the depth data into depth classes to provide that option when standardizing the CPUE data. We will include a brief discussion of continuous versus categorical factors later on.


```r
sps$CE <- NA     # make explicit space in the data.frame
sps$LnCE <- NA
pick <- which((sps$catch_kg > 0) & (sps$Effort > 0))
sps$CE[pick] <- sps$catch_kg[pick]/sps$Effort[pick]
sps$LnCE[pick] <- log(sps$CE[pick])   # natural log-transformation
# categorize Depth
range(sps$Depth,na.tm=TRUE)  # to aid selection of depth class width
```

```
## [1]   1 366
```

```r
sps$DepCat <- NA
sps$DepCat <- trunc(sps$Depth/25) * 25
table(sps$DepCat)
```

```
## 
##    0   25   50   75  100  125  150  175  200  225  250  275  300  325  350 
##    6   19  224 1569 4583 3593 1393   74   66   21   15   21    7    5    7
```


It is clear from the table of records by depth that most of the fishing occurs in waters of 150 meters or less.

Tables of numbers are very informative but sometimes it is much easier to gain a visual impression of patterns in one's data by plotting them. Typically, with fisheries data, one might plot each variable, such as catch, effort, log(CPUE), depth, etc, by year to see whether changes have occurred through time. Such changes might affect any analysis applied so it is always a good idea to examine (explore) one's data before using it. __cede__ provides a function _histyear_ that can plot a histogram of a selected variable by year. Use C:/~/R-4.0.0/library/rforcpue/help/histyear to see details of this function. 


```r
outH <- histyear(sps,Lbound=-1.75,Rbound=8.5,inc=0.25,pickvar="LnCE",
                 years="Year",varlabel="log(CPUE)",plots=c(4,3))
```

<img src="rforcpue_files/figure-html/cebyyr-1.png" style="display: block; margin: auto;" />

__Figure 3.__ The distribution of the log(CPUE) each year for which data is available. The green lines are fitted normal distributions put there purely for reference (log-transformation should normalize log-normal data, but note the spikiness in some of the plots).



```r
outH <- histyear(sps,Lbound=0,Rbound=375,inc=12.5,pickvar="Depth",
                 years="Year",varlabel="Depth (m)",plots=c(4,3),vline=120)
```

<img src="rforcpue_files/figure-html/depthbyyr-1.png" style="display: block; margin: auto;" />

__Figure 4.__ The distribution of reported mean depth of fishing each year. The green lines are fitted normal distributions there for reference, the blue lines, from the vline parameter are merely reference lines to ease comparisons between years.




```r
outH <- histyear(sps,Lbound=0,Rbound=10,inc=0.25,pickvar="Effort",
                 years="Year",varlabel="Effort ('000s Hrs)",plots=c(4,3),
                 vline=NA)
```

<img src="rforcpue_files/figure-html/depthbyyr2-1.png" style="display: block; margin: auto;" />

__Figure 5.__ The distribution of reported Effort each year. The green lines are fitted normal distributions there for reference. Note the spikes of reporting at four hours.


Spikes can be seen in each of the graphs and the question needs to arise whether this is due to rounding by the fishers or is a real phenomenon. In fact, unless dealing with counts of fish caught (quite possible in some fisheries) then rounding invariably occurs when estimating catches but also in effort.


```r
par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05)) 
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)  
plot(sps$Effort,sps$catch_kg,type="p",pch=16,col=rgb(1,0,0,1/5),
     ylim=c(0,500),xlab="Effort (Hrs)",ylab="Catch (Kg)")
abline(h=0.0,col="grey")
```

<img src="rforcpue_files/figure-html/catchvseffort-1.png" style="display: block; margin: auto;" />

__Figure 6.__ A plot of catch against effort for each record in the _sps_ data.frame. The catch axis has been truncated at 500 kg so as to allow the rounding of catches to be less compressed and more visually obvious. It should be clear there is rounding at every half hour between 2 - 6 hours. In addition, there is rounding at about 30 kg steps from 30 - 300 kg, with other categories above that. The 30-33kg rounding reflects a belief that a standard fish bin contains about 30-33Kg of fish.


The uneven grid like nature of the catch and effort data is reflected in the CPUE data, which might (ought to?) make one skeptical about the notion of a statistical model attempting to predict such values. While the residual errors between the predicted and observed cpue values (which is what fitting the statistical model is trying to minimize) might be described by a less granular statistical distribution they do derive from a comparison of smooth predicted values with the grouped observed values, so any results are likely to be uncertain and to under-estimate any inherent variation. The presence of such rounding behaviour in the reporting also implies that arguing about exactly which statistical distribution to use to provide a description of the residual distribution is often not useful. Consistent treatment through time may be more valuable for communicating results, but if alternatives are suggested then a simple solution is to present the results from each alternative for comparison.

Despite such problems it is possible to derive useful information from fisheries data. It is generally recognized that fisheries data in general is noisy and potentially contains many errors, especially when considering the less important species that fall into the data-poor category. Nevertheless, the challenge remains one of attempting to obtain useful and usable information from analyzing such data.

### Plotting Sketch Maps of Spatial Data

Since the advent of GPS and GPS plotters very many fishers use this equipment and fisheries departments have started to ask for precise location data accordingly. If such latitude and longitude data (or eastings and northings) are available it is often informative to plot such data as a sketch map to illustrate the focus and range of a fishery. __cede__ also provides the capacity to generate such sketch maps instead of using a full GIS. The idea here is not to conduct detailed spatial analyses, for which a GIS is better suited. Instead the idea is simply to gain a rapid impression of the operation of a fishery. Of course, care needs to be taken with such plots as they very obviously contain confidential information (such as exactly where fishers have been operating). This is especially important when there are very few fishers involved in a fishery. So while such images may not be able to be displayed in meetings they remain useful for data exploration purposes.


```r
leftlong <- 143.0;  rightlong <- 150.0
uplat <- -40.0;  downlat <- -44.6
plotaus(leftlong,rightlong,uplat,downlat,gridon=1.0)
addpoints(sps,intitle="Location of Catches")
```

```
## 11603 1 4500
```

```r
plotLand(incol="blue")
```

<img src="rforcpue_files/figure-html/sketchmap1-1.png" style="display: block; margin: auto;" />

__Figure 7.__ A sketch map of the the Lat Long data within the _sps_ data set. There are clearly a number of points reported to be out over what would be abyssal plain, but the majority of points define the range of the fishery.


Rather than show individual points it is also possible, by using the function _plotpolys_, to aggregate catches into different geographical sub-divisions (e.g. 0.25 or 0.5 degree squares, definable with the _gridon_ parameter). If these are coloured relative to the density of total catches the locations where most of the yield of a fishery derives from becomes apparent. The output, from the function includes the plotting but also the sub-divisions used and the counts of each of those sub-divisions. The final plotting of the land is merely to provide a tidy looking plot.



```r
leftlong <- 143.0;  rightlong <- 150.0
uplat <- -40.0;  downlat <- -44.6
plotaus(leftlong,rightlong,uplat,downlat,gridon=1.0)
plotpolys(sps,leftlong,rightlong,uplat,downlat,gridon=0.2,leg="left",
          intitle="0.2 degree squares",mincount=2,namecatch="catch_kg")
```

```
## subdiv     87.057 8.7057 0.87057 0.087057 
## counthot   0 0 0 0 
## 494.8624     500 250 100 50 10 5 1 0.001 
## countpoly  0 2 6 6 11 4 16 31
```

```r
plotLand(incol="pink")
```

<img src="rforcpue_files/figure-html/sketchmap2-1.png" style="display: block; margin: auto;" />

__Figure 8.__ A sketch map of the the Lat Long data within the _sps_ data set with catches aggregated into 0.2 degree squares. By requiring at least 2 records in each square before inclusion some of the deeper water extraneous records have been eliminated (although not all). The red, green, and royal blue squares denote the areas generating the greatest yields.


Such sketch maps can be helpful, especially when plotting a single year's data to illustrate how the extent of a fishery varies through time. There are obvious limitations. There is no formal map projection, one merely alters the width and height of the plot until the visual representation of the land looks acceptable. In addition there are islands missing so as to limit the size of the underlying coastal definition data set (to see this try entering _head(cede:::aus,30)_ into the console).

One is not limited to plotting up catches, by altering which variable is identified in the parameter _namecatch_ it is possible to plot up other variables but this aspect is still under development. Currently catches are assumed to be reported as kg, and these are divided by 1000 to focus on tonnes of catch. Modifying this scaling of the polygons is the issue with plotting say effort, although sometimes with effort dividing by 1000 is not an issue. Future versions of __cede__ should address this issue.

## CPUE Standardization

### Introduction

If one were to search online for CPUE standardization it would quickly become apparent that this is a very large subject with many alternative approaches and strategies. Here I will introduce two approaches, the first uses General and Generalized Linear Models (LMs and GLMs) and the second uses Generalized Additive Models (GAMS). This will only be a brief introduction to the subject but the hope is that such an introduction would enable users to explore further and develop approaches best suited to their own fisheries. 

Commercial catch and effort (CPUE) data are used in very many fishery stock assessments in Australia as an index of relative abundance. Using CPUE in this way assumes there is a direct relationship between catch rates and exploitable biomass. However, many other factors can influence catch rates, including vessel, gear(fishing method), depth, season (month), area, and time of fishing (e.g. day or night). The use of CPUE as an index of relative abundance requires the removal of the effects of variation due to changes in factors other than stock biomass, on the assumption that what remains will provide a better estimate of the underlying biomass dynamics. That the outcome provides a better approximation to the stock biomass trends remains an assumption because there may well be other factors influencing CPUE for which no data are available and thus cannot be included in any standardization. However, at least variation in CPUE due to the factors that are included is accounted for. The process of adjusting the time series of CPUE for the effects of other factors is known as standardization and the accepted way of doing this is to use some statistical modelling procedure that focuses attention onto the annual average catch rates adjusted for the variation in the averages brought about by all the other factors identified. Idiosyncrasies between species and methods across Australia means that each fishery/stock for which standardized catch rates are required entails its own set of conditions and selection of data. 

### The Limits of Standardization

The use of commercial CPUE as an index of the relative abundance of exploitable biomass can be misleading when there are factors that significantly influence CPUE but cannot be accounted for in a statistical standardization analysis. Over the last few decades the management of many Australian fisheries have undergone significant changes. For example, in the Commonwealth fisheries there was the introduction of the quota management system into the SESSF in 1992, and the introduction of the Harvest Strategy Policy (HSP) and associated structural adjustment in 2005 - 2007. The combination of limited quotas and the HSP is now controlling catches in such a way that many fishers have been altering their fishing behaviour to take into account the availability of quota and their own access to quota needed to land the species taken in the mixed species SESSF. In other jurisdictions the introduction of such things as extensive marine protected areas have altered the fishing behaviour of commercial fishers and this too can effectively break CPUE time-series, which adds complexity to any analysis.


## Methods

### Initial Data Selection

Fisheries data is often noisy and can contain obvious errors and mistakes (e.g. an inshore species reportedly being caught in 6000 m of water). The data exploration mentioned earlier should allow one to defensibly select data for further analysis (i.e. defensibly remove implausible data points). Often such data selection is aimed at identifying records that represent typical activities in each fishery concerned. In particular some selection criteria are aimed at focussing on records where the species is being targeted. For example, most species have a depth range within which they are typically caught. Ideally, an agreed depth range should be used so that it becomes standard to select data records between some minimum and maximum depth range. A second example relates to one vessel in the SESSF catching a particular species by a particular gear having catch rates 10 - 20 times those of other vessels fishing in the same places at the same time. Further exploration indicated that the vessel concerned had misunderstood how to fill in the log book so their data was removed from subsequent analysis. Whatever decisions are made about any selection or filtering of data, each choice should be defensible and it should be possible to present the evidence for the selection made (e.g. illustrate extreme values, typical depth ranges, unusual vessels). The ultimate defense for making a data selection is to present the analysis with and without the selection to demonstrate any effect. 

Once a defensible set of data records have been selected there are other modifications needed. At its most basic a linear model is very similar to a regression analysis. If you imagine conducting a regression of Log(CPUE) against Year so as to evaluate how those catch rates have changed through time then all that would come out would be a single line having two parameters, an intercept and gradient. There are only two parameters because it would treat the factor 'Year' as a continuous variable. What we actually want is a separate index for each year, we need to treat the 'Year' factor as a categorical factor rather than as a continuous variable. Thus, for example, 1986 - 2016 is not a series of numbers between those years, but rather represents separate years. Below we will illustrate the use of using all categorical factors and then a different illustration showing how to include a continuous variable such as depth, into a standardization.

### Standardization

The use of _properties_ indicates that the _sps_ data set contains six clear factors: Year, Month, Vessel, Depth or DepCat, DayNight, and Zone. The Zone factor is a subdivision of mainly the Latitude factor although longitude is also in there to a lesser extent.

First we need to convert some of the factors into categorical factors using the eponymous function _makecategorical_. It is good practice not to over-write your original data.frame so here the _sps_ name is slightly modified to _sps1_.  





































