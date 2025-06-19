


#This is the results in R

best.glm3 <- glm.nb(formula = EKP.recruits.scaled ~ Year:Month:Area + 
                    StartTimeGroup + Depth.cat + MoonPhase,
                    data = ekp.dat3, link = log)


expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 50),
            sex = c("Male","Female"))


newdata = data.frame(Year = as.factor(rep(c(2006:2015,2017:2018),each =2)),
                     Month = as.factor(rep(11:12, 12)),
                     Area = as.factor(rep("Moreton Bay",24)),
                     StartTimeGroup = as.factor(rep("00:00",24)),
                     Depth.cat  = as.factor(rep(10,24)),
                     MoonPhase = as.factor(rep("Full moon",24))
)



predm10 <- predict(best.glm3,newdata,type = "response")

cbind(newdata,predm10)



> cbind(newdata,predm10)
Year Month   predm10
1  2006    11 112.57019
2  2006    12  21.23830
3  2007    11 331.30503
4  2007    12 106.58937
5  2008    11 258.87554
6  2008    12 173.12720
7  2009    11  91.74123
8  2009    12 209.19469
9  2010    11 216.47768
10 2010    12 142.40373
11 2011    11 176.18676
12 2011    12 169.70842
13 2012    11 242.87037
14 2012    12 242.20919
15 2013    11 213.92574
16 2013    12 270.00779
17 2014    11 130.33357
18 2014    12 195.09055
19 2015    11 176.06911
20 2015    12 180.64009
21 2017    11 188.67891
22 2017    12 140.29556
23 2018    11 198.26754
24 2018    12  21.23830