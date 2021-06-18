###........................................###
###
###    von Bertalanffy modelling
###    BoF: SPA1A,1B,4,and 5
###    L.Nasmith
###    September 2016
###    Modified Sept 2017 - J.SAmeoto
###
###........................................###

#NB: depth is not used in the von B modelling

#NOTE: NO AGING CONDUCTED IN 2017 - no new aging data - SAME VONB USED FOR 2017 as per 2016


#setwd


#required packages
library (lme4)
library(nlme)
library(RODBC)
library(lattice)

#############
# read in shell height and meat weight data from database
############

#strata.spa1a<-c(6,7,12:20,39)
#strata.spa1b<-c(35,37,38,49,51:53)
#strata.spa4<-c(1:5, 8:10)
#strata.spa5==21

#RODBCconn <-odbcConnect("PTRAN", uid=username, pwd=password)
RODBCconn <-odbcConnect("PTRAN", uid=un.sameotoj, pwd=pw.sameotoj) #!!!!UPDATE

#detailed meat weight/shell height sampling data
BFdetail.vonB<-sqlQuery(RODBCconn,
"SELECT *
FROM scallsur.scwgthgt
WHERE strata_id IN (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 35, 37, 38, 39, 49, 51, 52, 53)")


#add YEAR column to data
BFdetail.vonB$YEAR <- as.numeric(substr(BFdetail.vonB$CRUISE,3,6))
names(BFdetail.vonB)[2] <-"TOW.NO" #rename TOW_NO column; lme4 doesn't like underscores in column names

#############
# check detail file for NAs (AGE and HEIGHT)
############
summary(BFdetail.vonB)
BFdetail.vonB<-BFdetail.vonB[complete.cases(BFdetail.vonB[,c(13)]),]  #remove NAs from HEIGHT
BFdetail.vonB<-BFdetail.vonB[complete.cases(BFdetail.vonB[,c(14)]),]  #remove NAs from AGE
summary(BFdetail.vonB)

#############################
#### model all years (tow nested within year)

#create dataset for model
test.data<-BFdetail.vonB
test.data$Year.fac<-as.factor(test.data$YEAR)
#summary(test.data)
str(test.data)
### using nlme
library(nlme)

begTime <- Sys.time()

VONBF2016.nlme<-nlme(HEIGHT~SSasympOff(AGE,Linf,logK,T0), random=~Linf+logK+T0~1|Year.fac/TOW.NO,
              fixed=list(Linf~1,logK~1,T0~1), data=test.data,start=c(144,-1.7738,-1.0),method="REML")


runTime <- Sys.time()-begTime
runTime #just for curiosity:   18.39 mins

summary (VONBF2016.nlme)

> summary (VONBF2016.nlme)
Nonlinear mixed-effects model fit by REML
Model: HEIGHT ~ SSasympOff(AGE, Linf, logK, T0)
Data: test.data
AIC    BIC  logLik
460186 460334 -230077

Random effects:
  Formula: list(Linf ~ 1, logK ~ 1, T0 ~ 1)
Level: Year.fac
Structure: General positive-definite, Log-Cholesky parametrization
StdDev   Corr
Linf 16.84983 Linf   logK
logK  0.33943 -0.947
T0    0.74212 -0.779  0.922

Formula: list(Linf ~ 1, logK ~ 1, T0 ~ 1)
Level: TOW.NO %in% Year.fac
Structure: General positive-definite, Log-Cholesky parametrization
StdDev  Corr
Linf     9.97556 Linf   logK
logK     0.16864 -0.723
T0       0.38894 -0.285  0.803
Residual 3.99479

Fixed effects: list(Linf ~ 1, logK ~ 1, T0 ~ 1)
Value Std.Error    DF t-value p-value
Linf 148.197    3.7847 77980  39.156  0.0000
logK  -1.576    0.0762 77980 -20.698  0.0000
T0    -0.366    0.1665 77980  -2.195  0.0281
Correlation:
  Linf   logK
logK -0.946
T0   -0.776  0.921

Standardized Within-Group Residuals:
  Min         Q1        Med         Q3        Max
-8.6933965 -0.6085053  0.0087226  0.6349398  9.6892768

Number of Observations: 79915
Number of Groups:
  Year.fac TOW.NO %in% Year.fac
20                 1933

##########################################################################################
##
## Summary from 2015 asessment (using data upto & including 2014)
##
##########################################################################################


summary(VONBF2015.nlme)


> summary(VONBF2015.nlme)
Nonlinear mixed-effects model fit by REML
  Model: HEIGHT ~ SSasympOff(AGE, Linf, logK, T0)
 Data: test.data
       AIC      BIC    logLik
  440672.8 440820.7 -220320.4

Random effects:
 Formula: list(Linf ~ 1, logK ~ 1, T0 ~ 1)
 Level: Year.fac
 Structure: General positive-definite, Log-Cholesky parametrization
     StdDev    Corr
Linf 9.0351384 Linf   logK
logK 0.2315316 -0.895
T0   0.6529497 -0.770  0.947

 Formula: list(Linf ~ 1, logK ~ 1, T0 ~ 1)
 Level: TOW.NO %in% Year.fac
 Structure: General positive-definite, Log-Cholesky parametrization
         StdDev    Corr
Linf     9.6498043 Linf   logK
logK     0.1686844 -0.719
T0       0.3890608 -0.323  0.828
Residual 4.0197190

Fixed effects: list(Linf ~ 1, logK ~ 1, T0 ~ 1)
         Value Std.Error    DF   t-value p-value
Linf 144.75105 2.1004421 74564  68.91456  0.0000
logK  -1.51602 0.0534899 74564 -28.34215  0.0000
T0    -0.27624 0.1504672 74564  -1.83587  0.0664
 Correlation:
     Linf   logK
logK -0.892
T0   -0.763  0.946

Standardized Within-Group Residuals:
         Min           Q1          Med           Q3          Max
-8.633926861 -0.607817765  0.009452056  0.634607818  9.629364222

Number of Observations: 76402
Number of Groups:
            Year.fac TOW.NO %in% Year.fac
                  19                 1836
###################

test<-ranef(VONBF2015.nlme)

test.plot.Year<-data.frame(Parms=c(test[[1]]$Linf,test[[1]]$logK,test[[1]]$T0),
                          Pnames=rep(c("Linf","logK","T0"),each=19),
                           Year=rep(1996:2014,3))
windows()
xyplot(Parms~Year|Pnames,data=test.plot.Year,type="b")


##############################################################################################################################################  PLOT FOR VONB

#code from J. Sameoto
#dat <- read.csv('E:/Documents and Settings/Inshore Scallop/BoF Assessment/2015/SPA6/dataoutput/GMdetail.csv')
dat <- BFdetail.vonB
dat.age <- aggregate(dat$HEIGHT, by=list(dat$YEAR, dat$AGE), FUN=mean)
names(dat.age) <- c("YEAR","AGE","HEIGHT")
dat.age <- dat.age[with(dat.age, order(dat.age$YEAR, dat.age$AGE)), ]


pred.dat <- seq(1,18,by=1)
#out <- 154.69*(1-exp((-exp(-1.78))*(pred.dat +0.95))) #These are GM values
out <-  144.75*(1-exp((-exp(-1.52))*(pred.dat + (0.28))))  #BoF values

windows()
plot(HEIGHT~AGE, data=subset(dat.age,YEAR==2005),pch=1 , ylab="Shell height (mm)", xlab="Age (Years)",ylim=c(0,175), xlim=c(0,20))
points(HEIGHT~AGE, data=subset(dat.age,YEAR==2006), pch=2)
points(HEIGHT~AGE, data=subset(dat.age,YEAR==2007), pch=3)
points(HEIGHT~AGE, data=subset(dat.age,YEAR==2008), pch=4)
points(HEIGHT~AGE, data=subset(dat.age,YEAR==2009), pch=5)
points(HEIGHT~AGE, data=subset(dat.age,YEAR==2010), pch=6)
points(HEIGHT~AGE, data=subset(dat.age,YEAR==2011), pch=7)
points(HEIGHT~AGE, data=subset(dat.age,YEAR==2012), pch=8)
points(HEIGHT~AGE, data=subset(dat.age,YEAR==2013), pch=9)
points(HEIGHT~AGE, data=subset(dat.age,YEAR==2014), pch=10)
lines(out~pred.dat, col='blue', lwd=2)
leg.txt <- 2005:2014
legend("topleft",pch=c(1:10),title="Year",legend = leg.txt)

