###........................................###
###
###    von Bertalanffy modelling
###    SFa 29W
###    J.Sameoto, April 2021
###    Significantly updated Oct 2021 - JS
###
###........................................###

#NB: depth is not used in the von B modelling

#required packages
library(lme4)
library(nlme)
library(tidyverse)
library(ggplot2)
library(lattice)
library(ROracle)

uid <- un.sameotoj
pwd <- pw.sameotoj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)
#yr <- 2020 

surveyyear <- 2022  #This is the last survey year for which you want to include  - not should match year of cruise below 
#cruise <- "BI2021"  #note should match year for surveyyear set above 

assessmentyear <- 2023 #year in which you are conducting the survey 
#area <- "1A1B4and5"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
path.directory <- "Y:/Inshore/SFA29/"


## ---- Obtain data for VonB modelling ---- 
# read in shell height and meat weight data from database
#

# Get the cruises
cruise.list <- paste0("SFA29",(2001:(surveyyear-1)))
cruise.list <- paste(cruise.list,collapse="','")

# SQL
quer1 <- paste(
  "SELECT *                                     ",
  "FROM scallsur.scwgthgt                      ",
  "WHERE strata_id IN (41, 42, 43, 44, 45)      ",
  "AND (cruise in ('",cruise.list,"'))          ",
  sep=""
)

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

#detailed meat weight/shell height sampling data w/ age 
SFA29detail.vonB <- dbGetQuery(chan, quer1)

#add YEAR column to data
SFA29detail.vonB$YEAR <- as.numeric(substr(SFA29detail.vonB$CRUISE,6,9))
names(SFA29detail.vonB)[c(grep("TOW_NO",names(SFA29detail.vonB)))] <-"TOW.NO" #rename TOW_NO column; lme4 doesn't like underscores in column names

####
# check detail file for NAs (AGE and HEIGHT)
###
summary(SFA29detail.vonB)
SFA29detail.vonB <- SFA29detail.vonB[complete.cases(SFA29detail.vonB[,c("HEIGHT")]),]  #remove NAs from HEIGHT
SFA29detail.vonB <- SFA29detail.vonB[complete.cases(SFA29detail.vonB[,c("AGE")]),]  #remove NAs from AGE
summary(SFA29detail.vonB)


#create dataset for model
test.data <- SFA29detail.vonB
test.data$Year.fac <- as.factor(test.data$YEAR)
summary(test.data)
str(test.data)

table(test.data$YEAR)
#2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 
# 838 1204 1184  717  573 1130 1035  928  453  744  673 1195 1236 1057 2616

## ---- Von B with random effect  on Linf AND logk AND T0 ----
#### model all years (tow nested within year)
begTime <- Sys.time()

VON.29W.nlme <- nlme(HEIGHT~SSasympOff(AGE,Linf,logK,T0), random=~Linf+logK+T0~1|Year.fac/TOW.NO,
              fixed=list(Linf~1,logK~1,T0~1), data=test.data,start=c(144,-1.7738,-1.0),method="REML")


runTime <- Sys.time()-begTime
runTime 

summary(VON.29W.nlme)

#> summary(VON.29W.nlme)
#
#Fixed effects: list(Linf ~ 1, logK ~ 1, T0 ~ 1) 
#Value Std.Error    DF   t-value p-value
#Linf 153.32424 2.4834805 15056  61.73764       0
#logK  -1.59372 0.0654613 15056 -24.34604       0
#T0    -0.65285 0.1360956 15056  -4.79700       0
#Correlation: 
#  Linf   logK  
#logK -0.932       
#T0   -0.909  0.964

##

#---- PLOTS FOR VONB ----

VON29YYYY.nlme.CI <- intervals(VON.29W.nlme)
VON29YYYY.nlme.CI
#Approximate 95% confidence intervals

#Fixed effects:
#        lower        est.       upper
#Linf 148.4563136 153.3242373 158.1921610
#logK  -1.7220353  -1.5937232  -1.4654111
#T0    -0.9196152  -0.6528512  -0.3860872


#random effects by year 
test <- ranef(VON.29W.nlme)

test.plot.Year <- data.frame(Parms=c(test[[1]]$Linf,test[[1]]$logK,test[[1]]$T0),
           Pnames=rep(c("Linf","logK","T0"),each=15),
           Year=rep(2001:2015,3))

ran.eff.vonB.parameters <- ggplot(data=test.plot.Year, aes(x=Year, y=Parms)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~Pnames) + theme_bw() + 
  ylab("Random effect")
ran.eff.vonB.parameters

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/ran.eff.vonB.parameters.29W.png"),width=11,height=8,units = "in",res=920)
ran.eff.vonB.parameters
dev.off()


#pull out year by tow random effects so can look at patterns with residuals by tow 
rand.effect.year.tow <- test$TOW.NO
rand.effect.year.tow$year.tow.no <- rownames(rand.effect.year.tow)
#need field to match on to data - so can look at tow attributes e.g. depth 
SFA29detail.vonB$year.tow.no <- paste0(SFA29detail.vonB$YEAR,"/",SFA29detail.vonB$TOW.NO)
tow.attributes <- SFA29detail.vonB %>% select(year.tow.no, DEPTH, BOTTOM_TEMP, YEAR)
rand.effect.year.tow.attributes <- merge(rand.effect.year.tow,tow.attributes,by=c("year.tow.no") )
#Above is just example, obviously depth used here is not good since it's not tide corrected, but this is just an example, and plots below, just example of what could try and do with vonB random effect so see if potential explanatory environmental variables in a future study; note depth/temperature, and fishing pressure likely all confounded 

#plot L inf by depth or bottom temp, etc 
ggplot(data=rand.effect.year.tow.attributes, aes(x=BOTTOM_TEMP, y=Linf)) + 
  geom_point() + 
  facet_wrap(~YEAR) +
  geom_smooth() + theme_bw() + 
  ylab("Random effect")

#plot logK inf by observed temp when tow conducted (e.g. possible proxy for oceanography differences ) 
ggplot(data=rand.effect.year.tow.attributes, aes(x=BOTTOM_TEMP, y=logK)) + 
  geom_point() + 
  facet_wrap(~YEAR) +
  geom_smooth() + theme_bw() + 
  ylab("Random effect")

#plot T0 inf by depth 
ggplot(data=rand.effect.year.tow.attributes, aes(x=BOTTOM_TEMP, y=T0)) + 
  geom_point() + 
  facet_wrap(~YEAR) +
  geom_smooth() + theme_bw() + 
  ylab("Random effect")

#Predictions for plots: 
row.names(test$Year.fac)
row.names(test$TOW.NO)
length(row.names(test$TOW.NO))

#predict.age <- data.frame(AGE = rep(seq(from = 0, to = 20, by = 1),length(row.names(test$TOW.NO))), TOW.NO=row.names(test$TOW.NO), Year.fac=c(rep("1996",21),rep("1997",21)))

# The tows object would be a list of all tows in the format of 1996/1
#pred <- expand.grid(age = 0:20, TOW_NO = tows)
#pred$year <- substr(pred$TOW_NO,1,4)

predict.age <- expand.grid(AGE =0:20 , Year.fac = 2001:2015,TOW.NO = 1:1000)

#predict.age <- data.frame(AGE = rep(seq(from = 0, to = 20, by = 1),2), TOW.NO=1, Year.fac=c(rep("1996",21),rep("1997",21)))
p.curve <- predict(VON.29W.nlme, predict.age, level=0:2)
head(p.curve)
tail(p.curve)
p.curve <- cbind(p.curve, (predict.age %>% select(AGE)))  #Note this merge assumes it's all in the right order

#only need one year of predict.fixed for plot since all fixed effects predictions are the same 
p.curve.fixed <- p.curve %>% filter(Year.fac=="2001")
p.curve.fixed.Year.fac <- unique(p.curve %>% select(Year.fac, predict.Year.fac, AGE))

#CIs: 
upper <- VON29YYYY.nlme.CI$fixed[7]* (1 - exp(-exp(VON29YYYY.nlme.CI$fixed[8]) * (predict.age$AGE  - VON29YYYY.nlme.CI$fixed[3]))) 
lwr <- VON29YYYY.nlme.CI$fixed[1]* (1 - exp(-exp(VON29YYYY.nlme.CI$fixed[2]) * (predict.age$AGE  - VON29YYYY.nlme.CI$fixed[9] )))

#Prediction for plotting with CIs 
p.curve.fixed <- cbind(p.curve.fixed, age=predict.age$AGE, upper, lwr)

#Keep only those year/tow.no combinations that actually exist in the data; but this messes up relative length of objects to plot so not used, only change is that plots would render faster 
#p.curve <- p.curve %>% dplyr::filter(TOW.NO %in% row.names(test$TOW.NO))

#PLOT VonB; note main prediction are the same at level 0; fixed effect only 
#note this plot could use slightly bigger axis labels
vonB.29.95CI <- ggplot() + 
  #geom_point(data=BFdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.3) +
  geom_jitter(data=SFA29detail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.1) + 
  geom_line(data=p.curve.fixed, aes(AGE, predict.fixed), colour="blue", lwd=2, show.legend = F) +
  geom_ribbon(data=p.curve.fixed,aes(ymin=lwr, ymax=upper, x=AGE), alpha=0.6, fill="blue") + 
  theme(panel.grid = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14)) + 
  xlab("Age (years)")+ylab("Shell height (mm)") + theme_bw()
vonB.29.95CI

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/VonB.SFA29W.png"),width=11,height=8,units = "in",res=920)
vonB.29.95CI
dev.off()

#Random effect of year
vonB.29.year.raneff <- ggplot() + 
  #geom_point(data=BFdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.3) +
  #geom_jitter(data=BFdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.1) + 
  #geom_line(data=p.curve.fixed, aes(AGE, predict.fixed), colour="blue", lwd=2, show.legend = F) +
  geom_ribbon(data=p.curve.fixed,aes(ymin=lwr, ymax=upper, x=AGE), alpha=0.6, fill="blue") + 
  theme(panel.grid = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14)) + 
  xlab("Age (years)")+ylab("Shell height (mm)") + theme_bw() + 
  geom_line(data=p.curve.fixed.Year.fac, aes(x=AGE, y=predict.Year.fac, group=Year.fac))
vonB.29.year.raneff

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/VonB.year.raneff.SFA29W.png"),width=11,height=8,units = "in",res=920)
vonB.29.year.raneff
dev.off()

#Random effect of tow within year
vonB.29.tow.in.year.raneff <- ggplot() + 
  #geom_point(data=BIdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.3) +
  #geom_jitter(data=BFdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.1) + 
  #geom_line(data=p.curve.fixed, aes(age, predict.fixed), colour="blue", lwd=2, show.legend = F) +
  #geom_ribbon(data=p.curve.fixed,aes(ymin=lwr, ymax=upper, x=age), alpha=0.6, fill="blue") + 
  theme(panel.grid = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14)) + 
  xlab("Age (years)")+ylab("Shell height (mm)") + theme_bw() + 
  geom_line(data=p.curve, aes(x=AGE, y=predict.TOW.NO, group=TOW.NO))

vonB.29.tow.in.year.raneff

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/VonB.year.tow.raneff.29W.png"),width=11,height=8,units = "in",res=920)
vonB.29.tow.in.year.raneff
dev.off()


#!!!Now save vonB model output as .RData object: e.g. BIvonB2021.RData
save(SFA29detail.vonB, VON.29W.nlme, file = paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/SFA29W.vonB.",surveyyear,".RData"))

### END #### 


####
##
## ----- Summary from 2015 asessment (using data upto & including 2014) -----
##
####

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
