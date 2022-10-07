###.................................................................###
###    von Bertalanffy modelling
###    SPA 3
### Rehauled to prep to be moved to github by J.Sameoto July 2021
###.................................................................###

#NB: depth is not used in the von B modelling
###NB!! using same aging/vonB as per the 2016 assessment; will add new data and run new model when go to next RAP 

#required packages
library(lme4)
library(nlme)
library(tidyverse)
library(ROracle)

# Define: 
uid <- un.sameotoj
pwd <- pw.sameotoj

uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

surveyyear <- 2022  #This is the last survey year for which you want to include  - not should match year of cruise below 
cruise <- "BI2022"  #note should match year for surveyyear set above 

assessmentyear <- 2022 #year in which you are conducting the survey 
area <- "3"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
path.directory <- "Y:/Inshore/BoF/"


# ---- Obtain data for VonB modelling ---- 
#strata.spa3<-c(22:24)

#SQL query 1: detailed meat weight/shell height sampling data
quer1 <- paste0("SELECT * FROM scallsur.scwgthgt WHERE strata_id IN (22,23,24)")

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# Select data from database; execute query with ROracle
#detailed sampling data
BIdetail.vonB <- dbGetQuery(chan, quer1)

#add YEAR column to data
BIdetail.vonB$YEAR <- as.numeric(substr(BIdetail.vonB$CRUISE,3,6))
names(BIdetail.vonB)[2] <-"TOW.NO" #rename TOW_NO column; lme4 doesn't like underscores in column names

#Check years that have aging data: 
BIdetail.vonB %>% filter(!is.na(AGE)) %>% distinct(YEAR)
#Since 2015 RAP, only use aging data up to 2015. We'll use newer data when we go to the next RAP process. 
BIdetail.vonB <- BIdetail.vonB %>% filter(!is.na(AGE)) %>% filter(YEAR <= 2015)


####
# check detail file for NAs (WET_MEAT_WEIGHT and HEIGHT) and positive depths
####
summary(BIdetail.vonB)
BIdetail.vonB <-BIdetail.vonB[complete.cases(BIdetail.vonB[,c(grep("HEIGHT", colnames(BIdetail.vonB)))]),]   #remove NAs from HEIGHT
BIdetail.vonB <- BIdetail.vonB[complete.cases(BIdetail.vonB[,c(grep("AGE", colnames(BIdetail.vonB)))]),]  #remove NAs from AGE
summary(BIdetail.vonB)

#Year must be a factor for modelling 
BIdetail.vonB$Year.fac <- as.factor(BIdetail.vonB$YEAR)
summary(BIdetail.vonB)
str(BIdetail.vonB)
levels(BIdetail.vonB$Year.fac)

# ---- Von B with random effect ONLY on Linf and logk (NO random effect on T0) ----
# i.e. random=~Linf+logK~1 (without T0)  
## THIS MODEL USED IN 2016 and used for subsequent UPDATES
begTime <- Sys.time()
VONBIYYYY.nlme <- nlme(HEIGHT~SSasympOff(AGE,Linf,logK,T0), 
                      random=~Linf+logK~1|Year.fac/TOW.NO,
                      fixed=list(Linf~1,logK~1,T0~1), 
                      data=BIdetail.vonB,start=c(144,-1.7738,-1.0),
                      method="REML", na.action = na.omit)
runTime <- Sys.time()-begTime
runTime #Time difference of 28.7948 secs

summary(VONBIYYYY.nlme)

# Output
#Nonlinear mixed-effects model fit by REML
#Model: HEIGHT ~ SSasympOff(AGE, Linf, logK, T0) 
#Data: test.data 
#AIC      BIC    logLik
#155762.7 155844.4 -77871.33

#Random effects:
#  Formula: list(Linf ~ 1, logK ~ 1)
#Level: Year.fac
#Structure: General positive-definite, Log-Cholesky parametrization
#StdDev    Corr  
#Linf 5.1040070 Linf  
#logK 0.1071534 -0.833

#Formula: list(Linf ~ 1, logK ~ 1)
#Level: TOW.NO %in% Year.fac
#Structure: General positive-definite, Log-Cholesky parametrization
#StdDev     Corr  
#Linf     14.9696474 Linf  
#logK      0.1998786 -0.921
#Residual  4.3523329       

#Fixed effects: list(Linf ~ 1, logK ~ 1, T0 ~ 1) 
#Value Std.Error    DF   t-value p-value
#Linf 149.95206 1.5737867 25563  95.28106       0
#logK  -1.64984 0.0313600 25563 -52.60964       0
#T0    -0.38157 0.0107922 25563 -35.35575       0
#Correlation: 
#  Linf   logK  
#logK -0.837       
#T0   -0.093  0.126

#Standardized Within-Group Residuals:
#  Min           Q1          Med           Q3          Max 
#-5.555700775 -0.585909618 -0.007796025  0.574913080  6.867735656 

#Number of Observations: 26255
#Number of Groups: 
#  Year.fac TOW.NO %in% Year.fac 
#   13                  690 



# ---- Von B with random effect on T0 ----
## Note this model formulation often does not converge and cannot be used but this is left as a placeholder so you know or could try yourself 
#
#begTime <- Sys.time()
#VONBIYYYY.nlme.T0random <-nlme(HEIGHT~SSasympOff(AGE,Linf,logK,T0), 
#                       random=~Linf+logK+T0~1|Year.fac/TOW.NO,
#                      fixed=list(Linf~1,logK~1,T0~1), 
#                      data=test.data,start=c(144,-1.7738,-1.0),
#                      method="REML", na.action = na.omit)
#runTime <- Sys.time()-begTime
#runTime #Time difference of   4.43478 hours "maximum number of iterations reached without convergence"
#summary(VONBIYYYY.nlme.T0random)



# ---- PLOT FOR VONB ----
VONBIYYYY.nlme.CI <- intervals(VONBIYYYY.nlme)
VONBIYYYY.nlme.CI
#Approximate 95% confidence intervals
#Fixed effects:
#  lower        est.       upper
#Linf 146.867352 149.9520637 153.0367749
#logK  -1.711307  -1.6498391  -1.5883717
#T0    -0.402720  -0.3815667  -0.3604134
VONBIYYYY.nlme.CI$fixed
VONBIYYYY.nlme.CI$sigma
VONBIYYYY.nlme.CI$reStruct


# Random effects of Von B model parameters plot
test <- ranef(VONBIYYYY.nlme)
test.plot.Year <- data.frame(Parms=c(test[[1]]$Linf,test[[1]]$logK),
                             Pnames=rep(c("Linf","logK"),each=length(unique(BIdetail.vonB$YEAR))),
                             Year=rep(unique(BIdetail.vonB$YEAR),2)) #note the 2 here is bc there's 2 random parameters ("Linf","logK")

ran.eff.vonB.parameters <- ggplot(data=test.plot.Year, aes(x=Year, y=Parms)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~Pnames) + theme_bw() + 
  ylab("Random effect")
ran.eff.vonB.parameters

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/ran.eff.vonB.parameters.",area,".png"),width=11,height=8,units = "in",res=920)
ran.eff.vonB.parameters
dev.off()

#pull out year by tow random effects so can look at patterns with residuals by tow 
rand.effect.year.tow <- test$TOW.NO
rand.effect.year.tow$year.tow.no <- rownames(rand.effect.year.tow)
#need field to match on to data - so can look at tow attributes e.g. depth 
BIdetail.vonB$year.tow.no <- paste0(BIdetail.vonB$YEAR,"/",BIdetail.vonB$TOW.NO)
tow.attributes <- BIdetail.vonB %>% select(year.tow.no, DEPTH, BOTTOM_TEMP, YEAR)
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

#plot T0 inf by depth - is no T0 since couldn't get model with random T0 to converge 
#ggplot(data=rand.effect.year.tow.attributes, aes(x=BOTTOM_TEMP, y=T0)) + 
#  geom_point() + 
#  facet_wrap(~YEAR) +
#  geom_smooth() + theme_bw() + 
#  ylab("Random effect")

#######

#Predictions for plots: 
row.names(test$Year.fac)
row.names(test$TOW.NO)
length(row.names(test$TOW.NO))

#predict.age <- data.frame(AGE = rep(seq(from = 0, to = 20, by = 1),length(row.names(test$TOW.NO))), TOW.NO=row.names(test$TOW.NO), Year.fac=c(rep("1996",21),rep("1997",21)))

# The tows object would be a list of all tows in the format of 1996/1
#pred <- expand.grid(age = 0:20, TOW_NO = tows)
#pred$year <- substr(pred$TOW_NO,1,4)

predict.age <- expand.grid(AGE =0:20 , Year.fac = 1996:2015,TOW.NO = 1:1000)

#predict.age <- data.frame(AGE = rep(seq(from = 0, to = 20, by = 1),2), TOW.NO=1, Year.fac=c(rep("1996",21),rep("1997",21)))
p.curve <- predict(VONBIYYYY.nlme, predict.age, level=0:2)
head(p.curve)
tail(p.curve)
p.curve <- cbind(p.curve, (predict.age %>% select(AGE)))  #Note this merge assumes it's all in the right order

#only need one year of predict.fixed for plot since all fixed effects predictions are the same 
p.curve.fixed <- p.curve %>% filter(Year.fac=="1996")
p.curve.fixed.Year.fac <- unique(p.curve %>% select(Year.fac, predict.Year.fac, AGE))

#CIs: 
upper <- VONBIYYYY.nlme.CI$fixed[7]* (1 - exp(-exp(VONBIYYYY.nlme.CI$fixed[8]) * (predict.age$AGE  - VONBIYYYY.nlme.CI$fixed[3]))) 
lwr <- VONBIYYYY.nlme.CI$fixed[1]* (1 - exp(-exp(VONBIYYYY.nlme.CI$fixed[2]) * (predict.age$AGE  - VONBIYYYY.nlme.CI$fixed[9] )))

#Prediction for plotting with CIs 
p.curve.fixed <- cbind(p.curve.fixed, age=predict.age$AGE, upper, lwr)

#Keep only those year/tow.no combinations that actually exist in the data; but this messes up relative length of objects to plot so not used, only change is that plots would render faster 
#p.curve <- p.curve %>% dplyr::filter(TOW.NO %in% row.names(test$TOW.NO))

#PLOT VonB; note main prediction are the same at level 0; fixed effect only 
#note this plot could use slightly bigger axis labels
vonB.BI.95CI <- ggplot() + 
  #geom_point(data=BIdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.3) +
  geom_jitter(data=BIdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.1) + 
  geom_line(data=p.curve.fixed, aes(AGE, predict.fixed), colour="blue", lwd=2, show.legend = F) +
  geom_ribbon(data=p.curve.fixed,aes(ymin=lwr, ymax=upper, x=AGE), alpha=0.6, fill="blue") + 
  theme(panel.grid = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14)) + 
  xlab("Age (years)")+ylab("Shell height (mm)") + theme_bw()
vonB.BI.95CI

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/VonB_SPA",area,".png"),width=11,height=8,units = "in",res=920)
vonB.BI.95CI
dev.off()

#Random effect of year
vonB.BI.year.raneff <- ggplot() + 
  #geom_point(data=BIdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.3) +
  #geom_jitter(data=BIdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.1) + 
  #geom_line(data=p.curve.fixed, aes(AGE, predict.fixed), colour="blue", lwd=2, show.legend = F) +
  geom_ribbon(data=p.curve.fixed,aes(ymin=lwr, ymax=upper, x=AGE), alpha=0.6, fill="blue") + 
  theme(panel.grid = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14)) + 
  xlab("Age (years)")+ylab("Shell height (mm)") + theme_bw() + 
  geom_line(data=p.curve.fixed.Year.fac, aes(x=AGE, y=predict.Year.fac, group=Year.fac))
vonB.BI.year.raneff

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/VonB_SPA_year_raneff_",area,".png"),width=11,height=8,units = "in",res=920)
vonB.BI.year.raneff
dev.off()

#Random effect of tow within year
vonB.BI.tow.in.year.raneff <- ggplot() + 
  #geom_point(data=BIdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.3) +
  #geom_jitter(data=BIdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.1) + 
  #geom_line(data=p.curve.fixed, aes(age, predict.fixed), colour="blue", lwd=2, show.legend = F) +
  #geom_ribbon(data=p.curve.fixed,aes(ymin=lwr, ymax=upper, x=age), alpha=0.6, fill="blue") + 
  theme(panel.grid = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14)) + 
  xlab("Age (years)")+ylab("Shell height (mm)") + theme_bw() + 
  geom_line(data=p.curve, aes(x=AGE, y=predict.TOW.NO, group=TOW.NO))
vonB.BI.tow.in.year.raneff

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/VonB_SPA_year.tow_raneff_",area,".png"),width=11,height=8,units = "in",res=920)
vonB.BI.tow.in.year.raneff
dev.off()


#!!!Now save vonB model output as .RData object: e.g. BIvonB2021.RData
save(VONBIYYYY.nlme, file = paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/SPA",area,"/BIvonB",surveyyear,".RData"))

### END #### 
