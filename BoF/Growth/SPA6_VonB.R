###.................................................................###
###    von Bertalanffy modelling
###    SPA 6
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
#uid <- keyring::key_list("Oracle")[1,2]
#pwd <- keyring::key_get("Oracle", uid)


surveyyear <- 2024  #This is the last survey year for which you want to include  - not should match year of cruise below 
cruise <- "GM2024"  #note should match year for surveyyear set above 

assessmentyear <- 2024 #year in which you are conducting the survey 
area <- "6"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
path.directory <- "Y:/Inshore/BoF/"


# ---- Obtain data for VonB modelling ---- 

#SQL query 1: detailed meat weight/shell height sampling data
quer1 <- paste0("SELECT * FROM scallsur.scwgthgt WHERE strata_id IN (30,31,32)")

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# Select data from database; execute query with ROracle
GMdetail.vonB <- dbGetQuery(chan, quer1)

#add YEAR column to data
GMdetail.vonB$YEAR <- as.numeric(substr(GMdetail.vonB$CRUISE,3,6))
names(GMdetail.vonB)[2] <-"TOW.NO" #rename TOW_NO column; lme4 doesn't like underscores in column names

#Check years that have aging data: 
GMdetail.vonB %>% filter(!is.na(AGE)) %>% distinct(YEAR)
#Since 2015 RAP, only use aging data up to 2015. We'll use newer data when we go to the next RAP process. 
GMdetail.vonB <- GMdetail.vonB %>% filter(!is.na(AGE)) %>% filter(YEAR <= 2015)


####
# check detail file for NAs (WET_MEAT_WEIGHT and HEIGHT) and positive depths
####
summary(GMdetail.vonB)
GMdetail.vonB <- GMdetail.vonB[complete.cases(GMdetail.vonB[,c(grep("HEIGHT", colnames(GMdetail.vonB)))]),]  #remove NAs from HEIGHT
GMdetail.vonB <- GMdetail.vonB[complete.cases(GMdetail.vonB[,c(grep("AGE", colnames(GMdetail.vonB)))]),]  #remove NAs from AGE
summary(GMdetail.vonB)

#Year must be a factor for modelling 
GMdetail.vonB$Year.fac <- as.factor(GMdetail.vonB$YEAR)
summary(GMdetail.vonB)
str(GMdetail.vonB)
levels(GMdetail.vonB$Year.fac)


# ---- Von B with random effect ONLY on Linf and logk (NO random effect on T0) ----
# i.e. random=~Linf+logK~1 (without T0)  
## THIS MODEL USED IN 2016 and used for subsequent UPDATES

begTime <- Sys.time()
VONGMYYYY.nlme <- nlme(HEIGHT~SSasympOff(AGE,Linf,logK,T0),
                       random=~Linf+logK+T0~1|Year.fac/TOW.NO,
              fixed=list(Linf~1,logK~1,T0~1), 
              data=GMdetail.vonB, start=c(144,-1.7738,-1.0),
              method="REML", na.action = na.omit)
runTime <- Sys.time()-begTime
#runTime Time difference of 14.23638 mins
summary(VONGMYYYY.nlme)

# Output
#> summary(VONGMYYYY.nlme)
#Nonlinear mixed-effects model fit by REML
#Model: HEIGHT ~ SSasympOff(AGE, Linf, logK, T0)
#Data: test.data
#AIC      BIC    logLik
#70733.58 70852.78 -35350.79

#Random effects:
#  Formula: list(Linf ~ 1, logK ~ 1, T0 ~ 1)
#Level: Year.fac
#Structure: General positive-definite, Log-Cholesky parametrization
#StdDev     Corr
#Linf 12.5942539 Linf   logK
#logK  0.3051101 -0.981
#T0    0.7156097 -0.891  0.959

#Formula: list(Linf ~ 1, logK ~ 1, T0 ~ 1)
#Level: TOW.NO %in% Year.fac
#Structure: General positive-definite, Log-Cholesky parametrization
#StdDev     Corr
#Linf     10.1094578 Linf   logK
#logK      0.1644722 -0.758
#T0        0.4693008 -0.335  0.796
#Residual  3.5945495

#Fixed effects: list(Linf ~ 1, logK ~ 1, T0 ~ 1)
#Value Std.Error    DF   t-value p-value
#Linf 157.42307  3.941508 12377  39.93981       0
#logK  -1.83441  0.094534 12377 -19.40484       0
#T0    -1.11288  0.222574 12377  -5.00004       0
#Correlation:
#  Linf   logK
#logK -0.975
#T0   -0.877  0.955

#Standardized Within-Group Residuals:
#  Min          Q1         Med          Q3         Max
#-4.39048093 -0.63215569  0.01155903  0.65775225  5.66833569

#Number of Observations: 12710
#Number of Groups:
#  Year.fac TOW.NO %in% Year.fac
#    11                  331



# ---- PLOT FOR VONB ----
VONGMYYYY.nlme.CI <- intervals(VONGMYYYY.nlme)
VONGMYYYY.nlme.CI
##
#Approximate 95% confidence intervals
#Fixed effects:
#  lower       est.       upper
#Linf 149.696844 157.423073 165.1493020
#logK  -2.019712  -1.834409  -1.6491055
#T0    -1.549166  -1.112880  -0.6765932
VONGMYYYY.nlme.CI$fixed
VONGMYYYY.nlme.CI$sigma
VONGMYYYY.nlme.CI$reStruct



# Random effects of Von B model parameters plot
test <- ranef(VONGMYYYY.nlme)
test.plot.Year <- data.frame(Parms=c(test[[1]]$Linf,test[[1]]$logK,test[[1]]$T0),
                             Pnames=rep(c("Linf","logK","T0"),each=length(unique(GMdetail.vonB$YEAR))),
                             Year=rep(unique(GMdetail.vonB$YEAR),3)) #note the 3 here is bc there's  3 random parameters ("Linf","logK","T0")

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
GMdetail.vonB$year.tow.no <- paste0(GMdetail.vonB$YEAR,"/",GMdetail.vonB$TOW.NO)
tow.attributes <- GMdetail.vonB %>% select(year.tow.no, DEPTH, BOTTOM_TEMP, YEAR)
rand.effect.year.tow.attributes <- merge(rand.effect.year.tow,tow.attributes,by=c("year.tow.no") )
#Above is just example, obviously depth used here is not good since it's not tide corrected, but this is just an example, and plots below, just example of what could try and do with vonB random effect so see if potential explanatory environmental variables in a future study; note depth/temperature, and fishing pressure likely all confounded 

#NOTE; if the tow that has age data doesn't haev associated attribute data (e.g. depth, bottom_temp, it won't plot -- see this if plot bottom temp - shows no data for some  years)
#plot L inf by depth or bottom temp, etc 
ggplot(data=rand.effect.year.tow.attributes, aes(x=DEPTH, y=Linf)) + 
  geom_point() + 
  facet_wrap(~YEAR) +
  geom_smooth() + theme_bw() + 
  ylab("Random effect")

#plot logK inf by observed temp when tow conducted (e.g. possible proxy for oceanography differences ) 
ggplot(data=rand.effect.year.tow.attributes, aes(x=DEPTH, y=logK)) + 
  geom_point() + 
  facet_wrap(~YEAR) +
  geom_smooth() + theme_bw() + 
  ylab("Random effect")

#plot T0 inf by depth 
ggplot(data=rand.effect.year.tow.attributes, aes(x=DEPTH, y=T0)) + 
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

predict.age <- expand.grid(AGE =0:20 , Year.fac = 2005:2015,TOW.NO = 1:1000)

#predict.age <- data.frame(AGE = rep(seq(from = 0, to = 20, by = 1),2), TOW.NO=1, Year.fac=c(rep("1996",21),rep("1997",21)))
p.curve <- predict(VONGMYYYY.nlme, predict.age, level=0:2)
head(p.curve)
tail(p.curve)
p.curve <- cbind(p.curve, (predict.age %>% select(AGE)))  #Note this merge assumes it's all in the right order

#only need one year of predict.fixed for plot since all fixed effects predictions are the same 
p.curve.fixed <- p.curve %>% filter(Year.fac=="2005")
p.curve.fixed.Year.fac <- unique(p.curve %>% select(Year.fac, predict.Year.fac, AGE))

#CIs: 
upper <- VONGMYYYY.nlme.CI$fixed[7]* (1 - exp(-exp(VONGMYYYY.nlme.CI$fixed[8]) * (predict.age$AGE  - VONGMYYYY.nlme.CI$fixed[3]))) 
lwr <- VONGMYYYY.nlme.CI$fixed[1]* (1 - exp(-exp(VONGMYYYY.nlme.CI$fixed[2]) * (predict.age$AGE  - VONGMYYYY.nlme.CI$fixed[9] )))

#Prediction for plotting with CIs 
p.curve.fixed <- cbind(p.curve.fixed, age=predict.age$AGE, upper, lwr)

#Keep only those year/tow.no combinations that actually exist in the data; but this messes up relative length of objects to plot so not used, only change is that plots would render faster 
#p.curve <- p.curve %>% dplyr::filter(TOW.NO %in% row.names(test$TOW.NO))

#PLOT VonB; note main prediction are the same at level 0; fixed effect only 
#note this plot could use slightly bigger axis labels
vonB.GM.95CI <- ggplot() + 
  #geom_point(data=GMdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.3) +
  geom_jitter(data=GMdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.1) + 
  geom_line(data=p.curve.fixed, aes(AGE, predict.fixed), colour="blue", linewidth = 2, show.legend = F) +
  geom_ribbon(data=p.curve.fixed,aes(ymin=lwr, ymax=upper, x=AGE), alpha=0.6, fill="blue") + 
  theme(panel.grid = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14)) + 
  xlab("Age (years)")+ylab("Shell height (mm)") + theme_bw()
vonB.GM.95CI

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/VonB_SPA",area,".png"),width=11,height=8,units = "in",res=920)
vonB.GM.95CI
dev.off()

#Random effect of year
vonB.GM.year.raneff <- ggplot() + 
  #geom_point(data=GMdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.3) +
  #geom_jitter(data=GMdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.1) + 
  #geom_line(data=p.curve.fixed, aes(AGE, predict.fixed), colour="blue", lwd=2, show.legend = F) +
  geom_ribbon(data=p.curve.fixed,aes(ymin=lwr, ymax=upper, x=AGE), alpha=0.6, fill="blue") + 
  theme(panel.grid = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14)) + 
  xlab("Age (years)")+ylab("Shell height (mm)") + theme_bw() + 
  geom_line(data=p.curve.fixed.Year.fac, aes(x=AGE, y=predict.Year.fac, group=Year.fac))
vonB.GM.year.raneff

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/VonB_SPA_year_raneff_",area,".png"),width=11,height=8,units = "in",res=920)
vonB.GM.year.raneff
dev.off()

#Random effect of tow within year
vonB.GM.tow.in.year.raneff <- ggplot() + 
  #geom_point(data=GMdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.3) +
  #geom_jitter(data=GMdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.1) + 
  #geom_line(data=p.curve.fixed, aes(age, predict.fixed), colour="blue", lwd=2, show.legend = F) +
  #geom_ribbon(data=p.curve.fixed,aes(ymin=lwr, ymax=upper, x=age), alpha=0.6, fill="blue") + 
  theme(panel.grid = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14)) + 
  xlab("Age (years)")+ylab("Shell height (mm)") + theme_bw() + 
  geom_line(data=p.curve, aes(x=AGE, y=predict.TOW.NO, group=TOW.NO))
vonB.GM.tow.in.year.raneff

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/VonB_SPA_year.tow_raneff_",area,".png"),width=11,height=8,units = "in",res=920)
vonB.GM.tow.in.year.raneff
dev.off()


#!!!Now save vonB model output as .RData object: e.g. BIvonB2021.RData
save(GMdetail.vonB, VONGMYYYY.nlme, file = paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/SPA",area,"/GMvonB",surveyyear,".RData"))

### END #### 

