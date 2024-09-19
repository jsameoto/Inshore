###........................................###
###
###    von Bertalanffy modelling
###    BoF: SPA1A,1B,4,and 5
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
cruise <- "BF2024"  #note should match year for surveyyear set above 

assessmentyear <- 2024 #year in which you are conducting the survey 
area <- "1A1B4and5"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
path.directory <- "Y:/Inshore/BoF/"


#required packages
#library (lme4)
#library(nlme)
#library(RODBC)
#library(lattice)

# ---- Obtain data for VonB modelling ---- 
#strata.spa1a<-c(6,7,12:20,39)
#strata.spa1b<-c(35,37,38,49,51:53)
#strata.spa4<-c(1:5, 8:10)
#strata.spa5==21

#SQL query 1: detailed meat weight/shell height sampling data
quer1 <- paste0("SELECT * FROM scallsur.scwgthgt WHERE strata_id IN (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 35, 37, 38, 39, 49, 51, 52, 53)")

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# Select data from database; execute query with ROracle
#detailed sampling data
BFdetail.vonB <- dbGetQuery(chan, quer1)

#add YEAR column to data
BFdetail.vonB$YEAR <- as.numeric(substr(BFdetail.vonB$CRUISE,3,6))
names(BFdetail.vonB)[2] <-"TOW.NO" #rename TOW_NO column; lme4 doesn't like underscores in column names

#Check years that have aging data: 
BFdetail.vonB %>% filter(!is.na(AGE)) %>% distinct(YEAR)
#Since 2015 RAP, only use aging data up to 2015. We'll use newer data when we go to the next RAP process. 
BFdetail.vonB <- BFdetail.vonB %>% filter(!is.na(AGE)) %>% filter(YEAR <= 2015)

####
# check detail file for NAs (WET_MEAT_WEIGHT and HEIGHT) and positive depths
####
summary(BFdetail.vonB)
BFdetail.vonB <- BFdetail.vonB[complete.cases(BFdetail.vonB[,c(grep("HEIGHT", colnames(BFdetail.vonB)))]),]  #remove NAs from HEIGHT
BFdetail.vonB <- BFdetail.vonB[complete.cases(BFdetail.vonB[,c(grep("AGE", colnames(BFdetail.vonB)))]),]  #remove NAs from AGE
summary(BFdetail.vonB)

#Year must be a factor for modelling 
BFdetail.vonB$Year.fac <- as.factor(BFdetail.vonB$YEAR)
summary(BFdetail.vonB)
str(BFdetail.vonB)
levels(BFdetail.vonB$Year.fac)


# ---- Von B with random effect  on Linf AND logk AND T0 ----
## THIS MODEL USED IN 2016 and used for subsequent UPDATES

begTime <- Sys.time()
VONBFYYYY.nlme <- nlme(HEIGHT~SSasympOff(AGE,Linf,logK,T0), 
              random=~Linf+logK+T0~1|Year.fac/TOW.NO,
              fixed=list(Linf~1,logK~1,T0~1), 
              data=BFdetail.vonB,start=c(144,-1.7738,-1.0),
              method="REML", na.action = na.omit)
runTime <- Sys.time()-begTime
runTime #just for curiosity:   18.39 mins
summary (VONBFYYYY.nlme)

#Note warning from 1996 to 2015 data model run: 
#Warning message:
#  In nlme.formula(HEIGHT ~ SSasympOff(AGE, Linf, logK, T0), random = ~Linf +  :
#                    Iteration 2, LME step: nlminb() did not converge (code = 1). Do increase 'msMaxIter'!

#> summary (VONBFYYYY.nlme)
#Nonlinear mixed-effects model fit by REML
#Model: HEIGHT ~ SSasympOff(AGE, Linf, logK, T0)
#Data: test.data
#AIC    BIC  logLik
#460186 460334 -230077

#Random effects:
#  Formula: list(Linf ~ 1, logK ~ 1, T0 ~ 1)
#Level: Year.fac
#Structure: General positive-definite, Log-Cholesky parametrization
#StdDev   Corr
#Linf 16.84983 Linf   logK
#logK  0.33943 -0.947
#T0    0.74212 -0.779  0.922

#Formula: list(Linf ~ 1, logK ~ 1, T0 ~ 1)
#Level: TOW.NO %in% Year.fac
#Structure: General positive-definite, Log-Cholesky parametrization
#StdDev  Corr
#Linf     9.97556 Linf   logK
#logK     0.16864 -0.723
#T0       0.38894 -0.285  0.803
#Residual 3.99479

#Fixed effects: list(Linf ~ 1, logK ~ 1, T0 ~ 1)
#Value Std.Error    DF t-value p-value
#Linf 148.197    3.7847 77980  39.156  0.0000
#logK  -1.576    0.0762 77980 -20.698  0.0000
#T0    -0.366    0.1665 77980  -2.195  0.0281
#Correlation:
#  Linf   logK
#logK -0.946
#T0   -0.776  0.921

#Standardized Within-Group Residuals:
#  Min         Q1        Med         Q3        Max
#-8.6933965 -0.6085053  0.0087226  0.6349398  9.6892768

#Number of Observations: 79915
#Number of Groups:
#  Year.fac TOW.NO %in% Year.fac
#      20                 1933


# ---- Von B with random effect ONLY on Linf AND logk (NOT on T0) ----
# i.e. random=~Linf+logK~1 (without T0)  
#begTime <- Sys.time()
#VONBFYYYY.nlme1 <- nlme(HEIGHT~SSasympOff(AGE,Linf,logK,T0), 
#                       random=~Linf+logK~1|Year.fac/TOW.NO,
#                       fixed=list(Linf~1,logK~1,T0~1), 
#                       data=BFdetail.vonB,start=c(144,-1.7738,-1.0),
#                       method="REML", na.action = na.omit)
#runTime <- Sys.time()-begTime
#runTime # 38 sec
#summary(VONBFYYYY.nlme1)

#Fixed effects: list(Linf ~ 1, logK ~ 1, T0 ~ 1) 
#Value Std.Error    DF   t-value p-value
#Linf 148.83257 2.4029107 77980  61.93845       0
#logK  -1.60573 0.0368524 77980 -43.57202       0
#T0    -0.32735 0.0055999 77980 -58.45579       0


# ---- PLOT FOR VONB ----
VONBFYYYY.nlme.CI <- intervals(VONBFYYYY.nlme)
VONBFYYYY.nlme.CI
#Approximate 95% confidence intervals
#Fixed effects:
#          lower        est.        upper
#Linf 140.7786114 148.1966770 155.61474262
#logK  -1.7254698  -1.5762138  -1.42695779
#T0    -0.6919948  -0.3655897  -0.03918449

# Random effects of Von B model parameters plot
test <- ranef(VONBFYYYY.nlme)
test.plot.Year <- data.frame(Parms=c(test[[1]]$Linf,test[[1]]$logK,test[[1]]$T0),
                          Pnames=rep(c("Linf","logK","T0"),each=length(unique(BFdetail.vonB$YEAR))),
                           Year=rep(unique(BFdetail.vonB$YEAR),3)) #note the 3 here is bc there's  3 random parameters ("Linf","logK","T0")

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
BFdetail.vonB$year.tow.no <- paste0(BFdetail.vonB$YEAR,"/",BFdetail.vonB$TOW.NO)
tow.attributes <- BFdetail.vonB %>% select(year.tow.no, DEPTH, BOTTOM_TEMP, YEAR)
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

predict.age <- expand.grid(AGE =0:20 , Year.fac = 1996:2015,TOW.NO = 1:1000)

#predict.age <- data.frame(AGE = rep(seq(from = 0, to = 20, by = 1),2), TOW.NO=1, Year.fac=c(rep("1996",21),rep("1997",21)))
p.curve <- predict(VONBFYYYY.nlme, predict.age, level=0:2)
head(p.curve)
tail(p.curve)
p.curve <- cbind(p.curve, (predict.age %>% dplyr::select(AGE)))  #Note this merge assumes it's all in the right order

#only need one year of predict.fixed for plot since all fixed effects predictions are the same 
p.curve.fixed <- p.curve %>% filter(Year.fac=="1996")
p.curve.fixed.Year.fac <- unique(p.curve %>% dplyr::select(Year.fac, predict.Year.fac, AGE))

#CIs: 
upper <- VONBFYYYY.nlme.CI$fixed[7]* (1 - exp(-exp(VONBFYYYY.nlme.CI$fixed[8]) * (predict.age$AGE  - VONBFYYYY.nlme.CI$fixed[3]))) 
lwr <- VONBFYYYY.nlme.CI$fixed[1]* (1 - exp(-exp(VONBFYYYY.nlme.CI$fixed[2]) * (predict.age$AGE  - VONBFYYYY.nlme.CI$fixed[9] )))

#Prediction for plotting with CIs 
p.curve.fixed <- cbind(p.curve.fixed, age=predict.age$AGE, upper, lwr)

#Keep only those year/tow.no combinations that actually exist in the data; but this messes up relative length of objects to plot so not used, only change is that plots would render faster 
#p.curve <- p.curve %>% dplyr::filter(TOW.NO %in% row.names(test$TOW.NO))

#PLOT VonB; note main prediction are the same at level 0; fixed effect only 
#note this plot could use slightly bigger axis labels
vonB.BF.95CI <- ggplot() + 
  #geom_point(data=BFdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.3) +
  geom_jitter(data=BFdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.1) + 
  geom_line(data=p.curve.fixed, aes(AGE, predict.fixed), colour="blue", lwd=2, show.legend = F) +
  geom_ribbon(data=p.curve.fixed,aes(ymin=lwr, ymax=upper, x=AGE), alpha=0.6, fill="blue") + 
  theme(panel.grid = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14)) + 
  xlab("Age (years)")+ylab("Shell height (mm)") + theme_bw()
vonB.BF.95CI

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/VonB_SPA",area,".png"),width=11,height=8,units = "in",res=920)
vonB.BF.95CI
dev.off()

#Random effect of year
vonB.BF.year.raneff <- ggplot() + 
  #geom_point(data=BFdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.3) +
  #geom_jitter(data=BFdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.1) + 
  #geom_line(data=p.curve.fixed, aes(AGE, predict.fixed), colour="blue", lwd=2, show.legend = F) +
  geom_ribbon(data=p.curve.fixed,aes(ymin=lwr, ymax=upper, x=AGE), alpha=0.6, fill="blue") + 
  theme(panel.grid = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14)) + 
  xlab("Age (years)")+ylab("Shell height (mm)") + theme_bw() + 
  geom_line(data=p.curve.fixed.Year.fac, aes(x=AGE, y=predict.Year.fac, group=Year.fac))
vonB.BF.year.raneff

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/VonB_SPA_year_raneff_",area,".png"),width=11,height=8,units = "in",res=920)
vonB.BF.year.raneff
dev.off()

#Random effect of tow within year
vonB.BF.tow.in.year.raneff <- ggplot() + 
  #geom_point(data=BIdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.3) +
  #geom_jitter(data=BFdetail.vonB, aes(AGE, HEIGHT), size=1.6, alpha=0.1) + 
  #geom_line(data=p.curve.fixed, aes(age, predict.fixed), colour="blue", lwd=2, show.legend = F) +
  #geom_ribbon(data=p.curve.fixed,aes(ymin=lwr, ymax=upper, x=age), alpha=0.6, fill="blue") + 
  theme(panel.grid = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14)) + 
  xlab("Age (years)")+ylab("Shell height (mm)") + theme_bw() + 
  geom_line(data=p.curve, aes(x=AGE, y=predict.TOW.NO, group=TOW.NO))

vonB.BF.tow.in.year.raneff

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/VonB_SPA_year.tow_raneff_",area,".png"),width=11,height=8,units = "in",res=920)
vonB.BF.tow.in.year.raneff
dev.off()


#!!!Now save vonB model output as .RData object: e.g. BIvonB2021.RData
save(BFdetail.vonB, VONBFYYYY.nlme, file = paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/SPA",area,"/BFvonB",surveyyear,".RData"))

### END #### 

