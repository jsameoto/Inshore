##########  This is the main script for running the models, getting the catch/exploitation estimates, making the decision table and running the 
##########  most recent prediction evaluation year.
###    DK 2018 overhaul to align with BoF script format and minimizes manual entry...
###........................................###

# The Feb 2018 revision did the following to this file........................
# 1:  Removed the previous years model runs, the results of model runs from 2014-2017 are now stored in the folder ... \SPA29\Model_results_2014_2016\.  This was
#     done because there was no consistent file structure before this time for how the model results were stored (if they were even retained).  Now all model results for these
#     years should be easily located.  From 2017 onwards the model results from the previous year are loaded from where they were saved last year
# 2:  Defined a set number of iterations for all the model MCMC runs stored above, niter was 300,000, nburnin was 50,000, nchains = 3, and nthin = 5.  
# 3:  While niter and other MCMC parameters can be changed, unless the model structure changes I don't see any reason to use anything other than these values as the 
#     model appears to converge, we do need to look more closely at convergence and our posteriors v.s. priors (but I haven't done much here)
# 4:  Made a SFA29.inits function below, this is helpful  useful when running a bunch of different years,
# 5:  Using variable NY rather than entering the number of years in all places required.
# 6:  Created/organized some very basic file structure for the results from this based on what we had in place.  
#     Note that due to historical strangeness model output with a 2017 tag are used in the 2018 assessment, so you'll see all the correct model
#     ouput will be tagged with a 2017 but placed in the 2018 folder (and so on and so forth...)
# 7:  Prediction evaluations have been overhauled, you should now only need to run these for the current year, all previous years PE model results are stored
#     there is a little loop to grab all of these and stitch them together at the end of the function.
# 8:  The model input data are stored in the files.../SFA29/year/model/SFA29X_ModelData.xlsx, these still need to be updated manually each year, which is clearly
#      less than ideal, next big step in cleaning up SFA29 scripts would be to automate the creation of this file.

# JS Updated in Jan 2022 to write out files automatically and in line with new assessment folder structure; input diagnostics for model runs 

options(stringsAsFactors = FALSE)
#required packages
library(ggplot2)
library(R2WinBUGS)
library(SSModeltest) #v 1.0-5;  this is the SSModel package used in BoF, modified to accomodate the SFA29 models.
library(lubridate)
library(readxl)
library(tidyverse)
#library(SSModel) #v 1.0-5
source("Y:/Inshore/SFA29/2017/model/SFA29model9-2015.R") #contains the SFA29model model (BUGS) 


#DEFINE:
path.directory <- "Y:/Inshore/SFA29/"
assessmentyear <- 2022 #year in which you are conducting the assessment 
surveyyear <- 2021  #last year of survey data you are using, e.g. if max year of survey is survey from summer 2019, this would be 2019 
area <- "SFA29A"  

#yr <- year(Sys.Date()) # This should be set to the year after the year of the last survey.  e.g. if 2018 that means you are using the 2017 survey. This assumes you're running assessment in surveyyear + 1
#yrs <- 2001:surveyyear  #(yr-1) 
yrs <- 2001:2020  #(yr-1) 

#setwd
#setwd(paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Data/Model/"))


# Let's be consistent with our MCMC!
niter = 300000
nchains = 3
nburnin= 50000
nthin = 5

# A function to autmatically generate the initial values, this still sucks as nchains needs to be 3 here...

SFA29.inits <- function(N)
{
  list(list(q = 0.1, log.Kh = c(1.1, 7), sigma = c(1,0.1), Ph = cbind(runif(N,0.01,0.99),runif(N,0,1))),
       list( q = 0.4, log.Kh = c(7, 1.1), sigma = c(1, 1), Ph = cbind(runif(N,0.01,0.99),runif(N,0,1))),
       list( q = 0.2, log.Kh = c(4, 2), sigma = c(1, 0.5), Ph = cbind(runif(N,0.01,0.99),runif(N,0,1))))
}
# Here is a little function to extract the data from the "model data file" and get is set up for model...
dat.wrap <- function(dat,col="Ih",yrs,strata)
{
  num.strata <- length(strata)
  res <- matrix(nrow=length(yrs),ncol=num.strata)
  for(i in 1:num.strata) res[,i] <- unlist(dat[dat$Strata==strata[i] & dat$Year %in% yrs,col])
  res[which(res == 0)] <- 0.0001 # For these data any 0's need to be positive...
  return(res)
}
  
#priors (same values used for all subareas)
SFA29.priors <- list(r.a=0,r.b=0.1,m.a=-1.9,m.b=0.5,S.a=0.1,S.b=0.99,logKh.a=1,logKh.b=10,q.a=10.64,q.b=9.36,sigma.a=0,sigma.b=10)

#model parameters (same values used for all subareas)
#SFA29.parms <- c("Bh","BBh","Ph","Rh" ,"Kh" , "m" , "sigma","q" , "S" , "log.Kh","Catch.t","CPUE.sub")
SFA29.parms <- c("Bh","BBh","Ph","Rh" ,"Kh" , "m" , "sigma","q" , "S" , "log.Kh","Catch.t","CPUE.sub", "resid.p", "sresid.p")


# the relationship between fishing intensity and density; !unique to subarea!
e.parms.29A <- c(0.003317,  0.004601,  0.000000, 35.010000)
# The area of each strata in A, I think...
A.area <- c(137.5875, 118.125)
num.areas <- length(A.area) # The number of strata in the area...

# Now bring in the model data...
#mod.dat <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/SFA29A_ModelData.2021.csv"))  
#mod.dat <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/test_min_gh_for2020/SFA29A_ModelData.2021_Minghintimeseriesfor2020.csv"))  
#mod.dat <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/test_max_gh_for2020/SFA29A_ModelData.2021_Maxghintimeseriesfor2020.csv"))  
mod.dat <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/ModelRun_to2020/SFA29A_ModelData.2020.csv"))  

strata <- c("low","med")

# Get the growth data, this will be in the order for the "strata" is above, as long as this is low then medium it should work (low, med is how the data is organized)
#growth.paras <- mod.dat$gh[mod.dat$Year ==max(yrs) & mod.dat$Strata %in% strata]
# Get the growth data, this will be in the order for the "strata" is above, needs to be ordered low,  medium,  high 
growth.paras <- mod.dat %>% filter(Year == max(yrs) & Strata %in% strata) %>% select(Strata, gh) %>% arrange(match(Strata, c("low", "med"))) %>% select(gh)
growth.paras  

##
#  ---- Current year update ----
##
# Get the initial values
yrs <- 2001:surveyyear # The years for the model.
NY <- length(yrs)
inits.29A <- SFA29.inits(NY)

# Note that the values entering the model (log-normal or gamma priors) need to be > 0, so if any values are 0 we add 0.0001 to them.  This 
# excludes Catch.actual since it does not need to be 0....  This is done in the "dat.wrap function.
SFA29Adata <- list(
N = NY,
H = num.areas,
Area =A.area,
N1 = 1,
Catch.actual = mod.dat$Catch.actual[!is.na(mod.dat$Catch.actual)], # First year won't have information
Ih = dat.wrap(mod.dat,"Ih",yrs,strata),
rh = dat.wrap(mod.dat,"rh",yrs,strata),
clappers = dat.wrap(mod.dat,"clappers",yrs,strata),
L = dat.wrap(mod.dat,"L",yrs,strata),
gh = dat.wrap(mod.dat,"gh",yrs,strata),
wk = mod.dat$wk[mod.dat$Year %in% yrs & !is.na(mod.dat$wk)], # Just removing trailing NA's that are in the data
obs.nu = dat.wrap(mod.dat,"obs.nu",yrs,strata),
obs.tau = dat.wrap(mod.dat,"obs.tau",yrs,strata),
obs.phi = dat.wrap(mod.dat,"obs.phi",yrs,strata),
VMS.Effort = dat.wrap(mod.dat,"VMSEffort",yrs[-length(yrs)],strata)) # There won't be VMS data for the most recent data

# now run the model and save the results.  This is taking ball park 1 hour to run 
A.mod.res <- SSModel(SFA29Adata,SFA29.priors,inits.29A,parms=SFA29.parms,model.file=SFA29model,
                Years=yrs, nchains=nchains,niter=niter,nburnin=nburnin,nthin=nthin,Area="SFA29W",e.parms=e.parms.29A,debug=F)

mod.res <- A.mod.res
# You want this to be a minimum of 400, if less than this you should increase your chain length
min.neff <- min(mod.res$summary[,9])
if(min.neff < 400) print(paste("Hold up sport!! Minimum n.eff is",min.neff," You should re-run the model and increase nchains until this is at least 400")) 
if(min.neff >= 400) print(paste("Good modelling friend, your minimum n.eff is",min.neff)) 
# same idea for Rhat, here we want to make sure Rhat is < 1.05 which suggests the chains are well mixed.
Rhat <- signif(max(mod.res$summary[,8]),digits=4)
if(Rhat > 1.05) print(paste("Hold up mes amis!! You have an Rhat of ",Rhat," You should re-run the model and increase nchains until this is below 1.05")) 
if(Rhat <= 1.05) print(paste("Good modelling friend, your max Rhat is",Rhat)) 


#Save out 
save(A.mod.res,file=paste0(file = paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/SFA29A.",surveyyear,".RData"))) 
#save(A.mod.res,file=paste0(file = paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/test_min_gh_for2020/SFA29A.",surveyyear,".RData"))) 
#save(A.mod.res,file=paste0(file = paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/test_max_gh_for2020/SFA29A.",surveyyear,".RData"))) 
#save(A.mod.res,file=paste0(file = paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/ModelRun_to2020/SFA29A.2020.RData"))) 


write.csv(A.mod.res$summary,file=paste0(file = paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/SFA29.A.mod.res.summary.",surveyyear,".csv"))) 
#write.csv(A.mod.res$summary,file=paste0(file = paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/test_min_gh_for2020/SFA29.A.mod.res.summary.",surveyyear,".csv"))) 
#write.csv(A.mod.res$summary,file=paste0(file = paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/test_max_gh_for2020/SFA29.A.mod.res.summary.",surveyyear,".csv"))) 
#write.csv(A.mod.res$summary,file=paste0(file = paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/ModelRun_to2020/SFA29.A.mod.res.summary.2020.csv"))) 


# Get the catch and biomass so that we can figure out the explotation rate from the model for each habitat
# The catch and CPUE data needs to fill in the years for which there was no catch....
# UPDATE EACH YEAR::: med.cat, med.CPUE, low.cat, low.CPUE::: seq(2,X,2) (add 2 to X each year to account new new data)
# No fishing 2017 season
# med.cat <- apply(A.mod.res$sims.matrix[,grep("Catch.t",colnames(A.mod.res$sims.matrix))],2,median)[seq(2,30,2)] -- why the seq? - to get 2nd row -- but need to adjust the "30" every year, can we automate this? 
#med.cat <- apply(A.mod.res$sims.matrix[,grep("Catch.t",colnames(A.mod.res$sims.matrix))],2,median)[seq(2,30,2)]

#Catch 
 med.cat <- apply(A.mod.res$sims.matrix[,grep("Catch.t",colnames(A.mod.res$sims.matrix))],2,median)
 med.cat <- med.cat[grep(",2]",names(med.cat))]
 med.cat <- c(0,med.cat[1],0,med.cat[2:12],0,0,med.cat[13:length(med.cat)])
 length(med.cat)
 
#Biomass  
# med.bm <-  apply(A.mod.res$sims.matrix[,grep("^Bh",colnames(A.mod.res$sims.matrix))],2,median)[seq(2,2*length(yrs),2)]
 med.bm <-  apply(A.mod.res$sims.matrix[,grep("^Bh",colnames(A.mod.res$sims.matrix))],2,median)
 med.bm <- med.bm[grep(",2]",names(med.bm))]
 length(med.bm)
 
#mortality 
# med.nat.m <-  apply(A.mod.res$sims.matrix[,grep("^m",colnames(A.mod.res$sims.matrix))],2,median)[seq(2,2*length(yrs),2)]
 med.nat.m <-  apply(A.mod.res$sims.matrix[,grep("^m",colnames(A.mod.res$sims.matrix))],2,median) 
 med.nat.m <- med.nat.m[grep(",2]",names(med.nat.m))]
 length(med.nat.m)
 
#modelled CPUE 
# med.CPUE <- apply(A.mod.res$sims.matrix[,grep("CPUE.sub",colnames(A.mod.res$sims.matrix))],2,median)[seq(2,30,2)]
 med.CPUE <- apply(A.mod.res$sims.matrix[,grep("CPUE.sub",colnames(A.mod.res$sims.matrix))],2,median)
 med.CPUE <- med.CPUE[grep(",2]",names(med.CPUE))]
 med.CPUE <- c(0,med.CPUE[1],0,med.CPUE[2:12],0,0,med.CPUE[13:length(med.CPUE)])
 length(med.CPUE)
 
# now the same in the low strata...
#Catch  
 #low.cat <- apply(A.mod.res$sims.matrix[,grep("Catch.t",colnames(A.mod.res$sims.matrix))],2,median)[seq(1,30,2)]
 low.cat <- apply(A.mod.res$sims.matrix[,grep("Catch.t",colnames(A.mod.res$sims.matrix))],2,median)
 low.cat <- low.cat[grep(",1]",names(low.cat))]
 low.cat <- c(0,low.cat[1],0,low.cat[2:12],0,0,low.cat[13:length(low.cat)])
 length(low.cat)
 
#Biomass
 #low.bm <-  apply(A.mod.res$sims.matrix[,grep("Bh",colnames(A.mod.res$sims.matrix))],2,median)[seq(1,2*length(yrs),2)]
 low.bm <- apply(A.mod.res$sims.matrix[,grep("^Bh",colnames(A.mod.res$sims.matrix))],2,median)
 low.bm <- low.bm[grep(",1]",names(low.bm))]
 length(low.bm)
 
#mortality  
 #low.nat.m <-  apply(A.mod.res$sims.matrix[,grep("^m",colnames(A.mod.res$sims.matrix))],2,median)[seq(1,2*length(yrs),2)]
 low.nat.m <-  apply(A.mod.res$sims.matrix[,grep("^m",colnames(A.mod.res$sims.matrix))],2,median)
 low.nat.m <- low.nat.m[grep(",1]",names(low.nat.m))]
 length(low.nat.m)
 
#modelled CPUE 
 #low.CPUE <- apply(A.mod.res$sims.matrix[,grep("CPUE.sub",colnames(A.mod.res$sims.matrix))],2,median)[seq(1,30,2)]
 low.CPUE <- apply(A.mod.res$sims.matrix[,grep("CPUE.sub",colnames(A.mod.res$sims.matrix))],2,median)
 low.CPUE <- low.CPUE[grep(",1]",names(low.CPUE))]
 low.CPUE <- c(0,low.CPUE[1],0,low.CPUE[2:12],0,0,low.CPUE[13:length(low.CPUE)])
 length(low.CPUE)
 
# # The dataframe with the results.  Note this is set up as Catch(2015) / (Catch2015 + Biomass2015)
# # This isn't entirely correct since we pull catch from an intermediate biomass, but it's pretty good!
 dat <- data.frame(Year = rep(yrs,2),Catch = c(med.cat,low.cat),Biomass = c(med.bm,low.bm),
                   nat.m = c(med.nat.m,low.nat.m),
                   CPUE = c(med.CPUE,low.CPUE),
                   mu = c(med.cat/(med.cat+med.bm),
                          low.cat/(low.cat+low.bm)),
                   Habitat = c(rep("Med",length(yrs)),rep("Low",length(yrs))),
                   size.area = c(rep(A.area[2],length(yrs)),rep(A.area[1],length(yrs))),
                   area = rep("SFA29A",2*length(yrs)),
                   row.names= NULL)
 dat$BM.dens <- dat$Biomass/dat$size.area
 dat
 
# This data all gets combined so we can make the figures later... 
write.csv(dat, file=paste0(file = paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/SFA29.A.model.results.summary.",surveyyear,".csv")), row.names = FALSE) 
#write.csv(dat, file=paste0(file = paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/test_min_gh_for2020/SFA29.A.model.results.summary.",surveyyear,".csv")), row.names = FALSE) 
#write.csv(dat, file=paste0(file = paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/test_max_gh_for2020/SFA29.A.model.results.summary.",surveyyear,".csv")), row.names = FALSE) 
#write.csv(dat, file=paste0(file = paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/ModelRun_to2020/SFA29.A.model.results.summary.2020.csv")), row.names = FALSE) 

#prior/posterior plot
png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Model/SFA29A/SFA29.A.prior.post.",surveyyear,".png"),width=11,height=8,units = "in",res=920)
plot(A.mod.res)
dev.off()

#plot catch to ensure everything makes sense and that you've put the 0's in the right years (compare back to previous year figures)
plot.Catch <- ggplot(dat, aes(x=Year, y=Catch, color=Habitat, group=Habitat)) + geom_point() + geom_line() + 
  theme_bw() 
plot.Catch

#plot biomass Density to ensure everything makes sense - compare back to previous year figures 
plot.Biomass.Density <- ggplot(dat, aes(x=Year, y=BM.dens, color=Habitat, group=Habitat)) + geom_point() + geom_line() + 
  theme_bw() 
plot.Biomass.Density

#process residuals 
A.mod.res.df <- as.data.frame(A.mod.res$summary)
resids <- A.mod.res.df[grep("^resid.p",row.names.data.frame(A.mod.res.df)),]
resids.low <- resids[grep(",1]",row.names.data.frame(resids)),]
resids.low$STRATA <- "low"
resids.low$Year <- 2001:surveyyear 
resids.med <- resids[grep(",2]",row.names.data.frame(resids)),]
resids.med$STRATA <- "med"
resids.med$Year <- 2001:surveyyear 

resids <- rbind(resids.low,resids.med)

plot.residuals <- ggplot(resids) + geom_point(aes(x=Year, y= mean)) + 
  geom_errorbar(aes(x = Year,ymin = `2.5%`,ymax=`97.5%`),width=0) + 
  theme_bw() + geom_hline(yintercept = 0) + facet_wrap(~STRATA)
plot.residuals

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Model/SFA29A/SFA29.A.plot.resid.p.",surveyyear,".png"),width=11,height=8,units = "in",res=920)
plot.residuals
dev.off()

#process residuals standardized 
A.mod.res.df <- as.data.frame(A.mod.res$summary)
s.resids <- A.mod.res.df[grep("^sresid.p",row.names.data.frame(A.mod.res.df)),]
s.resids.low <- s.resids[grep(",1]",row.names.data.frame(s.resids)),]
s.resids.low$STRATA <- "low"
s.resids.low$Year <- 2001:surveyyear 
s.resids.med <- s.resids[grep(",2]",row.names.data.frame(s.resids)),]
s.resids.med$STRATA <- "med"
s.resids.med$Year <- 2001:surveyyear 

s.resids <- rbind(s.resids.low,s.resids.med)

plot.std.residuals <- ggplot(s.resids) + geom_point(aes(x=Year, y= mean)) + 
  geom_errorbar(aes(x = Year,ymin = `2.5%`,ymax=`97.5%`),width=0) + 
  theme_bw() + geom_hline(yintercept = 0) + facet_wrap(~STRATA)
plot.std.residuals

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Model/SFA29A/SFA29.A.plot.s.resids.",surveyyear,".png"),width=11,height=8,units = "in",res=920)
plot.std.residuals
dev.off()


#Predict for next year, using the growth parameters from the model data...
A.next.predict <- predict(A.mod.res,exploit=0.1,g.parm=c(growth.paras$gh,NA))  #predict expect gh to be in order low, med, high - bc A only has low, med, add NA for high 
summary(A.next.predict)


#Method only does one exploitation at a time, note that I have changed the probabilites to be like those for BoF
#Workaround
set.seed(10) # set the random number generator if you want to make your results reproducable.
Decision.table <- matrix(NA,10,7)
dimnames(Decision.table)[[2]] <- names(summary(A.next.predict)$Next.year)

# The decision table for next years fishing season...
for(i in 1:10)
{
  temp<-predict(A.mod.res,exploit=0.02*(i-1), g.parm=c(growth.paras$gh,NA))
  Decision.table[i,]<-as.vector(unlist(summary(temp)$Next.year))
}
colnames(Decision.table) <- c(names(summary(temp)$Next.year))

Decision.table

#Write out decision table 
write.csv(Decision.table, paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/SFA29.A.mod.Decision.table.",surveyyear,".csv"), row.names = FALSE) 


############  Finally we can do prediction evaluations for each year....
## --- THE PREDICTION EVALUATION ----
## Now we can just run this through a loop rather than keep re-running these...
# Set the first year you want to run, note that we will have these saved for all years before the current year
# so there is no reason to run this for anything but the current year

first.pe.year <- max(yrs) # The results for previous years are all saved (see below), so there is no reason to set this to anything but yr-1 and
# just run this for 1 year
pe.years <- first.pe.year:first.pe.year
num.pe.years <- length(pe.years)
pe.res <- NULL
pe.pred <- NULL
# Walk through the model for each year...
for(i in 1:num.pe.years)
{

pe.yrs <- 2001:pe.years[i] # The years for the model.
pe.NY <- length(pe.yrs)
inits.29A <- SFA29.inits(pe.NY)  

SFA29Adata <- list(
  N = pe.NY,
  H = num.areas,
  Area =A.area,
  N1 = 1,
  Catch.actual = mod.dat$Catch.actual[mod.dat$Year %in% pe.yrs & !is.na(mod.dat$Catch.actual)], # First year won't have information
  Ih = dat.wrap(mod.dat,"Ih",pe.yrs,strata),
  rh = dat.wrap(mod.dat,"rh",pe.yrs,strata),
  clappers = dat.wrap(mod.dat,"clappers",pe.yrs,strata),
  L = dat.wrap(mod.dat,"L",pe.yrs,strata),
  gh = dat.wrap(mod.dat,"gh",pe.yrs,strata),
  wk = mod.dat$wk[mod.dat$Year %in% pe.yrs & !is.na(mod.dat$wk)], # Just removing trailing NA's that are in the data
  obs.nu = dat.wrap(mod.dat,"obs.nu",pe.yrs,strata),
  obs.tau = dat.wrap(mod.dat,"obs.tau",pe.yrs,strata),
  obs.phi = dat.wrap(mod.dat,"obs.phi",pe.yrs,strata),
  VMS.Effort = dat.wrap(mod.dat,"VMSEffort",pe.yrs[-length(pe.yrs)],strata)) # There won't be VMS data for the most recent data


pe.res[[as.character(pe.years[i])]] <- SSModel(SFA29Adata,SFA29.priors,inits.29A,parms=SFA29.parms,model.file=SFA29model,
                   Years=pe.yrs, nchains=nchains,niter=niter,nburnin=nburnin,nthin=nthin,Area="SFA29W",e.parms=e.parms.29A,debug=F)
pe.pred[[as.character(pe.years[i]+1)]] <- predict(pe.res[[as.character(pe.years[i])]],Catch=SFA29Adata$Catch.actual[length(SFA29Adata$Catch.actual)],
                                                g.parm=mod.dat$gh[mod.dat$Year ==pe.years[i] & mod.dat$Strata %in% strata])
}

#save(pe.pred,file = "Y:/INSHORE SCALLOP/SFA29/Model_results_2014_2016/SFA29A/prediction_evaluation_results_2011_2016.RData")
#save(pe.pred,file = paste0(getwd(),"/SFA29A_results/prediction_evaluation_results_",pe.years,".RData"))
save(pe.pred,file=paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/SFA29A.prediction.evaluation.results.",max(yrs),".RData")) 



# Now to make the historic plot below
# Assuming you haven't re-run everything above and wasted several hourse of you life
# we need to bring in all the prediction evalulation data we have....
# Now load all the old pe.pred results, first identify where to find the data...
direct <- "Y:/Inshore/SFA29/"
#direct <- "D:/r/Inshore/SFA29W"
pe.all <- NULL
#for(i in 2016:pe.years) {
for(i in 2016:2021) {
  if(i == 2016) 
  {
    load(paste0(direct,"Model_results_2015_2017/SFA29A/prediction_evaluation_results_2011_2016.RData"))
    pe.all <- pe.pred
  } # end if(i == 2016) 
  # Now load the more recent pe evaluation data
  if(i %in% 2017:2019) 
  {
  load(paste0(direct,(i+1),"/model/SFA29A_results/prediction_evaluation_results_",i,".RData"))
    pe.all <- c(pe.all,pe.pred)
  }  
  if(i == 2020)
  {
    load(paste0(direct,"2022/Assessment/Data/Model/SFA29A/ModelRun_to2020/SFA29A.prediction.evaluation.results.2020.RData"))
    pe.all <- c(pe.all,pe.pred)
  }
  if(i == 2021)
  {
    load(paste0(direct,"2022/Assessment/Data/Model/SFA29A/SFA29A.prediction.evaluation.results.2021.RData"))
    pe.all <- c(pe.all,pe.pred)
  }
} 


class(pe.all) <- "SFA29"

# Plot Prediction Evaluation 
# This produces 1 plot for low, 1 for medium, and 1 for high
#View to make sure it's what you expect! 
#windows(11,11)
par(mfrow=c(2,1))
SSModeltest::eval.predict.SFA29(pe.all,Year=2012,pred.lim=0.2)
dev.off()

#Save out plot 
png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Model/SFA29A/SFA29.A.predict.evaluation.",surveyyear,".png"),width=9,height=9,units = "in", res=400)
par(mfrow=c(2,1))
SSModeltest::eval.predict.SFA29(pe.all,Year=2012,pred.lim=0.2)
dev.off()




################
#use 1 year natural mortality to see effect on decision tables 

#Predict for next year, using the growth parameters from the model data...
A.next.predict <- predict(A.mod.res,exploit=0.1,g.parm=c(growth.paras$gh,NA), m.avg = 1)  #predict expect gh to be in order low, med, high - bc A only has low, med, add NA for high 
summary(A.next.predict)


#Method only does one exploitation at a time, note that I have changed the probabilites to be like those for BoF
#Workaround
set.seed(10) # set the random number generator if you want to make your results reproducable.
Decision.table <- matrix(NA,10,7)
dimnames(Decision.table)[[2]] <- names(summary(A.next.predict)$Next.year)

# The decision table for next years fishing season...
for(i in 1:10)
{
  temp<-predict(A.mod.res,exploit=0.02*(i-1), g.parm=c(growth.paras$gh,NA), m.avg = 1)
  Decision.table[i,]<-as.vector(unlist(summary(temp)$Next.year))
}
colnames(Decision.table) <- c(names(summary(temp)$Next.year))

Decision.table

#Write out decision table 
write.csv(Decision.table, paste0(path.directory,assessmentyear,"/Assessment/Data/Model/SFA29A/SFA29.A.mod.Decision.table.nat.m.1yr.",surveyyear,".csv"), row.names = FALSE) 

