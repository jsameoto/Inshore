#######################################################################
### function to summarize model output for use in CSAS document ######
#######################################################################

### edited by FK in June 2020

### Requires:
### Model RData file (like SPA4_Model_2019.Rdata)

### Saves:
### summary stats_4_2019.csv: paste(direct, assessmentyear, "/Assessment/Data/Model/SPA", area, "/summary stats_", area, "_", surveyyear, ".csv",sep="")
### summary stats temporal_4_2019.csv: paste(direct , assessmentyear, "/Assessment/Data/Model/SPA", area, "/summary stats temporal_", area, "_", surveyyear, ".csv",sep="")

BoF.model.stats <- function(area, assessmentyear, surveyyear, direct, RDatafile) {
  
  ### summary table of model output for CSAS doc
  # Spa1B.2017$summary) # summary results of posterior. B[1] is first year commercial biomass etc.
  # R is recruit biomass
  # q is catchability
  # K is carrying capacity
  # P is standardization of biomass (convergence thing, always around 1, helps BUGS deal)
  # sigma is process variance
  # S is length of time clappers "live"
  # m is natural mortality
  # kappa.tau is inv variance of S
  # r is ____ (possibly observation of R... something to do with recruits)
  # Fmort is instantaneous fishing mortality
  # mu is proportional fishing mortality
  # Irep is survey commercial biomass estimate
  # IRrep is survey recruit biomass estimate
  
  #setwd(direct)
  
  years <- data.frame(area=c("1A", "1B", "3", "4", "6"))
  
  years$minyear <- ifelse(years$area=="1A", 1997,
                          ifelse(years$area=="1B", 1997,
                                 ifelse(years$area=="3", 1996,
                                        ifelse(years$area=="4", 1983,
                                               ifelse(years$area=="6", 2006, NA)))))                                
  
  years$maxyear <- surveyyear
  
  yrs <- years$minyear[years$area==area]:years$maxyear[years$area==area]
  
  NY <- length(yrs)
  
  years$NYminyear <- NY - (surveyyear - years$minyear)
  years$NYmaxyear <- NY - (surveyyear - years$maxyear)
  years$NYlastyear <- NY - (surveyyear - years$maxyear) - 1
  
  options(scipen=999)
  
  modeloutput <- get(load(file = paste(direct, assessmentyear, "/Assessment/Data/Model/SPA", area, "/", RDatafile, ".RData", sep="")))
  
  summary.stats <- data.frame(description = c("recruit.biomass", 
                                                 "comm.biomass",
                                                 "prop.fishing.mort",
                                                 "natural.mort",
                                                 "catchability",
                                                 "car.capacity",
                                                 "process.var",
                                                 "clapper.duration"),
                                 variable = c("R", 
                                              "B",
                                              "mu",
                                              "m", 
                                              "q",
                                              "K",
                                              "sigma",
                                              "S"),
                                 old.value = c(modeloutput$summary[paste0("R[", NY-1, "]"),5],  
                                               modeloutput$summary[paste0("B[", NY-1, "]"),5],
                                               modeloutput$summary[paste0("mu[", NY-1, "]"),5],
                                               modeloutput$summary[paste0("m[", NY-1, "]"),5],
                                               rep(NA, 4)),
                                 old.2.5 = c(modeloutput$summary[paste0("R[",NY-1,"]"),3],
                                             modeloutput$summary[paste0("B[", NY-1, "]"),3],
                                             modeloutput$summary[paste0("mu[", NY-1, "]"),3],
                                             modeloutput$summary[paste0("m[", NY-1, "]"),3],
                                             rep(NA, 4)),
                                 old.95 = c(modeloutput$summary[paste0("R[",NY-1,"]"),7],
                                            modeloutput$summary[paste0("B[", NY-1, "]"),7],
                                            modeloutput$summary[paste0("mu[", NY-1, "]"),7],
                                            modeloutput$summary[paste0("m[", NY-1, "]"),7],
                                            rep(NA, 4)),
                                 new.value = c(modeloutput$summary[paste0("R[", NY, "]"),5],  
                                               modeloutput$summary[paste0("B[", NY, "]"),5],
                                               NA,
                                               modeloutput$summary[paste0("m[", NY, "]"),5],
                                               rep(NA, 4)),
                                 new.2.5 = c(modeloutput$summary[paste0("R[",NY,"]"),3],
                                             modeloutput$summary[paste0("B[", NY, "]"),3],
                                             NA,
                                             modeloutput$summary[paste0("m[", NY, "]"),3],
                                             rep(NA, 4)),
                                 new.95 = c(modeloutput$summary[paste0("R[",NY,"]"),7],
                                            modeloutput$summary[paste0("B[", NY, "]"),7],
                                            NA,
                                            modeloutput$summary[paste0("m[", NY, "]"),7],
                                            rep(NA, 4)),
                                 const.value = c(rep(NA,4),
                                                 median(modeloutput$sims.matrix[,paste0("q")]),
                                                 median(modeloutput$sims.matrix[,paste0("K")]),
                                                 median(modeloutput$sims.matrix[,paste0("sigma")]),
                                                 median(modeloutput$sims.matrix[,paste0("S")])),
                                 long.term.median = c(median(modeloutput$sims.matrix[,paste0("R[", 1:(NY-1), "]")]),
                                                      median(modeloutput$sims.matrix[,paste0("B[", 1:(NY-1), "]")]),
                                                      NA,
                                                      median(modeloutput$sims.matrix[,paste0("m[", 1:(NY-1), "]")]),
                                                      rep(NA,4)),
                                 LTM.years = c(rep(paste(years$minyear[years$area==area], "-", years$maxyear[years$area==area]-1), 4), rep(NA, 4))  )
  
  names(summary.stats) <- c("description", "variable", 
                               paste0(yrs[NY-1], "_median"), paste0(yrs[NY-1], "_2.5_CI"), paste0(yrs[NY-1], "_95_CI"),
                               paste0(yrs[NY], "_median"), paste0(yrs[NY], "_2.5_CI"), paste0(yrs[NY], "_95_CI"), "constants",
                               "long_term_median", "LTM_years")
  
  write.csv(summary.stats, paste(direct, assessmentyear, "/Assessment/Data/Model/SPA", area, "/summary stats_", area, "_", surveyyear, ".csv",sep=""),row.names = F)
 
  summary.stats.temporal <- data.frame(median.comm.biomass.est = c(apply(modeloutput$sims.matrix[,paste0("B[", 1:NY, "]")], 2, FUN=median)), 
                              median.exploit.rate.est = c(NA, apply(modeloutput$sims.matrix[,paste0("mu[", 1:(NY-1), "]")], 2, FUN=median)), 
                              median.survival.rate.est = c(apply(exp(-modeloutput$sims.matrix[,paste0("m[", 1:NY, "]")]), 2, FUN=median)), 
                              year = years$minyear[years$area==area]:years$maxyear[years$area==area],
                              area = rep(area, NY))
                      
  write.csv(summary.stats.temporal, paste(direct , assessmentyear, "/Assessment/Data/Model/SPA", area, "/summary stats temporal_", area, "_", surveyyear, ".csv",sep=""),row.names = F)
  
  stats.output <- list("summary.stats" = summary.stats, "summary.stats.temporal" = summary.stats.temporal)
  return(stats.output)
  
}

# BoF.model.stats(area = "3", year=2017, direct = "Y:/INSHORE SCALLOP/BoF/", RDatafile = "SPA3_Model_2017")
# BoF.model.stats(area = "1A", year=2017, direct = "Y:/INSHORE SCALLOP/BoF/", RDatafile = "SPA1A_Model_2017")
# BoF.model.stats(area = "1B", year=2017, direct = "Y:/INSHORE SCALLOP/BoF/", RDatafile = "SPA1B_Model_2017")
# BoF.model.stats(area = "4and5", year=2017, direct = "Y:/INSHORE SCALLOP/BoF/", RDatafile = "SPA4_Model_2017")
# BoF.model.stats(area = "6", year=2017, direct = "Y:/INSHORE SCALLOP/BoF/", RDatafile = "SPA6_Model_2017")
# 
# require(readxl)
# 
# landings <- read_xlsx("Y:/INSHORE SCALLOP/BoF/Bay of Fundy Scallop Assessment Summary Data_2018-03-20.xlsx", sheet="Landings")
# # landings <- ddply(.data=landings, .(year, area),
# #                   summarize,
# #                   landings=sum(landings))
# # View(landings)
# 
# biomass <- read_xlsx("Y:/INSHORE SCALLOP/BoF/Bay of Fundy Scallop Assessment Summary Data_2018-03-20.xlsx", sheet="Commercial Biomass Estimates")
# 
# exploit <- read_xlsx("Y:/INSHORE SCALLOP/BoF/Bay of Fundy Scallop Assessment Summary Data_2018-03-20.xlsx", sheet="Exploitation rates")
# 
# require(ggplot2)
# ggplot() + geom_point(data=landings, aes(Year, as.numeric(`Landings (t)`))) + facet_wrap(~Area, scales="free") + theme_bw()
# 
# ggplot() + geom_point(data=biomass, aes(Year, as.numeric(`Median Commercial Biomass Estimate (t)`))) + facet_wrap(~Area, scales="free") + theme_bw()
# 
# ggplot() + geom_point(data=exploit, aes(Year, as.numeric(`Median Exploitation Rate Estimate`))) + facet_wrap(~Area, scales="free") + theme_bw()
