##
##  Evaluate relative impact of 2020 growth parameter assumptions for SFA 29W 
##  J.Sameoto Feb 2022
## 

library(ggplot2)
library(tidyverse)

options(stringsAsFactors = FALSE )

area <- "D" #"A", "B", "C" , "D" 
years <- (2001:2021) 
years.mu <- (2001:2021) 

years.2019 <- (2001:2019)  
years.mu.2019 <- (2001:2019) 


ess <- "Y"
path <- ":/Inshore/SFA29/2022/Assessment/Data/Model/"
path2 <- ":/Inshore/SFA29/2020/model/"

model.2019 <- read.csv(paste0(ess,path2,"SFA29",area,"_results/","29",area,"_model_results_summary.csv"))

g.2019 <- read.csv(paste0(ess,path,"SFA29",area,"/SFA29.",area,".model.results.summary.2021.csv"))
g.min <- read.csv(paste0(ess,path,"SFA29",area,"/test_min_gh_for2020/SFA29.",area,".model.results.summary.2021.csv"))
g.max <- read.csv(paste0(ess,path,"SFA29",area,"/test_max_gh_for2020/SFA29.",area,".model.results.summary.2021.csv"))

head(model.2019)
head(g.2019)
str(g.2019)

model.2019$type <- "model.2019"
g.2019$type <- "g.2019"
g.min$type <- "g.min"
g.max$type <- "g.max"

model.dat <- rbind(model.2019, g.2019, g.min, g.max)

#---- comparisons plots ----

ggplot() + 
  geom_point(data=model.dat, aes(x= Year, y=BM.dens, group=type, colour = type)) + 
  geom_line(data=model.dat, aes(x= Year, y=BM.dens, group=type, colour = type)) + 
  ylab("Biomass Density") + 
  xlab("Year") + 
  facet_wrap(~Habitat) 

ggsave("g.impact.biomass.density.png", path=paste0(ess,":/Inshore/SFA29/2022/Assessment/Figures/Model/SFA29",area), dpi=300)   


ggplot() + 
  geom_point(data=model.dat, aes(x= Year, y=CPUE , group=type, colour = type)) + 
  geom_line(data=model.dat, aes(x= Year, y=CPUE , group=type, colour = type)) + 
  ylab("CPUE modelled") + 
  xlab("Year") + 
  facet_wrap(~Habitat) 

ggsave("g.impact.CPUE.png", path=paste0(ess,":/Inshore/SFA29/2022/Assessment/Figures/Model/SFA29",area), dpi=300)   

ggplot() + 
  geom_point(data=model.dat, aes(x= Year, y=Catch , group=type, colour = type)) + 
  geom_line(data=model.dat, aes(x= Year, y=Catch , group=type, colour = type)) + 
  ylab("Catch modelled") + 
  xlab("Year") + 
  facet_wrap(~Habitat) 

ggsave("g.impact.Catch.png", path=paste0(ess,":/Inshore/SFA29/2022/Assessment/Figures/Model/SFA29",area), dpi=300)   

ggplot() + 
  geom_point(data=model.dat, aes(x= Year, y=nat.m , group=type, colour = type)) +
  geom_line(data=model.dat, aes(x= Year, y=nat.m , group=type, colour = type)) + 
  ylab("Mortality") + 
  xlab("Year") + 
  facet_wrap(~Habitat) 

ggsave("g.impact.mortality.png", path=paste0(ess,":/Inshore/SFA29/2022/Assessment/Figures/Model/SFA29",area), dpi=300)   


ggplot() + 
  geom_point(data=model.dat, aes(x= Year, y=Biomass , group=type, colour = type)) +
  geom_line(data=model.dat, aes(x= Year, y=Biomass , group=type, colour = type)) + 
  ylab("Biomass") + 
  xlab("Year") + 
  facet_wrap(~Habitat) 

ggsave("g.impact.Biomass.png", path=paste0(ess,":/Inshore/SFA29/2022/Assessment/Figures/Model/SFA29",area), dpi=300)   


ggplot() + 
  geom_point(data=model.dat, aes(x= Year, y=mu , group=type, colour = type)) + 
  geom_line(data=model.dat, aes(x= Year, y=mu , group=type, colour = type)) + 
  ylab("Exploitation") + 
  xlab("Year") + 
  facet_wrap(~Habitat) 

ggsave("g.impact.exploitation.png", path=paste0(ess,":/Inshore/SFA29/2022/Assessment/Figures/Model/SFA29",area), dpi=300)   





# ---- relative difference in biomass density between min and max growth assumptions ----
diff.B <- model.dat$BM.dens[model.dat$type == "g.max"] - model.dat$BM.dens[model.dat$type == "g.min"] 
diff.B

#difference in 2020 and 2021 
#in 2020 in % 
round(abs((diff.B[length(diff.B)-1]/model.dat$BM.dens[model.dat$type == "g.max"][length(diff.B)-1])*100),2)
#A - 8.77
#B - 12.46
#C - 3.38
#D - 5.1


#in 2021 in % 
round(abs((diff.B[length(diff.B)]/model.dat$BM.dens[model.dat$type == "g.max"][length(diff.B)])*100),2)
#A - 6.88
#B - 6.69
#C - 0.95
#D - < 1



