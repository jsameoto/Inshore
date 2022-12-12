#########  This script stiches together all the model output and produces the figures for the SSR and it generally where you want to go to
#########  produce any figures from the models for SFA 29 and provides the data needed for the update document...
# DK Feb 2018, this is a revision of an older script LN had developed.
#Updated JS March 2020 during Covid-19 pandemic.. 

library(tidyverse)
library(lubridate)

#define 
yr <- 2022 # This should be set to the year after the year of the last survey.  e.g. if 2018 that means you are using the 2017 survey.
survey.year <- 2021

path.directory <- paste0("Y:/Inshore/SFA29/")

paste0(path.directory,yr,"/Assessment/Data/Model/")


# The LRP and USR's
ref.points <- data.frame(area = paste0("SFA29",rep(LETTERS[2:4],2)),
                         ref.pt = c(c(1.12,1.41,1.3),c(1.12,1.41,1.3)*2),
                         type = c(rep("LRP",3),rep("USR",3)))

# The LRP and USR's and Dmsy for each area
ref.points.2 <- data.frame(area = paste0("SFA29",rep(LETTERS[2:4],3)),
                         ref.pt = c(c(1.12,1.41,1.3),c(1.12,1.41,1.3)*2, c(3.75,4.68, 4.32)),
                         type = c(rep("LRP",3),rep("USR",3), rep("Dmsy",3)))

# Bring in model results, which initially 
mod.summary <- NULL
#mod.res <- NULL
for(i in 1:4)
{
  area <- LETTERS[i]
  # get the catch and exploitation summaries
  mod.summary[[i]] <- read.csv(paste0(path.directory,yr,"/Assessment/Data/Model/SFA29",area,"/SFA29.",area,".model.results.summary.",survey.year,".csv"))
  mod.summary[[i]]$area <- paste0("SFA29",area)
  # Bring in the full model results...
  #load(paste0(getwd(),"/SFA29",area,"_results/SFA_29",area,"_",(survey.year),".RData"))
}
mod.summary <- do.call("rbind",mod.summary)
mod.summary$Habitat <- factor(mod.summary$Habitat, levels = c("Low", "Med", "High"))
# make 0's NA's so they don't plot or contribute to calcs (Don't do this if you want them to be plotted or contribute!!.
#mod.summary$Catch[mod.summary$Catch == 0] <- NA
#mod.summary$CPUE[mod.summary$CPUE == 0] <- NA
#mod.summary$mu[mod.summary$mu == 0] <- NA


######## Now get a summary of the results by area
# The biomass estimates
current.year.bm <- mod.summary[mod.summary$Year == (survey.year),c("Biomass","Habitat","area")]
current.year.bm$Biomass <- signif(current.year.bm$Biomass,digits=3)
last.year.bm <- mod.summary[mod.summary$Year == (survey.year-1),c("Biomass","Habitat","area")]
last.year.bm$Biomass <- signif(last.year.bm$Biomass,digits=3)

# The explotation rates
current.year.exploit <- mod.summary[mod.summary$Year == (survey.year),c("mu","Habitat","area")]
current.year.exploit$mu <- signif(current.year.exploit$mu,digits=2)
last.year.exploit <- mod.summary[mod.summary$Year == (survey.year-1),c("mu","Habitat","area")]
last.year.exploit$mu <- signif(last.year.exploit$mu,digits=2)

# The Catch by area
current.year.catch <- mod.summary[mod.summary$Year == (survey.year),c("Catch","Habitat","area")]
current.year.catch$Catch <- signif(current.year.catch$Catch,digits=2)
last.year.catch <- mod.summary[mod.summary$Year == (survey.year-1),c("Catch","Habitat","area")]
last.year.catch$Catch <- signif(last.year.catch$Catch,digits=2)

# CPUE by area
current.year.CPUE <- mod.summary[mod.summary$Year == (survey.year),c("CPUE","Habitat","area")]
current.year.CPUE$CPUE <- signif(current.year.CPUE$CPUE,digits=2)
last.year.CPUE <- mod.summary[mod.summary$Year == (survey.year-1),c("CPUE","Habitat","area")]
last.year.CPUE$CPUE <- signif(last.year.CPUE$CPUE,digits=2)

# natural mortality by area
current.year.nat.m <- mod.summary[mod.summary$Year == (survey.year),c("nat.m","Habitat","area")]
current.year.nat.m$nat.m <- signif(current.year.nat.m$nat.m,digits=2)
last.year.nat.m <- mod.summary[mod.summary$Year == (survey.year-1),c("nat.m","Habitat","area")]
last.year.nat.m$nat.m <- signif(last.year.nat.m$nat.m,digits=2)
# The five year mean natural mortality by area/strata
five.year.mean.m <- aggregate(nat.m~Habitat+area,mod.summary[mod.summary$Year %in% (yr-5):(survey.year),],mean)
# proportional natural mortality...
mod.summary$nat.m.prop <- 1-exp(-mod.summary$nat.m)

# Finally the biomass density
mod.summary$BM.dens <- mod.summary$Biomass/mod.summary$size.area
current.year.bmd <- mod.summary[mod.summary$Year == (survey.year),c("BM.dens","Habitat","area")]
current.year.bmd$BM.dens <- signif(current.year.bmd$BM.dens,digits=3)
last.year.bmd <- mod.summary[mod.summary$Year == (survey.year-1),c("BM.dens","Habitat","area")]
last.year.bmd$BM.dens <- signif(last.year.bmd$BM.dens,digits=3)
# BMD for last 3 years
threeyear.bmd <- mod.summary[mod.summary$Year > (yr-4),c("Year","BM.dens","Habitat","area")]
threeyear.bmd$BM.dens <- signif(threeyear.bmd$BM.dens,digits=3)


## THIS CODE MAKES 2001 and 2003 0's NULL for SFA29A to match previous doc plots
#mod.summaryA <- mod.summary [mod.summary$area == "SFA29A",]
#mod.summaryAearly <- mod.summaryA [mod.summaryA$Year < 2005,]
#mod.summaryAlate <- mod.summaryA [mod.summaryA$Year > 2004,]
#mod.summaryAearly$Catch[mod.summaryAearly$Catch == 0] <- NA
#mod.summaryAearly$CPUE[mod.summaryAearly$CPUE == 0] <- NA
#mod.summaryAearly$mu[mod.summaryAearly$mu == 0] <- NA
#mod.summaryA2 <- rbind (mod.summaryAearly, mod.summaryAlate)
#mod.summaryBCD <- mod.summary [!mod.summary$area == "SFA29A",]
#mod.summary2 <- rbind (mod.summaryA2, mod.summaryBCD)
# remakes mod.summary to include change
#mod.summary <- mod.summary2

#Set colour/linetype/shape for plots
colr <- c('firebrick3', 'darkgrey', 'darkblue') #c("black","red","green") #darkgrey
line.type <- 1:3
symbs <- 15:17

#Bh - Commercial biomass in t
windows(8,7)
ggplot(mod.summary, aes(Year,Biomass,colour = Habitat)) + geom_line(aes(group=Habitat, linetype = Habitat, color = Habitat), size = 0.56) + 
         geom_point(aes(group=Habitat, shape = Habitat, color = Habitat), size = 1.5) + facet_wrap(~area) +
  scale_color_manual(values=colr, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low"))+
         xlab("Year") + ylab("Biomass (meats t)") + ggtitle("SFA29 Modelled Biomass") + theme_bw() +
  theme(legend.title = element_blank(),legend.text = element_text(size=14), panel.grid=element_blank()) 

#biomass density all A-D, no ref pts.
windows(8,7)
ggplot(mod.summary,aes(Year,BM.dens,colour = Habitat)) + geom_line(aes(group=Habitat, linetype = Habitat, color = Habitat), size = 0.56) + 
  geom_point(aes(group=Habitat, shape = Habitat, color = Habitat), size = 1.5) + facet_wrap(~area) +
  scale_color_manual(values=colr, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low"))+
  xlab("Year") + ylab("Biomass density (meats t/km^2)") + ggtitle("SFA29 Modelled Biomass Density") + theme_bw() +
  theme(legend.title = element_blank(),legend.text = element_text(size=14), panel.grid=element_blank()) 


# Biomass density B,C,D, with ref pts.
windows(11,7)
ggplot(mod.summary[mod.summary$Habitat=="High" & mod.summary$area != "SFA29A",],aes(Year,BM.dens,colour = Habitat)) + 
  geom_line(size = 0.7,linetype=1,colour='firebrick3') + geom_point(shape=3,colour='firebrick3', size = 3) + facet_wrap(~area) +
  geom_hline(aes(yintercept = ref.pt),ref.points,linetype=c(rep("solid",3), rep("dashed",3)), colour = c(rep("red",3),rep("black",3))) +
  xlab("Year") + ylab(expression (paste("Biomass density (meats t/ ", km^{2}, ")"))) + ggtitle("SFA29 Modelled Biomass Density") +
  theme_bw() + theme(panel.grid=element_blank())


# Biomass density B,C,D, with ref pts. INCLUDING Dmsy
windows(11,7)
ggplot(mod.summary[mod.summary$Habitat=="High" & mod.summary$area != "SFA29A",],aes(Year,BM.dens,colour = Habitat)) + 
  geom_line(size = 0.7,linetype=1,colour='firebrick3') + geom_point(shape=3,colour='firebrick3', size = 3) + facet_wrap(~area) +
  geom_hline(aes(yintercept = ref.pt),ref.points.2,linetype=c(rep("solid",3), rep("dashed",3), rep("dotdash",3)), colour = c(rep("red",3),rep("black",3),rep("blue",3))) +
  xlab("Year") + ylab(expression (paste("Biomass density (meats t/ ", km^{2}, ")"))) + ggtitle("SFA29 Modelled Biomass Density") +
  theme_bw() + theme(panel.grid=element_blank())


# Biomass density A,B,C,D, with ref pts - LRP, USR, Dmsy.
# easier to get the data we want outside the function call...
dat.all <- mod.summary[mod.summary$Habitat=="High" | mod.summary$area == "SFA29A" ,]
dat.all <- dat.all[dat.all$Habitat != "Low",]

#Add French names for Subareas
dat.all <- dat.all %>% 
  mutate(area_FR = case_when(area == "SFA29A" ~ "ZPP29A",
                             area == "SFA29B" ~ "ZPP29B",
                             area == "SFA29C" ~ "ZPP29C",
                             area == "SFA29D" ~ "ZPP29D"))

ref.points <- ref.points %>% 
  mutate(area_FR = case_when(area == "SFA29A" ~ "ZPP29A",
                             area == "SFA29B" ~ "ZPP29B",
                             area == "SFA29C" ~ "ZPP29C",
                             area == "SFA29D" ~ "ZPP29D"))

ref.points.2 <- ref.points.2 %>% 
  mutate(area_FR = case_when(area == "SFA29A" ~ "ZPP29A",
                             area == "SFA29B" ~ "ZPP29B",
                             area == "SFA29C" ~ "ZPP29C",
                             area == "SFA29D" ~ "ZPP29D"))

#Create for DMSY, USR, LRP labels
anno <- data.frame(Year = c(2017.1,2017.1,2017.1),
                   BM.dens = c(4.8,2.3,1.0),
                   lab = c("Dmsy","USR","LRP"),
                   area = c("SFA29D","SFA29D","SFA29D"), 
                   area_FR = c("ZPP29D","ZPP29D","ZPP29D"),
                   Habitat = factor(c("High","High","High"),levels = c("Low" , "Med" , "High")))

#PLOT
ggplot(dat.all,aes(Year,BM.dens,colour = Habitat)) + 
  geom_hline(aes(yintercept = ref.pt),ref.points.2,linetype=c(rep("solid",3), rep("dashed",3), rep("dotdash",3)), colour = c(rep("red",3),rep("black",3),rep("blue",3))) +
  geom_line(aes(Year,BM.dens,colour = Habitat,linetype=Habitat)) + 
  geom_point(aes(Year,BM.dens,colour = Habitat,shape=Habitat))+
  facet_wrap(~area,nrow=1) +
  xlab("Year") + ylab(expression (paste("Biomass density (meats t/ ", km^{2}, ")"))) + #ggtitle("SFA29 Modelled Biomass Density") +
  theme_bw() + theme(panel.grid=element_blank()) +scale_color_manual(values=colr, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low"))+
  theme_bw()+
  theme(legend.position="none", aspect.ratio=1.25) + 
  geom_text(data = anno,parse = TRUE, vjust = c(0.05, 0.7, 0.7), label = c(as.character(expression("D"["MSY"])),"USR","LRP"), col=c('blue','black','red'))

#save
ggsave(filename = paste0(path.directory,"/",yr,"/Assessment/Figures/Model/Commercial_Biomass_Density",survey.year,".png"), plot = last_plot(), scale = 2.5, width =11, height = 4, dpi = 300, units = "cm", limitsize = TRUE)

# EN FRANCAIS!!! - Check translation for DMSY LRP, USR
#MSY = RMD - un rendement maximal durable
#LRP = PRI - les points de référence inférieurs
#USR = PRS - les points de référence supérieurs

ggplot(dat.all,aes(Year,BM.dens,colour = Habitat)) + 
  geom_hline(aes(yintercept = ref.pt),ref.points.2,linetype=c(rep("solid",3), rep("dashed",3), rep("dotdash",3)), colour = c(rep("red",3),rep("black",3),rep("blue",3))) +
  geom_line(aes(Year,BM.dens,colour = Habitat,linetype=Habitat)) + 
  geom_point(aes(Year,BM.dens,colour = Habitat,shape=Habitat))+
  facet_wrap(~area_FR,nrow=1) +
  xlab("Année") + ylab(expression (paste("Densité de la biomasse (chairs, t/ ", km^{2}, ")"))) + #ggtitle("SFA29 Modelled Biomass Density") +
  theme_bw() + theme(panel.grid=element_blank()) +
  scale_color_manual(values=colr, breaks = c("High", "Med", "Low"),labels = c("High"="Elevé", "Med"="Moyen", "low"="Faible")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low"),labels = c("High"="Elevé", "Med"="Moyen", "low"="Faible")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"),labels = c("High"="Elevé", "Med"="Moyen", "low"="Faible"))+
  theme_bw()+
  theme(legend.position="none", aspect.ratio=1.25) + 
  geom_text(data = anno,parse = TRUE, vjust = c(0.05, 0.7, 0.7), label = c(as.character(expression("D"["RMD"])),"PRS","PRI"), col=c('blue','black','red'))

#save
ggsave(filename = paste0(path.directory,"/",yr,"/Assessment/Figures/Model/Commercial_Biomass_Density",survey.year,"_FR.png"), plot = last_plot(), scale = 2.5, width =11, height = 4, dpi = 300, units = "cm", limitsize = TRUE)

   

#catch
windows(8,7)
ggplot(mod.summary,aes(Year,Catch,colour = Habitat)) + geom_line(aes(group=Habitat, linetype = Habitat, color = Habitat), size = 0.75) + 
  geom_point(aes(group=Habitat, shape = Habitat, color = Habitat)) + facet_wrap(~area) +
  scale_color_manual(values=colr, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  xlab("Year") + ylab("Catch (meats kg)") + ggtitle("SFA29 Modelled Catch") + theme_bw() +
  theme(legend.title = element_blank(),legend.text = element_text(size=14), panel.grid=element_blank()) 


#exploitation
# remove 2001 because it is not indicative based on time series
windows(8,7)
ggplot(mod.summary[mod.summary$Year > 2001,],aes(Year,mu,colour = Habitat)) + geom_line(aes(group=Habitat, linetype = Habitat, color = Habitat), size = 0.75) + 
  geom_point(aes(group=Habitat, shape = Habitat, color = Habitat)) + facet_wrap(~area) +
  scale_color_manual(values=colr, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  xlab("Year") + ylab("Exploitation") + ggtitle("SFA29 Exploitation") + theme_bw() +
  theme(legend.title = element_blank(),legend.text = element_text(size=14), panel.grid=element_blank()) 

#exploitation (legend on plot for SSR)
windows(6,5)
ggplot(mod.summary[mod.summary$Year > 2001,],aes(Year,mu,colour = Habitat)) + geom_line(aes(group=Habitat, linetype = Habitat, color = Habitat), size = 0.75) + 
  geom_point(aes(group=Habitat, shape = Habitat, color = Habitat)) + facet_wrap(~area) +
  scale_color_manual(values=colr, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  xlab("Year") + ylab("Exploitation") + ggtitle("SFA29 Exploitation") + theme_bw() +
  theme(legend.title = element_blank(),legend.text = element_text(size=11), panel.grid=element_blank(), legend.position = c(0.9, 0.86))

#exploitation (legend on plot for SSR) A- Med, B,C,D - High only 
#Saved in 2020 as "Model_ExploitationForDoc_v2CSAS.png"
explot.data.plot <- rbind(mod.summary[mod.summary$area%in%c('SFA29A')&mod.summary$Habitat=="Med",], mod.summary[mod.summary$area%in%c('SFA29B','SFA29C','SFA29D')&mod.summary$Habita=="High",])

windows(6,5)
ggplot(explot.data.plot[explot.data.plot$Year > 2001,],aes(Year,mu)) + geom_line(aes(group=Habitat, linetype = Habitat, color = Habitat), size = 0.75) + 
  geom_point(aes(group=Habitat, shape = Habitat, color = Habitat)) + facet_wrap(~area,nrow=1) +
  scale_color_manual(values=colr, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  xlab("Year") + ylab("Exploitation") + #ggtitle("SFA29 Exploitation")
  theme_bw() +
  theme(legend.position="none", aspect.ratio = 1.25)
  #theme(legend.title = element_blank(),legend.text = element_text(size=11), panel.grid=element_blank(), legend.position = c(0.9, 0.86), aspect.ratio=1.25)

#save
ggsave(filename = paste0(path.directory,"/",yr,"/Assessment/Figures/Model/Model_Exploitation",survey.year,".png"), plot = last_plot(), scale = 2.5, width =11, height = 4, dpi = 300, units = "cm", limitsize = TRUE)

##EN FRANCAIS
#mod.summary.FR <- mod.summary
#mod.summary.FR$area <- factor(mod.summary.FR$area,levels=c("SFA29A","SFA29B","SFA29C","SFA29D"),
#                       labels=c("ZPP29A","ZPP29B","ZPP29C","ZPP29D"))
#mod.summary.FR$Habitat <- factor(mod.summary.FR$Habitat,levels=c("Low","Med","High"),
#                             labels=c("Faible","Moyen","Elevé"))

mod.summary <- mod.summary %>% 
  mutate(area_FR = case_when(area == "SFA29A" ~ "ZPP29A",
                             area == "SFA29B" ~ "ZPP29B",
                             area == "SFA29C" ~ "ZPP29C",
                             area == "SFA29D" ~ "ZPP29D"))
explot.data.plot <- rbind(mod.summary[mod.summary$area%in%c('SFA29A')&mod.summary$Habitat=="Med",], mod.summary[mod.summary$area%in%c('SFA29B','SFA29C','SFA29D')&mod.summary$Habita=="High",])

windows(6,5)
ggplot(explot.data.plot[explot.data.plot$Year > 2001,],aes(Year,mu,colour = Habitat)) + geom_line(aes(group=Habitat, linetype = Habitat, color = Habitat), size = 0.75) + 
  geom_point(aes(group=Habitat, shape = Habitat, color = Habitat)) + facet_wrap(~area_FR, nrow=1) +
  scale_color_manual(values=colr, breaks = c("High", "Med", "Low"),labels = c("High"="Elevé", "Med"="Moyen", "low"="Faible")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low"),labels = c("High"="Elevé", "Med"="Moyen", "low"="Faible")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"),labels = c("High"="Elevé", "Med"="Moyen", "low"="Faible"))+
  xlab("Année") + ylab("Exploitation") + 
  theme_bw() +
  theme(legend.position="none", aspect.ratio = 1.25)
  #theme(legend.title = element_blank(),legend.text = element_text(size=11), panel.grid=element_blank(), legend.position = c(0.9, 0.86))

#save
ggsave(filename = paste0(path.directory,"/",yr,"/Assessment/Figures/Model/Model_Exploitation",survey.year,"_FR.png"), plot = last_plot(), scale = 2.5, width =11, height = 4, dpi = 300, units = "cm", limitsize = TRUE)

#CPUE
windows(8,7)
ggplot(mod.summary,aes(Year,CPUE,colour = Habitat))  + geom_line(aes(group=Habitat, linetype = Habitat, color = Habitat), size = 0.75) + 
  geom_point(aes(group=Habitat, shape = Habitat, color = Habitat)) + facet_wrap(~area) +
  scale_color_manual(values=colr, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  xlab("Year") + ylab("CPUE (kg/hr)") + ggtitle("SFA29 Catch per Unit Effort") + theme_bw() +
  theme(legend.title = element_blank(),legend.text = element_text(size=14), panel.grid=element_blank()) 

#m - natural mortality
windows(8,7)
ggplot(mod.summary,aes(Year,nat.m,colour = Habitat))  + geom_line(aes(group=Habitat, linetype = Habitat, color = Habitat), size = 0.75) + 
  geom_point(aes(group=Habitat, shape = Habitat, color = Habitat)) + facet_wrap(~area) +
  scale_color_manual(values=colr, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  xlab("Year") + ylab("Natural mortality (Instantaneous)") + ggtitle("SFA29 Natural mortality") + theme_bw() +
  theme(legend.title = element_blank(),legend.text = element_text(size=14), panel.grid=element_blank()) 

#m - natural mortality on proportion scale, DK suggests we use this...
windows(8,7)
ggplot(mod.summary,aes(Year,nat.m.prop,colour = Habitat))  + geom_line(aes(group=Habitat, linetype = Habitat, color = Habitat), size = 0.75) + 
  geom_point(aes(group=Habitat, shape = Habitat, color = Habitat)) + facet_wrap(~area) +
  scale_color_manual(values=colr, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"),labels = c("high"="High", "Med"="Medium", "low"="Low")) +
  xlab("Year") + ylab("Natural mortality (proportional)") + ggtitle("SFA29 Natural mortality") + theme_bw() +
  theme(legend.title = element_blank(),legend.text = element_text(size=14), panel.grid=element_blank()) 



##############EXTRA PLOTS EXTRA PLOTS###
#m - natural mortality of high (with 5-year mean)
dat.all <- mod.summary[mod.summary$Habitat=="High" | mod.summary$area == "SFA29A" ,]
dat.all <- dat.all[dat.all$Habitat != "Low",]
# 5-year mean for each area
library (plyr)
fyr_mean.instm <- ddply (dat.all[dat.all$Year >=  max(dat.all$Year)-4,],. (area), summarize, mean = mean (nat.m))
lt_median.instm <- ddply (dat.all[dat.all$Year < max(dat.all$Year),],. (area), summarize, ltmed = median (nat.m))
# for proportional scale
fyr_mean.propm <- ddply (dat.all[dat.all$Year >=  max(dat.all$Year)-4,],. (area), summarize, mean = mean (nat.m.prop))
lt_median.propm <- ddply (dat.all[dat.all$Year < max(dat.all$Year),],. (area), summarize, ltmed = median (nat.m.prop))

#Instantaneous mortality plot with 5 year mean and long term mean 
windows(11,7)
ggplot(dat.all,aes(Year,nat.m,colour = Habitat, linetype = Habitat))  + geom_line( size = 0.75) + 
  geom_point(aes(group=Habitat, shape = Habitat, color = Habitat)) + facet_wrap(~area, nrow=1) +
  scale_color_manual(values=colr, breaks = c("High", "Med", "Low")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"))+
  xlab("Year") + ylab("Natural mortality (Instantaneous)") + #ggtitle("SFA29 Natural mortality") + 
  theme_bw() + theme(legend.position = "none", aspect.ratio=1.25) + 
  geom_segment(aes(x=min(dat.all$Year), xend=max(dat.all$Year)-1,y=ltmed,yend=ltmed),data=lt_median.instm,linetype=c("solid"), colour = c("black")) +
  geom_segment(aes(x=max(dat.all$Year)-4, xend=max(dat.all$Year), y= mean,yend= mean),data = fyr_mean.instm,linetype=c("dashed"), colour = c("black"))

#save
ggsave(filename = paste0(path.directory,"/",yr,"/Assessment/Figures/Model/NatMort_Instantaneous",survey.year,".png"), plot = last_plot(), scale = 2.5, width =11, height = 4, dpi = 300, units = "cm", limitsize = TRUE)

  
#Proportional mortality plot with 5 year mean and long term mean 
windows(11,7)
ggplot(dat.all,aes(Year,nat.m,colour = Habitat, linetype = Habitat))  + geom_line( size = 0.75) + 
  geom_point(aes(group=Habitat, shape = Habitat, color = Habitat)) + facet_wrap(~area, nrow=1) +
  scale_color_manual(values=colr, breaks = c("High", "Med", "Low")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"))+
  xlab("Year") + ylab("Natural mortality (Proportion)") + #ggtitle("SFA29 Natural mortality") + 
  theme_bw() + theme(legend.position = "none", aspect.ratio=1.25) + 
  geom_segment(aes(x=min(dat.all$Year), xend=max(dat.all$Year)-1,y=ltmed,yend=ltmed),data=lt_median.propm,linetype=c("solid"), colour = c("black")) +
  geom_segment(aes(x=max(dat.all$Year)-4, xend=max(dat.all$Year), y= mean,yend= mean),data = fyr_mean.propm,linetype=c("dashed"), colour = c("black"))




# EN FRANCAIS - Instantaneous mortality plot with 5 year mean and long term mean
dat.all$area.fr <- factor(dat.all$area,levels=c("SFA29A","SFA29B","SFA29C","SFA29D"),
                       labels=c("ZPP29A","ZPP29B","ZPP29C","ZPP29D"))

fyr_mean.instm$area.fr <- factor(fyr_mean.instm$area,levels=c("SFA29A","SFA29B","SFA29C","SFA29D"),
                       labels=c("ZPP29A","ZPP29B","ZPP29C","ZPP29D"))
lt_median.instm$area.fr <- factor(lt_median.instm$area,levels=c("SFA29A","SFA29B","SFA29C","SFA29D"),
                       labels=c("ZPP29A","ZPP29B","ZPP29C","ZPP29D"))

fyr_mean.propm$area.fr <- factor(fyr_mean.propm$area,levels=c("SFA29A","SFA29B","SFA29C","SFA29D"),
                                 labels=c("ZPP29A","ZPP29B","ZPP29C","ZPP29D"))
lt_median.propm$area.fr <- factor(lt_median.propm$area,levels=c("SFA29A","SFA29B","SFA29C","SFA29D"),
                                  labels=c("ZPP29A","ZPP29B","ZPP29C","ZPP29D"))


#Instantaneous mortality plot with 5 year mean and long term mean 
windows(11,7)
ggplot(dat.all,aes(Year,nat.m,colour = Habitat, linetype = Habitat))  + geom_line( size = 0.75) + 
  geom_point(aes(group=Habitat, shape = Habitat, color = Habitat)) + facet_wrap(~area.fr, nrow=1) +
  scale_color_manual(values=colr, breaks = c("High", "Med", "Low")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"))+
  xlab("Année") + ylab("Mortalité naturelle (instantantané)") + #ggtitle("SFA29 Natural mortality") + 
  theme_bw() + theme(legend.position = "none", aspect.ratio=1.25) + 
  geom_segment(aes(x=min(dat.all$Year), xend=max(dat.all$Year)-1,y=ltmed,yend=ltmed),data=lt_median.instm,linetype=c("solid"), colour = c("black")) +
  geom_segment(aes(x=max(dat.all$Year)-4, xend=max(dat.all$Year), y= mean,yend= mean),data = fyr_mean.instm,linetype=c("dashed"), colour = c("black"))

ggsave(filename = paste0(path.directory,"/",yr,"/Assessment/Figures/Model/NatMort_Instantaneous",survey.year,"_FR.png"), plot = last_plot(), scale = 2.5, width =11, height = 4, dpi = 300, units = "cm", limitsize = TRUE)

#Proportional mortality plot with 5 year mean and long term mean 
windows(11,7)
ggplot(dat.all,aes(Year,nat.m,colour = Habitat, linetype = Habitat))  + geom_line( size = 0.75) + 
  geom_point(aes(group=Habitat, shape = Habitat, color = Habitat)) + facet_wrap(~area.fr, nrow=1) +
  scale_color_manual(values=colr, breaks = c("High", "Med", "Low")) +
  scale_linetype_manual(values = line.type, breaks = c("High", "Med", "Low")) +
  scale_shape_manual(values = symbs, breaks = c("High", "Med", "Low"))+
  xlab("Année") + ylab("Mortalité naturelle (proportionnel)") + #ggtitle("SFA29 Natural mortality") + 
  theme_bw() + theme(legend.position = "none", aspect.ratio=1.25) + 
  geom_segment(aes(x=min(dat.all$Year), xend=max(dat.all$Year)-1,y=ltmed,yend=ltmed),data=lt_median.propm,linetype=c("solid"), colour = c("black")) +
  geom_segment(aes(x=max(dat.all$Year)-4, xend=max(dat.all$Year), y= mean,yend= mean),data = fyr_mean.propm,linetype=c("dashed"), colour = c("black"))




## END###


############################
# extra 


# Exploitation by biomass
windows(11,11)
ggplot(dat.all,aes(mu,Biomass,colour = Habitat))  + #geom_line(aes(group=Habitat, linetype = Habitat, color = Habitat), size = 0.75) + 
  geom_point(aes(group=Habitat, shape = Habitat, color = Habitat)) + facet_wrap(~area) +
  geom_path ()+
  #geom_text(aes(label=ifelse (Year>2013, Year, "")), hjust=-0.3,vjust=-0.3, colour = "black") +
  scale_color_manual(values = c("red", "green")) + scale_shape_manual(values = c(2,3)) +
  xlab("Exploitation") + ylab("Biomass") + ggtitle("SFA29 Biomass and Exploitation") + theme_bw() +
  theme(legend.title = element_blank(),legend.text = element_text(size=14), panel.grid=element_blank()) 


# Biomass density A,B,C,D, with ref pts. (WITH DMSY)
# easier to get the data we want outside the function call...
dat.all <- mod.summary[mod.summary$Habitat=="High" | mod.summary$area == "SFA29A" ,]
dat.all <- dat.all[dat.all$Habitat != "Low",]

ref.pointsDMSY <- data.frame(area = paste0("SFA29",rep(LETTERS[2:4],3)),
                         ref.pt = c(c(1.12,1.41,1.3),c(1.12,1.41,1.3)*2,c(3.75,4.68,4.32)),
                         type = c(rep("LRP",3),rep("USR",3),rep("DMSY",3)))

windows(11,7)
ggplot(dat.all,aes(Year,BM.dens,colour = Habitat)) + 
  geom_line(aes(Year,BM.dens,colour = Habitat,linetype=Habitat),size = 0.7) + 
  geom_point(aes(Year,BM.dens,colour = Habitat,shape=Habitat)) + facet_wrap(~area,nrow=1) +
  geom_hline(aes(yintercept = ref.pt),ref.pointsDMSY,linetype=rep(c("solid","dashed","dotdash"),3), colour = rep(c("red","black","dodgerblue2"),3)) +
  xlab("Year") + ylab(expression (paste("Biomass density (meats t/ ", km^{2}, ")"))) + #ggtitle("SFA29 Modelled Biomass Density") +
  theme_bw() + theme(panel.grid=element_blank()) + scale_linetype_manual(values = c(2,1)) +
  scale_color_manual(values = c("red", "green")) + scale_shape_manual(values = c(2,3)) +
  theme(legend.position="none")





