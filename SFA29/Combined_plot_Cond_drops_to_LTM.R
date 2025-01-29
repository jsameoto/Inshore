#
# PANEL PLOT FOR decision tables if Condition drops to LTM - and commercial growth rate drops by same percent and

#
rm(list=ls(all=T))
options(stringsAsFactors = FALSE)

#PACKAGES:
#required packages and functions:
library(openxlsx)
library(tidyverse)
library(SSModel)
library(ggplot2)
library(SSModeltest) #v 1.0-5;  this is the SSModel package used in BoF, modified to accomodate the SFA29 models.
library(lubridate)
library(tidyverse)
#library(SSModel) #v 1.0-5
source("Y:/Inshore/SFA29/2017/model/SFA29model9-2015.R") #contains the SFA29model model (BUGS) 


funcs <- c("https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/SSModel_predict_summary_median.r")
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}


#DEFINE:
direct <- "Y:/Inshore/SFA29/"
assessmentyear <- 2024 #year in which you are conducting the assessment 
surveyyear <- 2023 
yrs <- 2001:surveyyear 

#Load model indices for current assessment
mod.dat.A <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/Model/SFA29A/SFA29A_ModelData.",surveyyear,".csv"))
strata <- c("low","med")
growth.paras.A <- mod.dat.A %>% filter(Year == max(yrs) & Strata %in% strata) %>% select(Strata, gh) %>% arrange(match(Strata, c("low", "med"))) %>% select(gh)
growth.paras.A 

mod.dat.B <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/Model/SFA29B/SFA29B_ModelData.",surveyyear,".csv"))
strata <- c("low","med","high")
growth.paras.B <- mod.dat.B %>% filter(Year == max(yrs) & Strata %in% strata) %>% select(Strata, gh) %>% arrange(match(Strata, c("low", "med", "high"))) %>% select(gh)
growth.paras.B 

mod.dat.C <-read.csv(paste0(direct,assessmentyear,"/Assessment/Data/Model/SFA29C/SFA29C_ModelData.",surveyyear,".csv"))
strata <- c("low","med","high")
growth.paras.C <- mod.dat.C %>% filter(Year == max(yrs) & Strata %in% strata) %>% select(Strata, gh) %>% arrange(match(Strata, c("low", "med", "high"))) %>% select(gh)
growth.paras.C 

mod.dat.D <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/Model/SFA29D/SFA29D_ModelData.",surveyyear,".csv"))
strata <- c("low","med","high")
growth.paras.D <- mod.dat.D %>% filter(Year == max(yrs) & Strata %in% strata) %>% select(Strata, gh) %>% arrange(match(Strata, c("low", "med", "high"))) %>% select(gh)
growth.paras.D 


#Load model indices - If condition drops to ltm
mod.dat.A.at.ltm <- read.csv(paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SFA29A/DecisionTables_if_Condtion_drops_to_LTM/SFA29A_ModelData.2023_growthrate_condltm.csv"))
strata <- c("low","med")
growth.paras.A.at.ltm <- mod.dat.A.at.ltm %>% filter(Year == max(yrs) & Strata %in% strata) %>% select(Strata, gh) %>% arrange(match(Strata, c("low", "med"))) %>% select(gh)
growth.paras.A.at.ltm 

mod.dat.B.at.ltm <- read.csv(paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SFA29B/DecisionTables_if_Condtion_drops_to_LTM/SFA29B_ModelData.2023_growthrate_condltm.csv"))
strata <- c("low","med","high")
growth.paras.B.at.ltm <- mod.dat.B.at.ltm %>% filter(Year == max(yrs) & Strata %in% strata) %>% select(Strata, gh) %>% arrange(match(Strata, c("low", "med", "high"))) %>% select(gh)
growth.paras.B.at.ltm 


mod.dat.C.at.ltm <- read.csv(paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SFA29C/DecisionTables_if_Condtion_drops_to_LTM/SFA29C_ModelData.2023_growthrate_condltm.csv"))
strata <- c("low","med","high")
growth.paras.C.at.ltm <- mod.dat.C.at.ltm %>% filter(Year == max(yrs) & Strata %in% strata) %>% select(Strata, gh) %>% arrange(match(Strata, c("low", "med", "high"))) %>% select(gh)
growth.paras.C.at.ltm 

mod.dat.D.at.ltm <- read.csv(paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SFA29D/DecisionTables_if_Condtion_drops_to_LTM/SFA29D_ModelData.2023_growthrate_condltm.csv"))
strata <- c("low","med","high")
growth.paras.D.at.ltm <- mod.dat.D.at.ltm %>% filter(Year == max(yrs) & Strata %in% strata) %>% select(Strata, gh) %>% arrange(match(Strata, c("low", "med", "high"))) %>% select(gh)
growth.paras.D.at.ltm 

#Load model file
load(paste0(direct,assessmentyear, "/Assessment/Data/Model/SFA29A/SFA29A.2023.RData"))
mod.res.A <- A.mod.res

load(paste0(direct,assessmentyear, "/Assessment/Data/Model/SFA29B/SFA29B.2023.RData"))
mod.res.B <- B.mod.res

load(paste0(direct,assessmentyear, "/Assessment/Data/Model/SFA29C/SFA29C.2023.RData"))
mod.res.C <- C.mod.res

load(paste0(direct,assessmentyear, "/Assessment/Data/Model/SFA29D/SFA29D.2023.RData"))
mod.res.D <- D.mod.res

#############################################################################################################
#decision tables for each subarea
# Subarea A - For the Condition unchanged from current year (commercial growth rate is from the model object):

decision.current.A <- predict(A.mod.res,exploit=0.1,g.parm=c(growth.paras.A$gh,NA), m.avg = 1)
summary(decision.current.A)
set.seed(10) # set the random number generator if you want to make your results reproducible.
Decision.table.A <- matrix(NA,10,7)
dimnames(Decision.table.A)[[2]] <- names(summary(decision.current.A)$Next.year)
# The decision table for next years fishing season...
for(i in 1:10)
{
  temp<-predict(A.mod.res,exploit=0.01*(i-1), g.parm=c(growth.paras.A$gh,NA))
  Decision.table.A[i,]<-as.vector(unlist(summary(temp)$Next.year))
}
colnames(Decision.table.A) <- c(names(summary(temp)$Next.year))

Decision.table.A.current <-  as.data.frame(Decision.table.A) |> mutate(Table.Type = "Condition unchanged from current year") |> mutate(SUBAREA = "Subarea A") |> mutate(RRP=NA)
Decision.table.A.current

  
#A - For the Condition at LTM
decision.ltm.A <- predict(A.mod.res,exploit=0.1,g.parm=c(growth.paras.A.at.ltm$gh,NA), m.avg = 1)
summary(decision.ltm.A)
set.seed(10) # set the random number generator if you want to make your results reproducible.
Decision.table.A <- matrix(NA,14,7)
dimnames(Decision.table.A)[[2]] <- names(summary(decision.ltm.A)$Next.year)
# The decision table for next years fishing season...
for(i in 1:14)
{
  temp<-predict(A.mod.res,exploit=0.01*(i-1), g.parm=c(growth.paras.A.at.ltm$gh,NA))
  Decision.table.A[i,]<-as.vector(unlist(summary(temp)$Next.year))
}
colnames(Decision.table.A) <- c(names(summary(temp)$Next.year))
Decision.table.A.ltm <-  as.data.frame(Decision.table.A) |> mutate(Table.Type = "Condition at LTM") |> mutate(SUBAREA = "Subarea A") |> mutate(RRP=NA)
Decision.table.A.ltm

#write.csv(Decision.table.A.ltm, paste0("Y:/Inshore/SFA29/2024/Assessment/Data/Model/SFA29A/DecisionTables_if_Condtion_drops_to_LTM/SFA29.A.mod.Decision.table.2023_ifconditionltm.csv"))

decision.tabs.A <- rbind(Decision.table.A.current, Decision.table.A.ltm)

###########################################
#Subarea B - For the Condition unchanged from current year (commercial growth rate is from the model object):

decision.current.B  <- predict(B.mod.res, exploit=0.1, g.parm=c(growth.paras.B$gh))
summary(decision.current.B)
set.seed(10) # set the random number generator if you want to make your results reproducable.
Decision.table.B <- matrix(NA,10,7)
dimnames(Decision.table.B)[[2]] <- names(summary(decision.current.B)$Next.year)
Decision.table.B
Decision.table.B <- NULL
for(i in 1:7){
  temp<-predict(B.mod.res,exploit=0.01*(i-1), g.parm=c(growth.paras.B$gh))
  # Get the Probability of being above the LRP, for Area B is this 1.12, for area C this is 1.41, and D is 1.30
  lrp <- signif(length(which(temp$Bh.next$High/temp$Area[3] >= 1.12))/length(temp$Bh.next$High),digits=2)
  usr <- signif(length(which(temp$Bh.next$High/temp$Area[3] >= 2.24))/length(temp$Bh.next$High),digits=2)
  #temp<-predict(B.2015,exploit=0.02*(i-1), g.parm=c(1.1612, 1.1578, 1.1796))
  Decision.table.B[[i]]<-as.vector(c(unlist(summary(temp)$Next.year),lrp, usr))
}
Dec.tab.B <- do.call("rbind",Decision.table.B)
colnames(Dec.tab.B) <- c(names(summary(temp)$Next.year),"Prob_above_LRP", "Prob_above_USR")

Dec.tab.B <- as.data.frame(Dec.tab.B) |> mutate(Table.Type = "Condition unchanged from current year") |> mutate(SUBAREA = "Subarea B") |> mutate(RRP=0.06)
Dec.tab.B

#B - For the Condition at LTM
decision.ltm.B  <- predict(B.mod.res, exploit=0.1, g.parm=c(growth.paras.B.at.ltm$gh))
summary(decision.ltm.B)
set.seed(10) # set the random number generator if you want to make your results reproducable.
Decision.table.B <- matrix(NA,20,7)
dimnames(Decision.table.B)[[2]] <- names(summary(decision.ltm.B)$Next.year)
Decision.table.B
Decision.table.B <- NULL
for(i in 1:16){
  temp<-predict(B.mod.res,exploit=0.005*(i-1), g.parm=c(growth.paras.B.at.ltm$gh))
  # Get the Probability of being above the LRP, for Area B is this 1.12, for area C this is 1.41, and D is 1.30
  lrp <- signif(length(which(temp$Bh.next$High/temp$Area[3] >= 1.12))/length(temp$Bh.next$High),digits=2)
  usr <- signif(length(which(temp$Bh.next$High/temp$Area[3] >= 2.24))/length(temp$Bh.next$High),digits=2)
  #temp<-predict(B.2015,exploit=0.02*(i-1), g.parm=c(1.1612, 1.1578, 1.1796))
  Decision.table.B[[i]]<-as.vector(c(unlist(summary(temp)$Next.year),lrp, usr))
}
Dec.tab.B.ltm <- do.call("rbind",Decision.table.B)
colnames(Dec.tab.B.ltm) <- c(names(summary(temp)$Next.year),"Prob_above_LRP", "Prob_above_USR")

Dec.tab.B.ltm <- as.data.frame(Dec.tab.B.ltm) |> mutate(Table.Type = "Condition at LTM") |> mutate(SUBAREA = "Subarea B") |> mutate(RRP=0.06)
Dec.tab.B.ltm

#write.csv(Decision.tab.B.ltm, paste0("Y:/Inshore/SFA29/2024/Assessment/Data/Model/SFA29B/DecisionTables_if_Condtion_drops_to_LTM/SFA29.B.mod.Decision.table.2023_ifconditionltm.csv"))

#Reduce number of rows for cleaner lines in plot, add last row in to match current scenario:
Dec.tab.B.ltm.thinned <- rbind(Dec.tab.B.ltm[c(TRUE, FALSE),], Dec.tab.B.ltm[16,])

decision.tabs.B <- rbind(Dec.tab.B, Dec.tab.B.ltm.thinned)

###########################################
#Subarea C - For the Condition unchanged from current year (commercial growth rate is from the model object):

decision.current.C  <- predict(C.mod.res, exploit=0.1, g.parm=c(growth.paras.C$gh))
summary(decision.current.C)
set.seed(10) # set the random number generator if you want to make your results reproducable.
Decision.table.C <- matrix(NA,16,7)
dimnames(Decision.table.C)[[2]] <- names(summary(decision.current.C)$Next.year)
Decision.table.C
Decision.table.C <- NULL
for(i in 1:16){
  temp<-predict(C.mod.res,exploit=0.01*(i-1), g.parm=c(growth.paras.C$gh))
  # Get the Probability of being above the LRP, for Area B is this 1.12, for area C this is 1.41, and D is 1.30
  lrp <- signif(length(which(temp$Bh.next$High/temp$Area[3] >= 1.41))/length(temp$Bh.next$High),digits=2)
  usr <- signif(length(which(temp$Bh.next$High/temp$Area[3] >= 2.82))/length(temp$Bh.next$High),digits=2)
  #temp<-predict(C.2015,exploit=0.02*(i-1), g.parm=c(1.1612, 1.1578, 1.1796))
  Decision.table.C[[i]]<-as.vector(c(unlist(summary(temp)$Next.year),lrp, usr))
}
Dec.tab.C <- do.call("rbind",Decision.table.C)
colnames(Dec.tab.C) <- c(names(summary(temp)$Next.year),"Prob_above_LRP", "Prob_above_USR")

Dec.tab.C <- as.data.frame(Dec.tab.C) |> mutate(Table.Type = "Condition unchanged from current year") |> mutate(SUBAREA = "Subarea C") |> mutate(RRP=0.17)
Dec.tab.C


#C - For the Condition at LTM
decision.ltm.C  <- predict(C.mod.res, exploit=0.1, g.parm=c(growth.paras.C.at.ltm$gh))
summary(decision.ltm.C)
set.seed(10) # set the random number generator if you want to make your results reproducable.
Decision.table.C <- matrix(NA,21,7)
dimnames(Decision.table.C)[[2]] <- names(summary(decision.ltm.C)$Next.year)
Decision.table.C
Decision.table.C <- NULL
for(i in 1:21){
  temp<-predict(C.mod.res,exploit=0.01*(i-1), g.parm=c(growth.paras.C.at.ltm$gh))
  # Get the Probability of being above the LRP, for Area B is this 1.12, for area C this is 1.41, and D is 1.30
  lrp <- signif(length(which(temp$Bh.next$High/temp$Area[3] >= 1.41))/length(temp$Bh.next$High),digits=2)
  usr <- signif(length(which(temp$Bh.next$High/temp$Area[3] >= 2.82))/length(temp$Bh.next$High),digits=2)
  Decision.table.C[[i]]<-as.vector(c(unlist(summary(temp)$Next.year),lrp, usr))
}
Dec.tab.C.ltm <- do.call("rbind",Decision.table.C)
colnames(Dec.tab.C.ltm) <- c(names(summary(temp)$Next.year),"Prob_above_LRP", "Prob_above_USR")

Dec.tab.C.ltm <- as.data.frame(Dec.tab.C.ltm) |> mutate(Table.Type = "Condition at LTM") |> mutate(SUBAREA = "Subarea C") |> mutate(RRP=0.17)
Dec.tab.C.ltm
#write.csv(Decision.tab.C.ltm, paste0("Y:/Inshore/SFA29/2024/Assessment/Data/Model/SFA29C/DecisionTables_if_Condtion_drops_to_LTM/SFA29.C.mod.Decision.table.2023_ifconditionltm.csv"))

decision.tabs.C <- rbind(Dec.tab.C, Dec.tab.C.ltm)
decision.tabs.C 

###########################################
#Subarea D - For the Condition unchanged from current year (commercial growth rate is from the model object):

decision.current.D  <- predict(D.mod.res, exploit=0.1, g.parm=c(growth.paras.D$gh))
summary(decision.current.D)
set.seed(10) # set the random number generator if you want to make your results reproducable.
Decision.table.D <- matrix(NA,10,7)
dimnames(Decision.table.D)[[2]] <- names(summary(decision.current.D)$Next.year)
Decision.table.D
Decision.table.D <- NULL
for(i in 1:21){
  temp<-predict(D.mod.res,exploit=0.01*(i-1), g.parm=c(growth.paras.D$gh))
  # Get the Probability of being above the LRP, for Area B is this 1.12, for area C this is 1.41, and D is 1.30
  lrp <- signif(length(which(temp$Bh.next$High/temp$Area[3] >= 1.3))/length(temp$Bh.next$High),digits=2)
  usr <- signif(length(which(temp$Bh.next$High/temp$Area[3] >= 2.6))/length(temp$Bh.next$High),digits=2)
  Decision.table.D[[i]]<-as.vector(c(unlist(summary(temp)$Next.year),lrp, usr))
}
Dec.tab.D <- do.call("rbind",Decision.table.D)
colnames(Dec.tab.D) <- c(names(summary(temp)$Next.year),"Prob_above_LRP", "Prob_above_USR")

Dec.tab.D <- as.data.frame(Dec.tab.D) |> mutate(Table.Type = "Condition unchanged from current year") |> mutate(SUBAREA = "Subarea D") |> mutate(RRP=0.22)
Dec.tab.D

#Reduce number of rows for cleaner lines in plot:
Dec.tab.D <- Dec.tab.D[c(TRUE, FALSE),]

#D - For the Condition at LTM
decision.ltm.D  <- predict(D.mod.res, exploit=0.1, g.parm=c(growth.paras.D.at.ltm$gh))
summary(decision.ltm.D)
set.seed(10) # set the random number generator if you want to make your results reproducable.
Decision.table.D <- matrix(NA,52,7)
dimnames(Decision.table.D)[[2]] <- names(summary(decision.ltm.D)$Next.year)
Decision.table.D
Decision.table.D <- NULL
for(i in 1:52){
  temp<-predict(D.mod.res,exploit=0.005*(i-1), g.parm=c(growth.paras.D.at.ltm$gh))
  # Get the Probability of being above the LRP, for Area B is this 1.12, for area C this is 1.41, and D is 1.30
  lrp <- signif(length(which(temp$Bh.next$High/temp$Area[3] >= 1.3))/length(temp$Bh.next$High),digits=2)
  usr <- signif(length(which(temp$Bh.next$High/temp$Area[3] >= 2.6))/length(temp$Bh.next$High),digits=2)
  Decision.table.D[[i]]<-as.vector(c(unlist(summary(temp)$Next.year),lrp, usr))
}
Dec.tab.D.ltm <- do.call("rbind",Decision.table.D)
colnames(Dec.tab.D.ltm) <- c(names(summary(temp)$Next.year),"Prob_above_LRP", "Prob_above_USR")

Dec.tab.D.ltm <- as.data.frame(Dec.tab.D.ltm) |> mutate(Table.Type = "Condition at LTM") |> mutate(SUBAREA = "Subarea D") |> mutate(RRP=0.22)
Dec.tab.D.ltm

#write.csv(Dec.tab.D.ltm, paste0("Y:/Inshore/SFA29/2024/Assessment/Data/Model/SFA29D/DecisionTables_if_Condtion_drops_to_LTM/SFA29.D.mod.Decision.table.2023_ifconditionltm.csv"))

#Reduce number of rows for cleaner lines in plot:
Dec.tab.D.ltm.thinned <- Dec.tab.D.ltm[c(TRUE, FALSE),]
Dec.tab.D.ltm.thinned <- rbind(Dec.tab.D.ltm.thinned[1:25,], Dec.tab.D.ltm[52,])

decision.tabs.D <- rbind(Dec.tab.D, Dec.tab.D.ltm.thinned)



#Combine all tables into one dataframe for plotting
decision.tabs.all <- rbind(decision.tabs.A |> dplyr::rename(Exploit = Exploit.Medium) |> dplyr::rename(Density = Density.Medium) |> dplyr::rename(B.change = B.change.Medium) |> dplyr::rename(Prob.B = Prob.B.Medium) |> mutate(Prob_above_LRP = NA) |>  mutate(Prob_above_USR = NA),
                           decision.tabs.B |> dplyr::rename(Exploit = Exploit.High) |> dplyr::rename(Density = Density.High) |> dplyr::rename(B.change = B.change.High) |> dplyr::rename(Prob.B = Prob.B.High),
                           decision.tabs.C |> dplyr::rename(Exploit = Exploit.High) |> dplyr::rename(Density = Density.High) |> dplyr::rename(B.change = B.change.High) |> dplyr::rename(Prob.B = Prob.B.High),
                           decision.tabs.D |> dplyr::rename(Exploit = Exploit.High) |> dplyr::rename(Density = Density.High) |> dplyr::rename(B.change = B.change.High) |> dplyr::rename(Prob.B = Prob.B.High))


#PLOT Catch and Exploitation:

p <- ggplot() +
  geom_line(data = decision.tabs.all, aes(Catch.All, Exploit, colour = Table.Type, linetype = Table.Type))+
  scale_colour_manual(values = c("firebrick","darkblue"), name = "")+
  scale_linetype_manual(values = c("dotted","solid"), name = "")+
  scale_y_continuous(breaks = seq(0, 0.27, 0.05),limits = c(0, 0.27))+
  xlab("Catch (t)")+ ylab("Exploitation") +
  theme_bw()+
  theme(legend.position = c(.82,.10), legend.text = element_text(size = 12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),strip.text = element_text(size = 15))+ #legend position
  facet_wrap(~SUBAREA,  nrow = 2, ncol=2, scales = "fixed")+
  geom_hline(data = decision.tabs.all, aes(yintercept = RRP), linetype = "dashed")
p

#Export plot 
ggsave(filename = paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/Catchandexp_comparisons_btwn_current_and_condatLTM.png"), plot = p, scale = 2.5, width = 12, height = 9, dpi = 300, units = "cm", limitsize = TRUE)

#FRENCH: PLOT Catch and Exploitation:

decision.tabs.all.FR <- decision.tabs.all |> 
  mutate(SUBAREA_FR = case_when(SUBAREA == "Subarea A" ~ "Sous-zone A",
                                SUBAREA == "Subarea B" ~ "Sous-zone B",
                                SUBAREA == "Subarea C" ~ "Sous-zone C",
                                SUBAREA == "Subarea D" ~ "Sous-zone D")) |> 
  mutate(Table.Type = case_when(Table.Type == "Condition at LTM" ~ "Condition égal à la médiane du long terme",
                                Table.Type == "Condition unchanged from current year" ~  "Condition identique à l'année courante"))


p.FR <- ggplot() +
  geom_line(data = decision.tabs.all.FR, aes(Catch.All, Exploit, colour = Table.Type, linetype = Table.Type))+
  scale_colour_manual(values = c("firebrick","darkblue"), name = "")+
  scale_linetype_manual(values = c("dotted","solid"), name = "")+
  scale_y_continuous(breaks = seq(0, 0.27, 0.05),limits = c(0, 0.27))+
  xlab("Prises (t)")+ ylab("Exploitation") +
  theme_bw()+
  theme(legend.position = c(.835,.10), legend.text = element_text(size = 12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),strip.text = element_text(size = 15))+ #legend position
  facet_wrap(~SUBAREA_FR,  nrow = 2, ncol=2, scales = "fixed")+
  geom_hline(data = decision.tabs.all.FR, aes(yintercept = RRP), linetype = "dashed")
p.FR

#Export plot 
ggsave(filename = paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/Catchandexp_comparisons_btwn_current_and_condatLTM_FR.png"), plot = p.FR, scale = 2.5, width = 12, height = 9, dpi = 300, units = "cm", limitsize = TRUE)

